;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020-2023
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Laco is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published
;;  by the Free Software Foundation, either version 3 of the License,
;;  or (at your option) any later version.

;;  Laco is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-module (laco compile)
  #:use-module (laco utils)
  #:use-module (laco module)
  #:use-module (laco env)
  #:use-module (laco parser)
  #:use-module (laco pass)
  #:use-module (laco cps)
  #:use-module (laco ast)
  #:use-module (laco lir)
  #:use-module (laco types)
  #:use-module (laco codegen)
  #:use-module (laco assembler)
  #:use-module (laco openai)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module ((rnrs) #:select (get-string-all))
  #:export (laco-compile))

(define option-spec
  '((help (value #f))
    (output (single-char #\o) (value #t))
    (output-type (single-char #\t) (value #t))
    (version (single-char #\v) (value #f))
    (verbose (value #f))))

(define hidden-option-spec
  '((options-list (value #f))))

(define (option-spec-str)
  (fold (lambda (value acc) (string-append acc " --" (symbol->string (car value))))
        (string-append "--" (symbol->string (caar option-spec)))
        (cdr option-spec)))

(define (display-it x)
  (format #t "~a\n" x))

;; If Laco was installed, then use %site-dir
;; If not installed, try (car %load-path)
(define (detect-site-path)
  (define (check p) (file-exists? (format #f "~a/laco" p)))
  (cond
   ((check (car %load-path))
    (car %load-path))
   ((check (%site-dir))
    (%site-dir))
   (else
    (throw 'laco-error
           detect-site-path
           "Can't find Laco modules, please install Laco or use pre-inst-env!"))))

(define (init-optimizations)
  (process-use-modules
   (map (lambda (s) `((laco pass ,(string->symbol (file-basename s)))))
        (scandir (format #f "~a/laco/pass" (detect-site-path))
                 (lambda (s) (string-match "\\.scm" s))))))

(define (init-lir-optimizations)
  (process-use-modules
   (map (lambda (s) `((laco lpass ,(string->symbol (file-basename s)))))
        (scandir (format #f "~a/laco/lpass" (detect-site-path))
                 (lambda (s) (string-match "\\.scm" s))))))

(define announce-head
  "
Laco is a functional programming language compiler for embedded system.
Author: NalaGinrut <mulei@gnu.org>
")

(define announce-foot
  (format #f "~%~a~%Version: ~a.~%God bless hacking.~%~%" "GPLv3+"
          "0.5.0"))

(define help-str
  "
Usage:
  laco [options] filename

Options:
  -o, [--output=file]            # Specify output filename
                                   Default: <input-filename>.lef
  -t, [--output-type=type]       # Output specified stage result
                                   Values: ast, cps, lir, opt, sasm
  -v, [--version]                # Show current version

  --help                         # Show this screen
")

(define (show-help)
  (display announce-head)
  (display help-str)
  (display announce-foot))

(define (optimize cexpr)
  (define (do-optimize cexpr)
    (run-pass
     cexpr
     normalize
     useless-constant
     effect-analysis
     dead-function-elimination
     fold-constant
     (constant-propagation 2)
     useless-cont
     function-inline
     dead-variable-elimination
     eta-cont
     eta-function
     delta-reduction
     fold-branch
     useless-constant
     tail-call-optimizing
     escape-analysis
     primitive-conversion
     args-extend
     closure-lifting
     closure-conversion
     eliminate-redundant
     ;; NOTE: lambda-lifting must be at the end, otherwise the lifted results will
     ;;       miss all the rest passes.
     lambda-lifting))
  (init-optimizations)
  (parameterize ((current-kont 'global)
                 (is-top? #t))
    (top-level-for-each
     ;; NOTE: We scan all free-vars in globals before any optimizing to make sure
     ;;       no any free-var will be eliminated in dead-function-elimination.
     (lambda (_ e)
       (for-each set-fv-in-globals! (map id-name (free-vars e)))))
    (top-level-for-each
     (lambda (f e)
       (parameterize ((current-def f))
         ;; Prevent unecessary lifting and inline for global functions
         (top-level-set! f (do-optimize e))))))
  (do-optimize cexpr))

(define (lir-optimize lexpr)
  (define (do-optimize lexpr)
    (run-pass
     lexpr
     free-vars-lifting
     remove-unused-captures
     closure-capture-free-vars))
  (init-lir-optimizations)
  (parameterize ((current-kont 'global)
                 (is-top? #t))
    (top-level-for-each
     (lambda (f e)
       (parameterize ((current-def f))
         ;; Prevent unecessary lifting and inline for global functions
         (top-level-set! f (do-optimize e))))))
  (do-optimize lexpr))

(define output-file (make-parameter #f))
(define output-type (make-parameter #f))
(define need-verbose? (make-parameter #f))

;; (type, generator, printer)
(define *stages*
  `((ast ,parse-module ,ast->src)
    (cps ,ast->cps ,cps->expr/g)
    (opt ,optimize ,cps->expr/g)
    (lir ,cps->lir/g ,lir->expr/g)
    (lopt ,lir-optimize ,lir->expr/g)
    (sasm ,lir->sasm ,lir->sasm-string)))

(define (run-till-stage exprs t)
  (when (not (assoc-ref *stages* t))
    (format (current-error-port)
            "Invalid type `~a'! The valid types are:~%" t)
    (for-each
     (lambda (s) (format (current-error-port) "~a~%" (car s)))
     *stages*)
    (exit -1))
  (let lp ((next *stages*) (stop? #f) (ret exprs))
    (cond
     ((or (null? next) stop?)
      (if (string? ret)
          (display ret)
          (pretty-print ret)))
     (else
      (match (car next)
        ((type generator printer)
         (let* ((is-stop? (eq? t type))
                (return (if is-stop? printer identity))
                (result (if (and is-stop? (eq? type 'sasm))
                            (return ret)
                            (return (generator ret)))))
           (lp (cdr next) is-stop? result)))
        (else (throw 'laco-error run-till-stage "BUG: Invalid stage item `~a'!"
                     next)))))))

(define (run-stages outfile mod)
  (if (output-type)
      (run-till-stage mod (string->symbol (output-type)))
      (codegen
       outfile
       (fold (lambda (x p)
               (let ((ret ((cadr x) p)))
                 (when (need-verbose?)
                   (format #t "======= ~a =======~%" (car x))
                   (if (eq? (car x) 'sasm)
                       (pretty-print ret)
                       (pretty-print ((caddr x) ret))))
                 ret))
             mod *stages*))))

(define (do-compile filename)
  (define outfile (if (output-file) (output-file) (gen-outfile filename)))
  (when (not (file-exists? filename))
    (error "File doens't exist!" filename))
  (when (file-exists? outfile)
    (delete-file outfile))
  (let ((mod (read-as-mod filename)))
    (cond
     ((mod-is-empty? mod)
      ;; TODO: Throw laco-warning here
      (format (current-error-port) "WARNING: ~s is empty file!~%" filename)
      (exit 0))
     (else
      (catch 'laco-error
        (lambda ()
          (run-stages outfile mod))
        (lambda e
          (cond
           ((getenv "LACO_OPENAI_KEY")
            => (lambda (key)
                 (format (current-warning-port)
                         "Detected OpenAI Key, enable AI checker!~%")
                 (format (current-warning-port)
                         "(Try to set https_proxy if you're behind the firewall)~%")
                 (display "\n\n\n--------------Waiting for AI hint-------------\n")
                 (let ((src (call-with-input-file filename get-string-all)))
                   (ai-check src key))
                 (exit -1)))
           (else (apply throw e)))))))))

(define (laco-compile args)
  (let ((options (if (null? args)
                     '()
                     (getopt-long args (append option-spec hidden-option-spec)))))
    (define-syntax-rule (->opt k) (option-ref options k #f))
    (define-syntax-rule (get-intput-name args)
      (match (->opt '())
        ((filename) filename)
        (else
         (show-help)
         #f)))
    (cond
     ((->opt 'help) (show-help))
     ((->opt 'options-list) (display-it (option-spec-str)))
     (else
      (parameterize ((output-file (->opt 'output))
                     (output-type (->opt 'output-type))
                     (need-verbose? (->opt 'verbose)))
        (let ((filename (get-intput-name args)))
          (when filename (do-compile filename))))))))
