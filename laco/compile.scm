;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020
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
  #:use-module (laco lir)
  #:use-module (laco types)
  #:use-module (laco codegen)
  #:use-module (laco assembler)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (laco-compile))

(define option-spec
  '((help (value #f))
    (output (single-char #\o) (value #t))
    (output-type (single-char #\t) (value #t))
    (version (single-char #\v) (value #f))))

(define hidden-option-spec
  '((options-list (value #f))))

(define (option-spec-str)
  (fold (lambda (value acc) (string-append acc " --" (symbol->string (car value))))
        (string-append "--" (symbol->string (caar option-spec)))
        (cdr option-spec)))

(define (display-it x)
  (format #t "~a\n" x))

(define (init-optimizations)
  (process-use-modules
   (map (lambda (s) `((laco pass ,(string->symbol (file-basename s)))))
        (scandir (string-append (dirname (current-filename)) "/pass")
                 (lambda (s) (string-match "\\.scm" s))))))

(define announce-head
  "
Laco is a functional programming language compiler for embedded system.
Author: NalaGinrut <mulei@gnu.org>
")

(define announce-foot
  (format #f "~%~a~%Version: ~a.~%God bless hacking.~%~%" "GPLv3+"
          "0.0.1"))

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
     dead-function-elimination
     fold-constant
     (constant-propagation 2)
     useless-cont
     fold-branch
     function-inline
     dead-variable-elimination
     elre
     eta-cont
     eta-function
     delta-reduction
     closure-conversion
     lambda-lifting))
  (init-optimizations)
  (parameterize ((current-kont 'global))
    ;; Prevent unecessary lifting and inline for global functions
    (top-level-for-each (lambda (_ e) (do-optimize e))))
  (do-optimize cexpr))

(define output-file (make-parameter #f))
(define output-type (make-parameter #f))

;; (type, generator, printer)
(define *stages*
  `((ast ,parse-module ,ast->src)
    (cps ,ast->cps ,cps->expr/g)
    (opt ,optimize ,cps->expr/g)
    (lir ,cps->lir/g ,lir->expr/g)
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
      (codegen outfile (fold (lambda (x p) (car x) ((cadr x) p)) mod *stages*))))

(define (do-compile filename)
  (define outfile (if (output-file) (output-file) (gen-outfile filename)))
  (when (not (file-exists? filename))
    (error "File doens't exist!" filename))
  (when (file-exists? outfile)
    (delete-file outfile))
  (let ((mod (read-as-mod filename)))
    (if (mod-is-empty? mod)
        (exit 0)
        (run-stages outfile mod))))

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
                     (output-type (->opt 'output-type)))
        (let ((filename (get-intput-name args)))
          (when filename (do-compile filename))))))))
