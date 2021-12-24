;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020-2021
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

(define-module (laco module)
  #:use-module (laco utils)
  #:use-module (laco env)
  #:use-module (laco records)
  #:export (mod
            mod?
            mod-filename
            mod-path
            mod-exprs
            mod-env

            mod-is-empty?
            read-as-mod))

;; NOTE: We call the compilation-unit a `module'.

;; TODO: Support r7rs modules
;; TODO: Replace top-level with mod-env
;; TODO: Distinct `library' and `program'

(define-typed-record mod
  (fields
   (filename string?)
   (path list?)
   (exprs list?)
   (env env?)))

(define (mod-is-empty? mod)
  (equal? '(begin) (mod-exprs mod)))

;; FIXME: We put the default module contents in this naive way. We will handle them
;;       well when we implement module import/export.
(define defaults
  '(
    (define (ble-reset!)
      ;; cut down power of BLE module for 50ms
      (gpio-set! 'dev_gpio_ble_disable #t)
      (usleep 50000)
      (gpio-set! 'dev_gpio_ble_disable #f))
    (define (ble-enable!)
      ;; FR8016 firmware need to have 87.5ms to 93.75ms before receive AT command
      (usleep 100000)
      (display "\nAT+AUTO+++=Y\n"))

    ;; FIXME: It should be (define* (newline #:optional port) ...). We'll fix it
    ;;        when we have define*.
    (define (newline)
      (display "\n"))

    (define* (make-bytevector k #:optional (byte 0))
      (%make-bytevector k byte))

    (define* (bytevector-copy bv #:optional (start 0) (end (bytevector-length bv)))
      (%bytevector-copy bv start end))

    (define* (bytevector-copy! to at from #:optional (start 0) (end (bytevector-length from)))
      (%bytevector-copy! to at from start end))

    (define* (make-string k #:optional (char #\a))
      (%make-string k char))

    (define* (string-copy str0 #:optional (start 0) (end (string-length str0)))
      (%string-copy str0 start end))

    (define* (string-append string1 string2)
      (%string-append string1 string2))

    (define (pk str obj)
      (display ";;; (")
      (display str)
      (display ") ")
      (display obj)
      (newline)
      obj)
    ))

;; If mod-path is #f, then it's the main script
(define* (read-as-mod filename #:optional (mod-path '()))
  (define (read-all-exprs)
    (define port (open-file filename "r"))
    (let lp ((ret '()))
      (let ((e (read port)))
        (cond
         ((eof-object? e)
          (close port)
          ;; TOD: Change `begin' to `module'
          ;; skip <eof>
          `(begin ,@defaults ,@(reverse! ret)))
         (else (lp (cons e ret)))))))
  (make-mod filename mod-path (read-all-exprs) (new-env (string->symbol filename))))
