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

(define-module (laco module)
  #:use-module (laco utils)
  #:use-module (laco env)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (mod
            mod?
            mod-filename
            mod-path
            mod-exprs
            mod-env

            mod-is-empty?
            read-as-mod))

;; TODO: Support r7rs modules
;; TODO: Replace top-level with mod-env
(define-typed-record mod
  (fields
   (filename string?)
   (path list?)
   (exprs list?)
   (env env?)))

(define (mod-is-empty? mod)
  (equal? '(begin) (mod-exprs mod)))

;; If mod-path is #f, then it's the main script
(define* (read-as-mod filename #:optional (mod-path '()))
  (define (read-all-exprs)
    (define port (open-file filename "r"))
    (let lp ((ret '()))
      (let ((e (read port)))
        (cond
         ((eof-object? e)
          (close port)
          ;; skip <eof>
          `(begin ,@(reverse! ret)))
         (else (lp (cons e ret)))))))
  (make-mod filename mod-path (read-all-exprs) (new-env)))
