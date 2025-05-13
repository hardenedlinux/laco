;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2025
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

(define-module (laco reserved)
  #:export (is-reserved-symbol?))

;; NOTE: we don't allow primitives-redefine, so this list is for checking.
(define *reserved*
  `(quote quasiquote unquote unquote-splicing lambda if set!
          cond and or case let let* letrec begin do define delay
          ,@(@@ (laco primitives) *prim-table*)))

(define (is-reserved-symbol? sym)
  (memq sym *reserved*))
