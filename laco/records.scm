;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2021
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


;; The Guile2 compatible record-type, it supports nested inheritance in
;; pattern matching. Altough it's slower than Guile3 record-type, it's convenient,
;; and it only affects a little with the compiling speed in Laco.

(define-module (laco records)
  #:use-module (laco records syntactic)
  #:use-module (laco records procedural)
  #:re-export (record-type-descriptor
               define-record-type
               record-constructor-descriptor
               make-record-type-descriptor
               record-type-descriptor?
               make-record-constructor-descriptor

               record-constructor
               record-predicate
               record-accessor
               record-mutator))
