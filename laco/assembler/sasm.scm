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

(define-module (laco assembler sasm)
  #:use-module ((rnrs) #:select (make-bytevector
                                 bytevector-u8-set!
                                 bytevector-u32-set!
                                 bytevector-length))
  #:use-module (laco assembler encode)
  #:use-module (laco utils)
  #:export (main-entry
            call-proc
            fjump
            jump))

(define *label-table* (make-hash-table))
(define (label-register! name)
  ;;(format #t "label ~a: ~a~%" name (label-counter 0))
  (hash-set! *label-table* name (label-counter 0)))
(define-syntax-rule (label-ref name)
  (hash-ref *label-table* name))

(define (vm-stack-pop)
  ;; (format #t "vm-stack-pop: 1~%")
  (primitive-encode/basic 1))

(define (main-entry)
  (let ((bv (make-bytevector 4 0))
        (main (label-ref '____principio)))
    (when (not main)
      (throw 'laco-error main-entry "BUG: main entry is missing!"))
    (bytevector-u32-set! bv 0 main 'big)
    bv))

(define-public (label name)
  (label-register! name)
  ;;(label-in! name)
  (when (not (is-pure-label? name))
    ;;(is-normal-call? name)
    ;;(pk "not pure lable" name) (read)
    (label-in! name))
  #vu8())

(define-public (label-end name)
  (when (not (is-pure-label? name))
    ;;(is-normal-call? name)
    (label-out!))
  #vu8())

;; NOTE: These 2 kind of labels offset can only be confirmed when it ends.
;; --------------------------------------------------
(define-public (closure-end label)
  (label-register! label)
  #vu8())

(define-public (branch-end label)
  (label-register! label)
  #vu8())
;; --------------------------------------------------


;; ------- single encoding -----------------
(define-public (local i)
  ;; (format #t "local: 1~%")
  (cond
   ((and (>= i 0) (< i 16))
    (single-encode 0 i))
   ((and (>= i 16) (< i 32))
    (single-encode 1 i))
   (else (throw 'laco-error local "Invalid offset `~a'!" i))))

(define-public (call-local name i keep?)
  (define (gen)
    ;; (format #t "call-local: 1~%")
    (cond
     ((and (>= i 0) (< i 16))
      (single-encode #b0100 i))
     ((and (>= i 16) (< i 32))
      (single-encode #b0101 (- i 16)))
     (else (throw 'laco-error call-local "Invalid offset `~a'!" i))))
  (cond
   (keep? (gen))
   (else
    (list (gen)
          (vm-stack-pop)))))

;; --------- special double encoding ----------
;; 0010 xxxx xxaaaaaa    Ref free up(fp)^a in offset x
(define-public (free label i)
  ;; (format #t "free: 2~%")
  (let ((frame (make-bytevector 1 0))
        (f (label-back-index label)))
    (when (and (< i 0) (>= i 64))
      (throw 'laco-error free "Invalid free offset `~a'" i))
    (when (and (< f 0) (>= i 64))
      (throw 'laco-error free "Invalid free frame back index `~a'" f))
    (bytevector-u8-set! frame 0 (logior (ash (logand i #b11) 6) f))
    (label-counter 1) ; count for frame
    (list
     (single-encode #b0010 (ash (logand i #b111100) -2))
     frame)))

(define-public (call-free label i keep?)
  (define (gen)
    ;; (format #t "call-free: 2~%")
    (let ((frame (make-bytevector 1 0))
          (f (label-back-index label)))
      ;;(pk "call-free" f) (read)
      (when (and (< i 0) (>= i 64))
        (throw 'laco-error free "Invalid free offset `~a'" i))
      (when (and (< f 0) (>= i 64))
        (throw 'laco-error free "Invalid free frame back index `~a'" f))
      (bytevector-u8-set! frame 0 (logior (ash (logand i #b11) 6) f))
      (label-counter 1) ; count for frame
      (list
       (single-encode #b0011 (ash (logand i #b111100) -2))
       frame)))
  (cond
   (keep? (gen))
   (else
    (list (gen)
          (vm-stack-pop)))))

;; --------- double encoding -----------
(define-public (prelude label mode-name arity)
  ;; (format #t "prelude: 2~%")
  (let ((b (logior (ash arity 2) (name->mode mode-name))))
    (double-encode #b0000 b)))

;; --------- triple encoding -----------
(define-public (vec-ref offset i)
  ;; (format #t "vec-ref: ~a~%" 0)
  (throw 'laco-error vec-ref "Haven't implemented yet!"))

(define* (call-proc label keep? #:optional (count? #t))
  (let ((offset (label-ref label)))
    (cond
     (offset
      (triple-encode #b0000
                     (ash (logand offset #xff00) -8)
                     (logand offset #xff)
                     count?))
     (else
      ;; (format #t "call-proc: ~a 3~%" label)
      (label-counter 3)
      (let ((bv `#((call-proc ,label ,keep? #f))))
        ;; NOTE: We have to put keep? clause here to avoid twice eval of
        ;;       (vm-stack-pop), otherwise the LEF body will move forward 1 byte
        ;;       offset.
        (if keep?
            bv
            (list bv (vm-stack-pop))))))))

(define* (fjump label #:optional (count? #t))
  (let ((offset (label-ref label)))
    (cond
     (offset
      (triple-encode #b0001
                     (ash (logand offset #xff00) -8)
                     (logand offset #xff)
                     count?))
     (else
      ;; (format #t "fjump: 3~%")
      (label-counter 3)
      `#((fjump ,label #f))))))

(define* (jump label #:optional (count? #t))
  (let ((offset (label-ref label)))
    (cond
     (offset
      (triple-encode #b0010
                     (ash (logand offset #xff00) -8)
                     (logand offset #xff)
                     count?))
     (else
      ;; (format #t "jump: 3~%")
      (label-counter 3)
      `#((jump ,label #f))))))

;; --------- quadruple encode -----------
(define-public (vec-set! offset i v)
  ;; (format #t "vec-set!: ~a~%" 0)
  (throw 'laco-error vec-set! "Haven't implemented yet!"))

(define* (closure-on-heap arity frame-size entry #:optional (count? #t))
  (quadruple-encode #b0001
                    (logior (ash arity 4) frame-size)
                    (ash (logand entry #xff00) -8)
                    (logand entry #xff)
                    count?))

(define* (closure-on-stack arity frame-size entry #:optional (count? #t))
  (quadruple-encode #b0010
                    (logior (ash arity 4) frame-size)
                    (ash (logand entry #xff00) -8)
                    (logand entry #xff)
                    count?))

(define* (closure mode arity frame-size entry-label #:optional (count? #t))
  (let ((entry (label-ref entry-label)))
    (cond
     (entry
      (closure-on-heap arity frame-size entry count?)
      #;
      (case mode                        ;
      ((stack) (closure-on-stack arity frame-size entry count?)) ;
      ((heap) (closure-on-heap arity frame-size entry count?)) ;
      (else (throw 'laco-error closure "Invalid mode `~a'!" mode))))
     (else
      ;; (format #t "closure: 4~%")
      (label-counter 4)
      `#((closure ,mode ,arity ,frame-size ,entry-label #f))))))

;; --------- special encode -----------
;; TODO: detect if it's primitive/extend
(define-public (prim-call pn keep?)
  ;; (format #t "prim-call: ~a~%" (if (> pn 15) 2 1))
  (let ((prim (if (> pn 15) primitive-encode/extend primitive-encode/basic)))
    (cond
     (keep?
      (prim pn))
     (else
      (list (prim pn)
            (vm-stack-pop))))))

(define-public (special-encode i)
  ;; (format #t "special-encode: ~a~%" 2)
  (primitive-encode/extend i))

(define-public (halt)
  ;; (format #t "halt: ~a~%" 1)
  #vu8(255))

;; ----------- object creation -----------
(define-public (push-integer-object i)
  ;; (format #t "push-integer-object: ~a~%" 6)
  (integer-obj-encode i))

(define-public (push-real-object r)
  ;; (format #t "push-real-object: ~a~%" 6)
  (real-obj-encode r))

(define-public (push-rational-object r)
  ;; (format #t "push-rational-object: ~a~%" 6)
  (rational-obj-encode r))

(define-public (push-complex-object i)
  ;; (format #t "push-complex-object: 6 or 10~%")
  (complex-obj-encode i))

(define-public (push-string-object s)
  (string-obj-encode s))

(define-public (push-keyword-object s)
  (keyword-obj-encode s))

(define-public (push-char-object c)
  (when (not (char? c))
    (throw 'laco-error push-char-object "Invalid char `~a'!" c))
  (char-obj-encode (char->integer c)))

(define-public (push-list-object size)
  (collection-obj-encode 7 size))

(define-public (push-vector-object size)
  (collection-obj-encode 5 size))

(define-public (push-bytevector-object bv)
  (bytevector-obj-encode bv))

;; TODO:
;; 1. add opt-index
;; 2. detect opt-index and convert vargs to list in vm.c
;; 3. patch local-call and free-call
(define* (push-proc-object entry arity opt-index #:optional (count? #t))
  (let ((offset (label-ref entry))
        ;; If there's no opt-index, then we set it to the last index of args
        (opt (or opt-index arity)))
    (cond
     (offset (proc-obj-encode offset arity opt count?))
     (else
      ;; (format #t "push-proc-object: ~a~%" 6)
      (label-counter 6)
      `#((push-proc-object ,entry #f))))))

(define-public (push-prim-object pn)
  ;; (format #t "push-prim-object: ~a~%" 6)
  (prim-obj-encode pn))

(define-public (push-boolean-false)
  ;; (format #t "push-boolean-false: ~a~%" 1)
  (boolean-obj-encode 0))

(define-public (push-pair-object)
  ;; (format #t "push-pair-object: ~a~%" 6)
  (pair-obj-encode))

(define-public (push-boolean-true)
  ;; (format #t "push-boolean-true: ~a~%" 1)
  (boolean-obj-encode 1))

(define-public (push-symbol-object s)
  ;; (format #t "push-symbol-object: ~a~%" 0)
  (let ((offset (intern-offset s)))
    (symbol-encode offset)))

;; --------- assignment ------------
(define-public (local-assign offset)
  ;; (format #t "local-assign: ~a~%" 1)
  (let ((frame (make-bytevector 1 0)))
    (when (and (< offset 0) (>= offset 160))
      (throw 'laco-error local-assign "Invalid local offset `~a'"
             offset))
    (bytevector-u8-set! frame 0 (logand offset #xFF))
    (label-counter 1)
    (list
     (single-encode #b0111 (ash (logand offset #b111100000000) -8))
     frame)))

(define-public (free-assign label offset)
  ;; (format #t "free-assign: ~a~%" 1)
  (let ((frame (make-bytevector 1 0))
        (f (label-back-index label)))
    (when (and (< offset 0) (>= offset 64))
      (throw 'laco-error free-assign "Invalid free offset `~a'"
             offset))
    (when (and (< f 0) (>= f 64))
      (throw 'laco-error free-assign
             "Invalid free frame back index `~a'" f))
    (bytevector-u8-set! frame 0
                        (logior (ash (logand offset #b11) 6) f))
    (label-counter 1)
    (list
     (single-encode #b0110 (ash (logand offset #b11111100) -2))
     frame)))

(define-public (global-assign name)
  ;; (format #t "global-assign: ~a~%" 0)
  (let ((i (global-index name)))
    (cond
     ((and (>= i 0) (< i 256))
      (double-encode #b0011 i))
     ((and (>= i 256) (< i 65536))
      (let ((ii (- i 256)))
        (triple-encode #b0100 (ash (logand #xff00 ii) -8) (logand #xff ii))))
     (else (throw 'laco-error global-assign "Invalid offset `~a'!" i)))))

(define-public (global name)
  ;; (format #t "global: ~a~%" 0)
  (let ((i (global-index name)))
    (cond
     ((and (>= i 0) (< i 256))
      (double-encode #b0100 i))
     ((and (>= i 256) (< i 65536))
      (let ((ii (- i 256)))
        (triple-encode #b0101 (ash (logand #xff00 ii) -8) (logand #xff ii))))
     (else (throw 'laco-error global "Invalid offset `~a'!" i)))))

(define-public (global-call name)
  ;; (format #t "global-call: ~a~%" 0)
  (let ((i (global-index name)))
    (cond
     ((and (>= i 0) (< i 256))
      (double-encode #b0101 i))
     ((and (>= i 256) (< i 65536))
      (let ((ii (- i 256)))
        (triple-encode #b0110 (ash (logand #xff00 ii) -8) (logand #xff ii))))
     (else (throw 'laco-error global-call "Invalid offset `~a'!" i)))))
