(define-module (tests)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs)
  #:use-module (laco compile))

(define (lambdachip-run name)
  (let ((tf (format #f "tests/scm/~a.scm" name)))
    (when (not (file-exists? tf))
      (format #t "The test file `~a' is missing!" tf)
      (exit -1))
    (let ((vm (getenv "LAMBDACHIP_VM_PATH"))
          (lef (format #f "/tmp/~a.lef" name)))
      (when (not vm)
        (format #t "Please set LAMBDACHIP_VM_PATH then try again!")
        (exit -2))
      (system (format #f "./pre-inst-env scripts/laco ~a -o ~a >/dev/null" tf lef))
      (system (format #f "~a/lambdachip-vm ~a > /tmp/~a.log" vm lef name))
      (call-with-input-file (format #f "/tmp/~a.log" name) get-string-all))))

(define (get-result name)
  (call-with-input-file (format #f "tests/result/~a.txt" name) get-string-all))

(define (check case name)
  (test-equal case
    (get-result name)
    (lambdachip-run name)))

(test-begin "test-suite")

(check "Fibonacci" "fibonacci")
(check "Nested calling" "nested-call")

(test-end "test-suite")
