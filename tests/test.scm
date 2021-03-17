(define-module (tests)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs)
  #:use-module (laco compile))

(define (gen-lef filename)
  (string-append "/dev/shm/"
                 (substring/shared filename 0 (string-index filename #\.))))

(define (lambdachip-run filename)
  (let ((tf (format #f "tests/scm/~a" filename)))
    (when (not (file-exists? tf))
      (format #t "The test file `~a' is missing!" tf)
      (exit -1))
    (let ((vm (getenv "LAMBDACHIP_VM_PATH"))
          (lef (gen-lef filename)))
      (when (not vm)
        (format #t "Please set LAMBDACHIP_VM_PATH then try again!")
        (exit -2))
      (system (format #f "./pre-inst-env scripts/laco ~a -o ~a >/dev/null" tf lef))
      (system (format #f "~a/lambdachip-vm ~a > /dev/shm/mmr.log" vm lef))
      (call-with-input-file "/dev/shm/mmr.log" get-string-all))))

(test-begin "test-suite")

(test-equal "Fibonacci"
  "120"
  (lambdachip-run "fibonacci.scm"))

(test-equal "Nested calling"
  "2049"
  (lambdachip-run "nested-call.scm"))

(test-end "test-suite")
