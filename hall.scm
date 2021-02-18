(hall-description
  (name "laco")
  (prefix "")
  (version "0.0.1")
  (author "Mu Lei known as NalaGinrut")
  (copyright (2021))
  (synopsis "Lambda Compiler")
  (description
    "Laco is a r7rs Scheme compiler for LambdaChip VM which is designed for functional programming on embedded system.")
  (home-page "https://lambdachip.com")
  (license gpl3+)
  (dependencies `(("guile" ,guile-3.0)))
  (files (libraries
           ((directory
              "laco"
              ((directory
                 "assembler"
                 ((scheme-file "sasm") (scheme-file "encode")))
               (directory
                 "lpass"
                 ((scheme-file "remove-unused-captures")
                  (scheme-file "reduce-labels")
                  (scheme-file "fv-lifting")
                  (scheme-file "closure-capture-fv")))
               (directory
                 "pass"
                 ((scheme-file "useless-cont")
                  (scheme-file "useless-constant")
                  (scheme-file "tco")
                  (scheme-file "primitive-conversion")
                  (scheme-file "normalize")
                  (scheme-file "lambda-lifting")
                  (scheme-file "func-inline")
                  (scheme-file "fold-const")
                  (scheme-file "fold-branch")
                  (scheme-file "eta-func")
                  (scheme-file "eta-cont")
                  (scheme-file "escape-analysis")
                  (scheme-file "elre")
                  (scheme-file "delta-reduction")
                  (scheme-file "dce")
                  (scheme-file "const-propagation")
                  (scheme-file "closure-conversion")
                  (scheme-file "args-extend")))
               (directory
                 "records"
                 ((scheme-file "syntactic")
                  (scheme-file "procedural")))
               (scheme-file "utils")
               (scheme-file "types")
               (scheme-file "sasm")
               (scheme-file "records")
               (scheme-file "primitives")
               (scheme-file "pass")
               (scheme-file "parser")
               (scheme-file "object")
               (scheme-file "module")
               (scheme-file "lir")
               (scheme-file "env")
               (scheme-file "cps")
               (scheme-file "compile")
               (scheme-file "codegen")
               (scheme-file "ast")
               (scheme-file "assembler")))))
         (tests ())
         (programs
           ((directory
              "scripts"
              ((in-file "laco") (text-file "laco")))))
         (documentation
           ((symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")))
         (infrastructure
           ((scheme-file "guix")
            (scheme-file "hall")
            (directory
              "build-aux"
              ((tex-file "texinfo")
               (scheme-file "test-driver")
               (text-file "missing")
               (text-file "mdate-sh")
               (text-file "install-sh")))
            (directory
              "build-aux"
              ((tex-file "texinfo")
               (scheme-file "test-driver")
               (text-file "missing")
               (text-file "mdate-sh")
               (text-file "install-sh")))
            (autoconf-file "configure")
            (automake-file "Makefile")
            (in-file "pre-inst-env")))))
