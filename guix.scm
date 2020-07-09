(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(package
  (name "laco")
  (version "0.0.1")
  (source "./laco-0.0.1.tar.gz")
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs `())
  (synopsis "Lambda Compiler")
  (description
    "Laco is a r7rs Scheme compiler for LambdaChip VM which is designed for functional programming on embedded system.")
  (home-page "https://lambdachip.com")
  (license license:gpl3+))

