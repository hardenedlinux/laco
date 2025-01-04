bin_SCRIPTS = scripts/laco \
              scripts/laco

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES = laco/assembler/encode.go \
          laco/assembler/sasm.go \
          laco/assembler/sasm.scm \
          laco/assembler/encode.scm \
          laco/lpass/closure-capture-fv.go \
          laco/lpass/fv-lifting.go \
          laco/lpass/remove-unused-captures.go \
          laco/lpass/remove-unused-captures.scm \
          laco/lpass/fv-lifting.scm \
          laco/lpass/closure-capture-fv.scm \
          laco/pass/closure-conversion.go \
          laco/pass/normalize.go \
          laco/pass/elre.go \
          laco/pass/tco.go \
          laco/pass/lambda-lifting.go \
          laco/pass/func-inline.go \
          laco/pass/delta-reduction.go \
          laco/pass/const-propagation.go \
          laco/pass/eta-cont.go \
          laco/pass/fold-const.go \
          laco/pass/args-extend.go \
          laco/pass/fold-branch.go \
          laco/pass/escape-analysis.go \
          laco/pass/primitive-conversion.go \
          laco/pass/useless-constant.go \
          laco/pass/useless-cont.go \
          laco/pass/eta-func.go \
          laco/pass/dce.go \
          laco/pass/effect-analysis.go \
          laco/pass/useless-cont.scm \
          laco/pass/useless-constant.scm \
          laco/pass/tco.scm \
          laco/pass/primitive-conversion.scm \
          laco/pass/normalize.scm \
          laco/pass/lambda-lifting.scm \
          laco/pass/func-inline.scm \
          laco/pass/fold-const.scm \
          laco/pass/fold-branch.scm \
          laco/pass/eta-func.scm \
          laco/pass/eta-cont.scm \
          laco/pass/escape-analysis.scm \
          laco/pass/elre.scm \
          laco/pass/effect-analysis.scm \
          laco/pass/delta-reduction.scm \
          laco/pass/dce.scm \
          laco/pass/const-propagation.scm \
          laco/pass/closure-lifting.scm \
          laco/pass/closure-conversion.scm \
          laco/pass/args-extend.scm \
          laco/pass/closure-lifting.go \
          laco/records/procedural.go \
          laco/records/syntactic.go \
          laco/records/syntactic.scm \
          laco/records/procedural.scm \
          laco/types.scm \
          laco/pass.scm \
          laco/parser.scm \
          laco/openai.scm \
          laco/cps.scm \
          laco/compile.scm \
          laco/ast.scm \
          laco/assembler.scm \
          laco/openai.go \
          laco/cps.go \
          laco/lir.go \
          laco/primitives.go \
          laco/parser.go \
          laco/compile.go \
          laco/codegen.go \
          laco/object.go \
          laco/sasm.go \
          laco/utils.go \
          laco/ast.go \
          laco/pass.go \
          laco/env.go \
          laco/types.go \
          laco/module.go \
          laco/assembler.go \
          laco/records.go \
          laco/utils.scm \
          laco/sasm.scm \
          laco/records.scm \
          laco/primitives.scm \
          laco/object.scm \
          laco/module.scm \
          laco/lir.scm \
          laco/env.scm \
          laco/codegen.scm \
          laco.scm

TESTS = tests/result/var-list.txt \
        tests/result/symbol-list.txt \
        tests/result/simple-seq-print.txt \
        tests/result/redefine.txt \
        tests/result/rational-division.txt \
        tests/result/rational-1.txt \
        tests/result/raise.txt \
        tests/result/raise-cont.txt \
        tests/result/pair.txt \
        tests/result/or-test.txt \
        tests/result/numbers.txt \
        tests/result/null.txt \
        tests/result/nested-lambda-1.txt \
        tests/result/nested-call.txt \
        tests/result/named-let-narg-in-func.txt \
        tests/result/named-let-loop.txt \
        tests/result/named-let-loop-in-func.txt \
        tests/result/named-let-3.txt \
        tests/result/named-let-2.txt \
        tests/result/named-let-1.txt \
        tests/result/multiply.txt \
        tests/result/local-def-4.txt \
        tests/result/local-def-3.txt \
        tests/result/local-def-2.txt \
        tests/result/local-def-1.txt \
        tests/result/list_to_string.txt \
        tests/result/list-ref.txt \
        tests/result/list-print.txt \
        tests/result/list-car-cdr.txt \
        tests/result/list-append.txt \
        tests/result/let-star-side-effect-1.txt \
        tests/result/let-star-optimized-out.txt \
        tests/result/let-star-1.txt \
        tests/result/let-1.txt \
        tests/result/lambda-lifting-2.txt \
        tests/result/lambda-lifting-1.txt \
        tests/result/lambda-app-multi-args.txt \
        tests/result/keyword-print.txt \
        tests/result/int-list.txt \
        tests/result/global-list-1.txt \
        tests/result/global-expr-1.txt \
        tests/result/gcd.txt \
        tests/result/fold-branch-2.txt \
        tests/result/fold-branch-1.txt \
        tests/result/fibonacci.txt \
        tests/result/factorial.txt \
        tests/result/expr-in-mid-defs.txt \
        tests/result/eq_gt_lt.txt \
        tests/result/define-star-opts.txt \
        tests/result/define-star-keys.txt \
        tests/result/define-star-keys-opts.txt \
        tests/result/closure-2.txt \
        tests/result/closure-1.txt \
        tests/result/char-print.txt \
        tests/result/bytevector.txt \
        tests/result/branch-2.txt \
        tests/result/branch-1.txt \
        tests/result/applicative-order-1.txt \
        tests/result/and-test.txt \
        tests/result/add_sub.txt \
        tests/scm/var-list.scm \
        tests/scm/symbol-list.scm \
        tests/scm/simple-seq-print.scm \
        tests/scm/redefine.scm \
        tests/scm/rational-division.scm \
        tests/scm/rational-1.scm \
        tests/scm/raise.scm \
        tests/scm/raise-cont.scm \
        tests/scm/pair.scm \
        tests/scm/or-test.scm \
        tests/scm/numbers.scm \
        tests/scm/null.scm \
        tests/scm/nested-lambda-1.scm \
        tests/scm/nested-call.scm \
        tests/scm/named-let-narg-in-func.scm \
        tests/scm/named-let-loop.scm \
        tests/scm/named-let-loop-in-func.scm \
        tests/scm/named-let-3.scm \
        tests/scm/named-let-2.scm \
        tests/scm/named-let-1.scm \
        tests/scm/multiply.scm \
        tests/scm/local-def-4.scm \
        tests/scm/local-def-3.scm \
        tests/scm/local-def-2.scm \
        tests/scm/local-def-1.scm \
        tests/scm/list_to_string.scm \
        tests/scm/list-ref.scm \
        tests/scm/list-print.scm \
        tests/scm/list-car-cdr.scm \
        tests/scm/list-append.scm \
        tests/scm/let-star-side-effect-1.scm \
        tests/scm/let-star-optimized-out.scm \
        tests/scm/let-star-1.scm \
        tests/scm/let-1.scm \
        tests/scm/lambda-lifting-2.scm \
        tests/scm/lambda-lifting-1.scm \
        tests/scm/lambda-app-multi-args.scm \
        tests/scm/keyword-print.scm \
        tests/scm/int-list.scm \
        tests/scm/global-list-1.scm \
        tests/scm/global-expr-1.scm \
        tests/scm/gcd.scm \
        tests/scm/fold-branch-2.scm \
        tests/scm/fold-branch-1.scm \
        tests/scm/fibonacci.scm \
        tests/scm/factorial.scm \
        tests/scm/expr-in-mid-defs.scm \
        tests/scm/eq_gt_lt.scm \
        tests/scm/define-star-opts.scm \
        tests/scm/define-star-keys.scm \
        tests/scm/define-star-keys-opts.scm \
        tests/scm/closure-2.scm \
        tests/scm/closure-1.scm \
        tests/scm/char-print.scm \
        tests/scm/bytevector.scm \
        tests/scm/branch-2.scm \
        tests/scm/branch-1.scm \
        tests/scm/applicative-order-1.scm \
        tests/scm/and-test.scm \
        tests/scm/add_sub.scm \
        tests/test.scm

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = doc/version.texi \
                doc/laco.texi
dvi: # Don't build dvi docs

EXTRA_DIST += ChangeLog \
              AUTHORS \
              NEWS \
              doc/version.info \
              doc/version \
              doc/laco.info \
              doc/laco \
              doc/.dirstamp \
              COPYING \
              HACKING \
              README \
              pre-inst-env.in \
              Makefile.am \
              configure.ac \
              build-aux/texinfo.tex \
              build-aux/test-driver.scm \
              build-aux/missing \
              build-aux/mdate-sh \
              build-aux/install-sh \
              hall.scm \
              guix.scm \
              build-aux/test-driver.scm \
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)
