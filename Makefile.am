bin_SCRIPTS = scripts/laco

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

SOURCES = laco/assembler/sasm.scm \
          laco/assembler/encode.scm \
          laco/lpass/remove-unused-captures.scm \
          laco/lpass/fv-lifting.scm \
          laco/lpass/closure-capture-fv.scm \
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
          laco/utils.scm \
          laco/sasm.scm \
          laco/records.scm \
          laco/primitives.scm \
          laco/object.scm \
          laco/module.scm \
          laco/lir.scm \
          laco/env.scm \
          laco/codegen.scm \
          laco/macro.scm \
          laco/reserved.scm \
          laco.scm

TESTS = tests/test.scm

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=no

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = doc/version.texi \
                doc/laco.texi
dvi-local: # Don't build dvi docs

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
