

bin_SCRIPTS = 

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

SOURCES = laco/pass/fold-const.scm \
          laco/pass/useless-cont.scm \
          laco/pass/normalize.scm \
          laco/pass/lambda-lifting.scm \
          laco/pass/func-inline.scm \
          laco/pass/fold-branch.scm \
          laco/pass/eta-func.scm \
          laco/pass/eta-cont.scm \
          laco/pass/elre.scm \
          laco/pass/delta-reduction.scm \
          laco/pass/dce.scm \
          laco/pass/const-propagation.scm \
          laco/pass/closure-conversion.scm \
          laco/module.scm \
          laco/object.scm \
          laco/sasm.scm \
          laco/utils.scm \
          laco/codegen.scm \
          laco/primitives.scm \
          laco/pass.scm \
          laco/env.scm \
          laco/parser.scm \
          laco/lir.scm \
          laco/types.scm \
          laco/cps.scm \
          laco/ast.scm \
          laco/compile.scm \
          laco.scm

TESTS = 

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
              doc/laco.info \
              doc/stamp-vti \
              doc/.dirstamp \
              COPYING \
              HACKING \
              README \
              README.org \
              pre-inst-env.in \
              Makefile.am \
              configure.ac \
              build-aux/texinfo.tex \
              build-aux/mdate-sh \
              build-aux/missing \
              build-aux/install-sh \
              build-aux/test-driver.scm \
              hall.scm \
              .gitignore \
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
