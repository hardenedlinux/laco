# generated automatically by aclocal 1.16.5 -*- Autoconf -*-

# Copyright (C) 1996-2021 Free Software Foundation, Inc.

# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

m4_ifndef([AC_CONFIG_MACRO_DIRS], [m4_defun([_AM_CONFIG_MACRO_DIRS], [])m4_defun([AC_CONFIG_MACRO_DIRS], [_AM_CONFIG_MACRO_DIRS($@)])])
m4_ifndef([AC_AUTOCONF_VERSION],
  [m4_copy([m4_PACKAGE_VERSION], [AC_AUTOCONF_VERSION])])dnl
m4_if(m4_defn([AC_AUTOCONF_VERSION]), [2.71],,
[m4_warning([this file was generated for autoconf 2.71.
You have another version of autoconf.  It may work, but is not guaranteed to.
If you have problems, you may need to regenerate the build system entirely.
To do so, use the procedure documented by the package, typically 'autoreconf'.])])


# serial 11




# GUILE_PKG -- find Guile development files
#
# Usage: GUILE_PKG([VERSIONS])
#
# This macro runs the @code{pkg-config} tool to find development files
# for an available version of Guile.
#
# By default, this macro will search for the latest stable version of
# Guile (e.g. 3.0), falling back to the previous stable version
# (e.g. 2.2) if it is available.  If no guile-@var{VERSION}.pc file is
# found, an error is signaled.  The found version is stored in
# @var{GUILE_EFFECTIVE_VERSION}.
#
# If @code{GUILE_PROGS} was already invoked, this macro ensures that the
# development files have the same effective version as the Guile
# program.
#
# @var{GUILE_EFFECTIVE_VERSION} is marked for substitution, as by
# @code{AC_SUBST}.
#
AC_DEFUN([GUILE_PKG],
 [AC_REQUIRE([PKG_PROG_PKG_CONFIG])
  if test "x$PKG_CONFIG" = x; then
    AC_MSG_ERROR([pkg-config is missing, please install it])
  fi
  _guile_versions_to_search="m4_default([$1], [3.0 2.2 2.0])"
  if test -n "$GUILE_EFFECTIVE_VERSION"; then
    _guile_tmp=""
    for v in $_guile_versions_to_search; do
      if test "$v" = "$GUILE_EFFECTIVE_VERSION"; then
        _guile_tmp=$v
      fi
    done
    if test -z "$_guile_tmp"; then
      AC_MSG_FAILURE([searching for guile development files for versions $_guile_versions_to_search, but previously found $GUILE version $GUILE_EFFECTIVE_VERSION])
    fi
    _guile_versions_to_search=$GUILE_EFFECTIVE_VERSION
  fi
  GUILE_EFFECTIVE_VERSION=""
  _guile_errors=""
  for v in $_guile_versions_to_search; do
    if test -z "$GUILE_EFFECTIVE_VERSION"; then
      AC_MSG_NOTICE([checking for guile $v])
      PKG_CHECK_EXISTS([guile-$v], [GUILE_EFFECTIVE_VERSION=$v], [])
    fi
  done

  if test -z "$GUILE_EFFECTIVE_VERSION"; then
    AC_MSG_ERROR([
No Guile development packages were found.

Please verify that you have Guile installed.  If you installed Guile
from a binary distribution, please verify that you have also installed
the development packages.  If you installed it yourself, you might need
to adjust your PKG_CONFIG_PATH; see the pkg-config man page for more.
])
  fi
  AC_MSG_NOTICE([found guile $GUILE_EFFECTIVE_VERSION])
  AC_SUBST([GUILE_EFFECTIVE_VERSION])
 ])

# GUILE_FLAGS -- set flags for compiling and linking with Guile
#
# Usage: GUILE_FLAGS
#
# This macro runs the @code{pkg-config} tool to find out how to compile
# and link programs against Guile.  It sets four variables:
# @var{GUILE_CFLAGS}, @var{GUILE_LDFLAGS}, @var{GUILE_LIBS}, and
# @var{GUILE_LTLIBS}.
#
# @var{GUILE_CFLAGS}: flags to pass to a C or C++ compiler to build code that
# uses Guile header files.  This is almost always just one or more @code{-I}
# flags.
#
# @var{GUILE_LDFLAGS}: flags to pass to the compiler to link a program
# against Guile.  This includes @code{-lguile-@var{VERSION}} for the
# Guile library itself, and may also include one or more @code{-L} flag
# to tell the compiler where to find the libraries.  But it does not
# include flags that influence the program's runtime search path for
# libraries, and will therefore lead to a program that fails to start,
# unless all necessary libraries are installed in a standard location
# such as @file{/usr/lib}.
#
# @var{GUILE_LIBS} and @var{GUILE_LTLIBS}: flags to pass to the compiler or to
# libtool, respectively, to link a program against Guile.  It includes flags
# that augment the program's runtime search path for libraries, so that shared
# libraries will be found at the location where they were during linking, even
# in non-standard locations.  @var{GUILE_LIBS} is to be used when linking the
# program directly with the compiler, whereas @var{GUILE_LTLIBS} is to be used
# when linking the program is done through libtool.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_FLAGS],
 [AC_REQUIRE([GUILE_PKG])
  PKG_CHECK_MODULES(GUILE, [guile-$GUILE_EFFECTIVE_VERSION])

  dnl GUILE_CFLAGS and GUILE_LIBS are already defined and AC_SUBST'd by
  dnl PKG_CHECK_MODULES.  But GUILE_LIBS to pkg-config is GUILE_LDFLAGS
  dnl to us.

  GUILE_LDFLAGS=$GUILE_LIBS

  dnl Determine the platform dependent parameters needed to use rpath.
  dnl AC_LIB_LINKFLAGS_FROM_LIBS is defined in gnulib/m4/lib-link.m4 and needs
  dnl the file gnulib/build-aux/config.rpath.
  AC_LIB_LINKFLAGS_FROM_LIBS([GUILE_LIBS], [$GUILE_LDFLAGS], [])
  GUILE_LIBS="$GUILE_LDFLAGS $GUILE_LIBS"
  AC_LIB_LINKFLAGS_FROM_LIBS([GUILE_LTLIBS], [$GUILE_LDFLAGS], [yes])
  GUILE_LTLIBS="$GUILE_LDFLAGS $GUILE_LTLIBS"

  AC_SUBST([GUILE_EFFECTIVE_VERSION])
  AC_SUBST([GUILE_CFLAGS])
  AC_SUBST([GUILE_LDFLAGS])
  AC_SUBST([GUILE_LIBS])
  AC_SUBST([GUILE_LTLIBS])
 ])

# GUILE_SITE_DIR -- find path to Guile site directories
#
# Usage: GUILE_SITE_DIR
#
# This looks for Guile's "site" directories.  The variable @var{GUILE_SITE} will
# be set to Guile's "site" directory for Scheme source files (usually something
# like PREFIX/share/guile/site).  @var{GUILE_SITE_CCACHE} will be set to the
# directory for compiled Scheme files also known as @code{.go} files
# (usually something like
# PREFIX/lib/guile/@var{GUILE_EFFECTIVE_VERSION}/site-ccache).
# @var{GUILE_EXTENSION} will be set to the directory for compiled C extensions
# (usually something like
# PREFIX/lib/guile/@var{GUILE_EFFECTIVE_VERSION}/extensions). The latter two
# are set to blank if the particular version of Guile does not support
# them.  Note that this macro will run the macros @code{GUILE_PKG} and
# @code{GUILE_PROGS} if they have not already been run.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_SITE_DIR],
 [AC_REQUIRE([GUILE_PKG])
  AC_REQUIRE([GUILE_PROGS])
  AC_MSG_CHECKING(for Guile site directory)
  GUILE_SITE=`$PKG_CONFIG --print-errors --variable=sitedir guile-$GUILE_EFFECTIVE_VERSION`
  AC_MSG_RESULT($GUILE_SITE)
  if test "$GUILE_SITE" = ""; then
     AC_MSG_FAILURE(sitedir not found)
  fi
  AC_SUBST(GUILE_SITE)
  AC_MSG_CHECKING([for Guile site-ccache directory using pkgconfig])
  GUILE_SITE_CCACHE=`$PKG_CONFIG --variable=siteccachedir guile-$GUILE_EFFECTIVE_VERSION`
  if test "$GUILE_SITE_CCACHE" = ""; then
    AC_MSG_RESULT(no)
    AC_MSG_CHECKING([for Guile site-ccache directory using interpreter])
    GUILE_SITE_CCACHE=`$GUILE -c "(display (if (defined? '%site-ccache-dir) (%site-ccache-dir) \"\"))"`
    if test $? != "0" -o "$GUILE_SITE_CCACHE" = ""; then
      AC_MSG_RESULT(no)
      GUILE_SITE_CCACHE=""
      AC_MSG_WARN([siteccachedir not found])
    fi
  fi
  AC_MSG_RESULT($GUILE_SITE_CCACHE)
  AC_SUBST([GUILE_SITE_CCACHE])
  AC_MSG_CHECKING(for Guile extensions directory)
  GUILE_EXTENSION=`$PKG_CONFIG --print-errors --variable=extensiondir guile-$GUILE_EFFECTIVE_VERSION`
  AC_MSG_RESULT($GUILE_EXTENSION)
  if test "$GUILE_EXTENSION" = ""; then
    GUILE_EXTENSION=""
    AC_MSG_WARN(extensiondir not found)
  fi
  AC_SUBST(GUILE_EXTENSION)
 ])

# GUILE_PROGS -- set paths to Guile interpreter, config and tool programs
#
# Usage: GUILE_PROGS([VERSION])
#
# This macro looks for programs @code{guile} and @code{guild}, setting
# variables @var{GUILE} and @var{GUILD} to their paths, respectively.
# The macro will attempt to find @code{guile} with the suffix of
# @code{-X.Y}, followed by looking for it with the suffix @code{X.Y}, and
# then fall back to looking for @code{guile} with no suffix. If
# @code{guile} is still not found, signal an error. The suffix, if any,
# that was required to find @code{guile} will be used for @code{guild}
# as well.
#
# By default, this macro will search for the latest stable version of
# Guile (e.g. 3.0). x.y or x.y.z versions can be specified. If an older
# version is found, the macro will signal an error.
#
# The effective version of the found @code{guile} is set to
# @var{GUILE_EFFECTIVE_VERSION}.  This macro ensures that the effective
# version is compatible with the result of a previous invocation of
# @code{GUILE_FLAGS}, if any.
#
# As a legacy interface, it also looks for @code{guile-config} and
# @code{guile-tools}, setting @var{GUILE_CONFIG} and @var{GUILE_TOOLS}.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_PROGS],
 [_guile_required_version="m4_default([$1], [$GUILE_EFFECTIVE_VERSION])"
  if test -z "$_guile_required_version"; then
    _guile_required_version=3.0
  fi

  _guile_candidates=guile
  _tmp=
  for v in `echo "$_guile_required_version" | tr . ' '`; do
    if test -n "$_tmp"; then _tmp=$_tmp.; fi
    _tmp=$_tmp$v
    _guile_candidates="guile-$_tmp guile$_tmp $_guile_candidates"
  done

  AC_PATH_PROGS(GUILE,[$_guile_candidates])
  if test -z "$GUILE"; then
      AC_MSG_ERROR([guile required but not found])
  fi

  _guile_suffix=`echo "$GUILE" | sed -e 's,^.*/guile\(.*\)$,\1,'`
  _guile_effective_version=`$GUILE -c "(display (effective-version))"`
  if test -z "$GUILE_EFFECTIVE_VERSION"; then
    GUILE_EFFECTIVE_VERSION=$_guile_effective_version
  elif test "$GUILE_EFFECTIVE_VERSION" != "$_guile_effective_version"; then
    AC_MSG_ERROR([found development files for Guile $GUILE_EFFECTIVE_VERSION, but $GUILE has effective version $_guile_effective_version])
  fi

  _guile_major_version=`$GUILE -c "(display (major-version))"`
  _guile_minor_version=`$GUILE -c "(display (minor-version))"`
  _guile_micro_version=`$GUILE -c "(display (micro-version))"`
  _guile_prog_version="$_guile_major_version.$_guile_minor_version.$_guile_micro_version"

  AC_MSG_CHECKING([for Guile version >= $_guile_required_version])
  _major_version=`echo $_guile_required_version | cut -d . -f 1`
  _minor_version=`echo $_guile_required_version | cut -d . -f 2`
  _micro_version=`echo $_guile_required_version | cut -d . -f 3`
  if test "$_guile_major_version" -gt "$_major_version"; then
    true
  elif test "$_guile_major_version" -eq "$_major_version"; then
    if test "$_guile_minor_version" -gt "$_minor_version"; then
      true
    elif test "$_guile_minor_version" -eq "$_minor_version"; then
      if test -n "$_micro_version"; then
        if test "$_guile_micro_version" -lt "$_micro_version"; then
          AC_MSG_ERROR([Guile $_guile_required_version required, but $_guile_prog_version found])
        fi
      fi
    elif test "$GUILE_EFFECTIVE_VERSION" = "$_major_version.$_minor_version" -a -z "$_micro_version"; then
      # Allow prereleases that have the right effective version.
      true
    else
      as_fn_error $? "Guile $_guile_required_version required, but $_guile_prog_version found" "$LINENO" 5
    fi
  elif test "$GUILE_EFFECTIVE_VERSION" = "$_major_version.$_minor_version" -a -z "$_micro_version"; then
    # Allow prereleases that have the right effective version.
    true
  else
    AC_MSG_ERROR([Guile $_guile_required_version required, but $_guile_prog_version found])
  fi
  AC_MSG_RESULT([$_guile_prog_version])

  AC_PATH_PROG(GUILD,[guild$_guile_suffix])
  AC_SUBST(GUILD)

  AC_PATH_PROG(GUILE_CONFIG,[guile-config$_guile_suffix])
  AC_SUBST(GUILE_CONFIG)
  if test -n "$GUILD"; then
    GUILE_TOOLS=$GUILD
  else
    AC_PATH_PROG(GUILE_TOOLS,[guile-tools$_guile_suffix])
  fi
  AC_SUBST(GUILE_TOOLS)
 ])

# GUILE_CHECK -- evaluate Guile Scheme code and capture the return value
#
# Usage: GUILE_CHECK_RETVAL(var,check)
#
# @var{var} is a shell variable name to be set to the return value.
# @var{check} is a Guile Scheme expression, evaluated with "$GUILE -c", and
#    returning either 0 or non-#f to indicate the check passed.
#    Non-0 number or #f indicates failure.
#    Avoid using the character "#" since that confuses autoconf.
#
AC_DEFUN([GUILE_CHECK],
 [AC_REQUIRE([GUILE_PROGS])
  $GUILE -c "$2" > /dev/null 2>&1
  $1=$?
 ])

# GUILE_MODULE_CHECK -- check feature of a Guile Scheme module
#
# Usage: GUILE_MODULE_CHECK(var,module,featuretest,description)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (ice-9 common-list).
# @var{featuretest} is an expression acceptable to GUILE_CHECK, q.v.
# @var{description} is a present-tense verb phrase (passed to AC_MSG_CHECKING).
#
AC_DEFUN([GUILE_MODULE_CHECK],
         [AC_MSG_CHECKING([if $2 $4])
	  GUILE_CHECK($1,(use-modules $2) (exit ((lambda () $3))))
	  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
          AC_MSG_RESULT($$1)
         ])

# GUILE_MODULE_AVAILABLE -- check availability of a Guile Scheme module
#
# Usage: GUILE_MODULE_AVAILABLE(var,module)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (ice-9 common-list).
#
AC_DEFUN([GUILE_MODULE_AVAILABLE],
         [GUILE_MODULE_CHECK($1,$2,0,is available)
         ])

# GUILE_MODULE_REQUIRED -- fail if a Guile Scheme module is unavailable
#
# Usage: GUILE_MODULE_REQUIRED(symlist)
#
# @var{symlist} is a list of symbols, WITHOUT surrounding parens,
# like: ice-9 common-list.
#
AC_DEFUN([GUILE_MODULE_REQUIRED],
         [GUILE_MODULE_AVAILABLE(ac_guile_module_required, ($1))
          if test "$ac_guile_module_required" = "no" ; then
              AC_MSG_ERROR([required guile module not found: ($1)])
          fi
         ])

# GUILE_MODULE_EXPORTS -- check if a module exports a variable
#
# Usage: GUILE_MODULE_EXPORTS(var,module,modvar)
#
# @var{var} is a shell variable to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (ice-9 common-list).
# @var{modvar} is the Guile Scheme variable to check.
#
AC_DEFUN([GUILE_MODULE_EXPORTS],
 [GUILE_MODULE_CHECK($1,$2,$3,exports `$3')
 ])

# GUILE_MODULE_REQUIRED_EXPORT -- fail if a module doesn't export a variable
#
# Usage: GUILE_MODULE_REQUIRED_EXPORT(module,modvar)
#
# @var{module} is a list of symbols, like: (ice-9 common-list).
# @var{modvar} is the Guile Scheme variable to check.
#
AC_DEFUN([GUILE_MODULE_REQUIRED_EXPORT],
 [GUILE_MODULE_EXPORTS(guile_module_required_export,$1,$2)
  if test "$guile_module_required_export" = "no" ; then
      AC_MSG_ERROR([module $1 does not export $2; required])
  fi
 ])


# host-cpu-c-abi.m4 serial 13
dnl Copyright (C) 2002-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible and Sam Steingold.

dnl Sets the HOST_CPU variable to the canonical name of the CPU.
dnl Sets the HOST_CPU_C_ABI variable to the canonical name of the CPU with its
dnl C language ABI (application binary interface).
dnl Also defines __${HOST_CPU}__ and __${HOST_CPU_C_ABI}__ as C macros in
dnl config.h.
dnl
dnl This canonical name can be used to select a particular assembly language
dnl source file that will interoperate with C code on the given host.
dnl
dnl For example:
dnl * 'i386' and 'sparc' are different canonical names, because code for i386
dnl   will not run on SPARC CPUs and vice versa. They have different
dnl   instruction sets.
dnl * 'sparc' and 'sparc64' are different canonical names, because code for
dnl   'sparc' and code for 'sparc64' cannot be linked together: 'sparc' code
dnl   contains 32-bit instructions, whereas 'sparc64' code contains 64-bit
dnl   instructions. A process on a SPARC CPU can be in 32-bit mode or in 64-bit
dnl   mode, but not both.
dnl * 'mips' and 'mipsn32' are different canonical names, because they use
dnl   different argument passing and return conventions for C functions, and
dnl   although the instruction set of 'mips' is a large subset of the
dnl   instruction set of 'mipsn32'.
dnl * 'mipsn32' and 'mips64' are different canonical names, because they use
dnl   different sizes for the C types like 'int' and 'void *', and although
dnl   the instruction sets of 'mipsn32' and 'mips64' are the same.
dnl * The same canonical name is used for different endiannesses. You can
dnl   determine the endianness through preprocessor symbols:
dnl   - 'arm': test __ARMEL__.
dnl   - 'mips', 'mipsn32', 'mips64': test _MIPSEB vs. _MIPSEL.
dnl   - 'powerpc64': test _BIG_ENDIAN vs. _LITTLE_ENDIAN.
dnl * The same name 'i386' is used for CPUs of type i386, i486, i586
dnl   (Pentium), AMD K7, Pentium II, Pentium IV, etc., because
dnl   - Instructions that do not exist on all of these CPUs (cmpxchg,
dnl     MMX, SSE, SSE2, 3DNow! etc.) are not frequently used. If your
dnl     assembly language source files use such instructions, you will
dnl     need to make the distinction.
dnl   - Speed of execution of the common instruction set is reasonable across
dnl     the entire family of CPUs. If you have assembly language source files
dnl     that are optimized for particular CPU types (like GNU gmp has), you
dnl     will need to make the distinction.
dnl   See <https://en.wikipedia.org/wiki/X86_instruction_listings>.
AC_DEFUN([gl_HOST_CPU_C_ABI],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_C_ASM])
  AC_CACHE_CHECK([host CPU and C ABI], [gl_cv_host_cpu_c_abi],
    [case "$host_cpu" in

changequote(,)dnl
       i[34567]86 )
changequote([,])dnl
         gl_cv_host_cpu_c_abi=i386
         ;;

       x86_64 )
         # On x86_64 systems, the C compiler may be generating code in one of
         # these ABIs:
         # - 64-bit instruction set, 64-bit pointers, 64-bit 'long': x86_64.
         # - 64-bit instruction set, 64-bit pointers, 32-bit 'long': x86_64
         #   with native Windows (mingw, MSVC).
         # - 64-bit instruction set, 32-bit pointers, 32-bit 'long': x86_64-x32.
         # - 32-bit instruction set, 32-bit pointers, 32-bit 'long': i386.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#if (defined __x86_64__ || defined __amd64__ \
                     || defined _M_X64 || defined _M_AMD64)
                 int ok;
                #else
                 error fail
                #endif
              ]])],
           [AC_COMPILE_IFELSE(
              [AC_LANG_SOURCE(
                 [[#if defined __ILP32__ || defined _ILP32
                    int ok;
                   #else
                    error fail
                   #endif
                 ]])],
              [gl_cv_host_cpu_c_abi=x86_64-x32],
              [gl_cv_host_cpu_c_abi=x86_64])],
           [gl_cv_host_cpu_c_abi=i386])
         ;;

changequote(,)dnl
       alphaev[4-8] | alphaev56 | alphapca5[67] | alphaev6[78] )
changequote([,])dnl
         gl_cv_host_cpu_c_abi=alpha
         ;;

       arm* | aarch64 )
         # Assume arm with EABI.
         # On arm64 systems, the C compiler may be generating code in one of
         # these ABIs:
         # - aarch64 instruction set, 64-bit pointers, 64-bit 'long': arm64.
         # - aarch64 instruction set, 32-bit pointers, 32-bit 'long': arm64-ilp32.
         # - 32-bit instruction set, 32-bit pointers, 32-bit 'long': arm or armhf.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#ifdef __aarch64__
                 int ok;
                #else
                 error fail
                #endif
              ]])],
           [AC_COMPILE_IFELSE(
              [AC_LANG_SOURCE(
                [[#if defined __ILP32__ || defined _ILP32
                   int ok;
                  #else
                   error fail
                  #endif
                ]])],
              [gl_cv_host_cpu_c_abi=arm64-ilp32],
              [gl_cv_host_cpu_c_abi=arm64])],
           [# Don't distinguish little-endian and big-endian arm, since they
            # don't require different machine code for simple operations and
            # since the user can distinguish them through the preprocessor
            # defines __ARMEL__ vs. __ARMEB__.
            # But distinguish arm which passes floating-point arguments and
            # return values in integer registers (r0, r1, ...) - this is
            # gcc -mfloat-abi=soft or gcc -mfloat-abi=softfp - from arm which
            # passes them in float registers (s0, s1, ...) and double registers
            # (d0, d1, ...) - this is gcc -mfloat-abi=hard. GCC 4.6 or newer
            # sets the preprocessor defines __ARM_PCS (for the first case) and
            # __ARM_PCS_VFP (for the second case), but older GCC does not.
            echo 'double ddd; void func (double dd) { ddd = dd; }' > conftest.c
            # Look for a reference to the register d0 in the .s file.
            AC_TRY_COMMAND(${CC-cc} $CFLAGS $CPPFLAGS $gl_c_asm_opt conftest.c) >/dev/null 2>&1
            if LC_ALL=C grep 'd0,' conftest.$gl_asmext >/dev/null; then
              gl_cv_host_cpu_c_abi=armhf
            else
              gl_cv_host_cpu_c_abi=arm
            fi
            rm -f conftest*
           ])
         ;;

       hppa1.0 | hppa1.1 | hppa2.0* | hppa64 )
         # On hppa, the C compiler may be generating 32-bit code or 64-bit
         # code. In the latter case, it defines _LP64 and __LP64__.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#ifdef __LP64__
                 int ok;
                #else
                 error fail
                #endif
              ]])],
           [gl_cv_host_cpu_c_abi=hppa64],
           [gl_cv_host_cpu_c_abi=hppa])
         ;;

       ia64* )
         # On ia64 on HP-UX, the C compiler may be generating 64-bit code or
         # 32-bit code. In the latter case, it defines _ILP32.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#ifdef _ILP32
                 int ok;
                #else
                 error fail
                #endif
              ]])],
           [gl_cv_host_cpu_c_abi=ia64-ilp32],
           [gl_cv_host_cpu_c_abi=ia64])
         ;;

       mips* )
         # We should also check for (_MIPS_SZPTR == 64), but gcc keeps this
         # at 32.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#if defined _MIPS_SZLONG && (_MIPS_SZLONG == 64)
                 int ok;
                #else
                 error fail
                #endif
              ]])],
           [gl_cv_host_cpu_c_abi=mips64],
           [# In the n32 ABI, _ABIN32 is defined, _ABIO32 is not defined (but
            # may later get defined by <sgidefs.h>), and _MIPS_SIM == _ABIN32.
            # In the 32 ABI, _ABIO32 is defined, _ABIN32 is not defined (but
            # may later get defined by <sgidefs.h>), and _MIPS_SIM == _ABIO32.
            AC_COMPILE_IFELSE(
              [AC_LANG_SOURCE(
                 [[#if (_MIPS_SIM == _ABIN32)
                    int ok;
                   #else
                    error fail
                   #endif
                 ]])],
              [gl_cv_host_cpu_c_abi=mipsn32],
              [gl_cv_host_cpu_c_abi=mips])])
         ;;

       powerpc* )
         # Different ABIs are in use on AIX vs. Mac OS X vs. Linux,*BSD.
         # No need to distinguish them here; the caller may distinguish
         # them based on the OS.
         # On powerpc64 systems, the C compiler may still be generating
         # 32-bit code. And on powerpc-ibm-aix systems, the C compiler may
         # be generating 64-bit code.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#if defined __powerpc64__ || defined _ARCH_PPC64
                 int ok;
                #else
                 error fail
                #endif
              ]])],
           [# On powerpc64, there are two ABIs on Linux: The AIX compatible
            # one and the ELFv2 one. The latter defines _CALL_ELF=2.
            AC_COMPILE_IFELSE(
              [AC_LANG_SOURCE(
                 [[#if defined _CALL_ELF && _CALL_ELF == 2
                    int ok;
                   #else
                    error fail
                   #endif
                 ]])],
              [gl_cv_host_cpu_c_abi=powerpc64-elfv2],
              [gl_cv_host_cpu_c_abi=powerpc64])
           ],
           [gl_cv_host_cpu_c_abi=powerpc])
         ;;

       rs6000 )
         gl_cv_host_cpu_c_abi=powerpc
         ;;

       riscv32 | riscv64 )
         # There are 2 architectures (with variants): rv32* and rv64*.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#if __riscv_xlen == 64
                  int ok;
                #else
                  error fail
                #endif
              ]])],
           [cpu=riscv64],
           [cpu=riscv32])
         # There are 6 ABIs: ilp32, ilp32f, ilp32d, lp64, lp64f, lp64d.
         # Size of 'long' and 'void *':
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#if defined __LP64__
                  int ok;
                #else
                  error fail
                #endif
              ]])],
           [main_abi=lp64],
           [main_abi=ilp32])
         # Float ABIs:
         # __riscv_float_abi_double:
         #   'float' and 'double' are passed in floating-point registers.
         # __riscv_float_abi_single:
         #   'float' are passed in floating-point registers.
         # __riscv_float_abi_soft:
         #   No values are passed in floating-point registers.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#if defined __riscv_float_abi_double
                  int ok;
                #else
                  error fail
                #endif
              ]])],
           [float_abi=d],
           [AC_COMPILE_IFELSE(
              [AC_LANG_SOURCE(
                 [[#if defined __riscv_float_abi_single
                     int ok;
                   #else
                     error fail
                   #endif
                 ]])],
              [float_abi=f],
              [float_abi=''])
           ])
         gl_cv_host_cpu_c_abi="${cpu}-${main_abi}${float_abi}"
         ;;

       s390* )
         # On s390x, the C compiler may be generating 64-bit (= s390x) code
         # or 31-bit (= s390) code.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#if defined __LP64__ || defined __s390x__
                  int ok;
                #else
                  error fail
                #endif
              ]])],
           [gl_cv_host_cpu_c_abi=s390x],
           [gl_cv_host_cpu_c_abi=s390])
         ;;

       sparc | sparc64 )
         # UltraSPARCs running Linux have `uname -m` = "sparc64", but the
         # C compiler still generates 32-bit code.
         AC_COMPILE_IFELSE(
           [AC_LANG_SOURCE(
              [[#if defined __sparcv9 || defined __arch64__
                 int ok;
                #else
                 error fail
                #endif
              ]])],
           [gl_cv_host_cpu_c_abi=sparc64],
           [gl_cv_host_cpu_c_abi=sparc])
         ;;

       *)
         gl_cv_host_cpu_c_abi="$host_cpu"
         ;;
     esac
    ])

  dnl In most cases, $HOST_CPU and $HOST_CPU_C_ABI are the same.
  HOST_CPU=`echo "$gl_cv_host_cpu_c_abi" | sed -e 's/-.*//'`
  HOST_CPU_C_ABI="$gl_cv_host_cpu_c_abi"
  AC_SUBST([HOST_CPU])
  AC_SUBST([HOST_CPU_C_ABI])

  # This was
  #   AC_DEFINE_UNQUOTED([__${HOST_CPU}__])
  #   AC_DEFINE_UNQUOTED([__${HOST_CPU_C_ABI}__])
  # earlier, but KAI C++ 3.2d doesn't like this.
  sed -e 's/-/_/g' >> confdefs.h <<EOF
#ifndef __${HOST_CPU}__
#define __${HOST_CPU}__ 1
#endif
#ifndef __${HOST_CPU_C_ABI}__
#define __${HOST_CPU_C_ABI}__ 1
#endif
EOF
  AH_TOP([/* CPU and C ABI indicator */
#ifndef __i386__
#undef __i386__
#endif
#ifndef __x86_64_x32__
#undef __x86_64_x32__
#endif
#ifndef __x86_64__
#undef __x86_64__
#endif
#ifndef __alpha__
#undef __alpha__
#endif
#ifndef __arm__
#undef __arm__
#endif
#ifndef __armhf__
#undef __armhf__
#endif
#ifndef __arm64_ilp32__
#undef __arm64_ilp32__
#endif
#ifndef __arm64__
#undef __arm64__
#endif
#ifndef __hppa__
#undef __hppa__
#endif
#ifndef __hppa64__
#undef __hppa64__
#endif
#ifndef __ia64_ilp32__
#undef __ia64_ilp32__
#endif
#ifndef __ia64__
#undef __ia64__
#endif
#ifndef __m68k__
#undef __m68k__
#endif
#ifndef __mips__
#undef __mips__
#endif
#ifndef __mipsn32__
#undef __mipsn32__
#endif
#ifndef __mips64__
#undef __mips64__
#endif
#ifndef __powerpc__
#undef __powerpc__
#endif
#ifndef __powerpc64__
#undef __powerpc64__
#endif
#ifndef __powerpc64_elfv2__
#undef __powerpc64_elfv2__
#endif
#ifndef __riscv32__
#undef __riscv32__
#endif
#ifndef __riscv64__
#undef __riscv64__
#endif
#ifndef __riscv32_ilp32__
#undef __riscv32_ilp32__
#endif
#ifndef __riscv32_ilp32f__
#undef __riscv32_ilp32f__
#endif
#ifndef __riscv32_ilp32d__
#undef __riscv32_ilp32d__
#endif
#ifndef __riscv64_ilp32__
#undef __riscv64_ilp32__
#endif
#ifndef __riscv64_ilp32f__
#undef __riscv64_ilp32f__
#endif
#ifndef __riscv64_ilp32d__
#undef __riscv64_ilp32d__
#endif
#ifndef __riscv64_lp64__
#undef __riscv64_lp64__
#endif
#ifndef __riscv64_lp64f__
#undef __riscv64_lp64f__
#endif
#ifndef __riscv64_lp64d__
#undef __riscv64_lp64d__
#endif
#ifndef __s390__
#undef __s390__
#endif
#ifndef __s390x__
#undef __s390x__
#endif
#ifndef __sh__
#undef __sh__
#endif
#ifndef __sparc__
#undef __sparc__
#endif
#ifndef __sparc64__
#undef __sparc64__
#endif
])

])


dnl Sets the HOST_CPU_C_ABI_32BIT variable to 'yes' if the C language ABI
dnl (application binary interface) is a 32-bit one, to 'no' if it is a 64-bit
dnl one, or to 'unknown' if unknown.
dnl This is a simplified variant of gl_HOST_CPU_C_ABI.
AC_DEFUN([gl_HOST_CPU_C_ABI_32BIT],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_CACHE_CHECK([32-bit host C ABI], [gl_cv_host_cpu_c_abi_32bit],
    [if test -n "$gl_cv_host_cpu_c_abi"; then
       case "$gl_cv_host_cpu_c_abi" in
         i386 | x86_64-x32 | arm | armhf | arm64-ilp32 | hppa | ia64-ilp32 | mips | mipsn32 | powerpc | riscv*-ilp32* | s390 | sparc)
           gl_cv_host_cpu_c_abi_32bit=yes ;;
         x86_64 | alpha | arm64 | hppa64 | ia64 | mips64 | powerpc64 | powerpc64-elfv2 | riscv*-lp64* | s390x | sparc64 )
           gl_cv_host_cpu_c_abi_32bit=no ;;
         *)
           gl_cv_host_cpu_c_abi_32bit=unknown ;;
       esac
     else
       case "$host_cpu" in

         # CPUs that only support a 32-bit ABI.
         arc \
         | bfin \
         | cris* \
         | csky \
         | epiphany \
         | ft32 \
         | h8300 \
         | m68k \
         | microblaze | microblazeel \
         | nds32 | nds32le | nds32be \
         | nios2 | nios2eb | nios2el \
         | or1k* \
         | or32 \
         | sh | sh[1234] | sh[1234]e[lb] \
         | tic6x \
         | xtensa* )
           gl_cv_host_cpu_c_abi_32bit=yes
           ;;

         # CPUs that only support a 64-bit ABI.
changequote(,)dnl
         alpha | alphaev[4-8] | alphaev56 | alphapca5[67] | alphaev6[78] \
         | mmix )
changequote([,])dnl
           gl_cv_host_cpu_c_abi_32bit=no
           ;;

changequote(,)dnl
         i[34567]86 )
changequote([,])dnl
           gl_cv_host_cpu_c_abi_32bit=yes
           ;;

         x86_64 )
           # On x86_64 systems, the C compiler may be generating code in one of
           # these ABIs:
           # - 64-bit instruction set, 64-bit pointers, 64-bit 'long': x86_64.
           # - 64-bit instruction set, 64-bit pointers, 32-bit 'long': x86_64
           #   with native Windows (mingw, MSVC).
           # - 64-bit instruction set, 32-bit pointers, 32-bit 'long': x86_64-x32.
           # - 32-bit instruction set, 32-bit pointers, 32-bit 'long': i386.
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#if (defined __x86_64__ || defined __amd64__ \
                       || defined _M_X64 || defined _M_AMD64) \
                      && !(defined __ILP32__ || defined _ILP32)
                   int ok;
                  #else
                   error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=no],
             [gl_cv_host_cpu_c_abi_32bit=yes])
           ;;

         arm* | aarch64 )
           # Assume arm with EABI.
           # On arm64 systems, the C compiler may be generating code in one of
           # these ABIs:
           # - aarch64 instruction set, 64-bit pointers, 64-bit 'long': arm64.
           # - aarch64 instruction set, 32-bit pointers, 32-bit 'long': arm64-ilp32.
           # - 32-bit instruction set, 32-bit pointers, 32-bit 'long': arm or armhf.
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#if defined __aarch64__ && !(defined __ILP32__ || defined _ILP32)
                   int ok;
                  #else
                   error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=no],
             [gl_cv_host_cpu_c_abi_32bit=yes])
           ;;

         hppa1.0 | hppa1.1 | hppa2.0* | hppa64 )
           # On hppa, the C compiler may be generating 32-bit code or 64-bit
           # code. In the latter case, it defines _LP64 and __LP64__.
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#ifdef __LP64__
                   int ok;
                  #else
                   error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=no],
             [gl_cv_host_cpu_c_abi_32bit=yes])
           ;;

         ia64* )
           # On ia64 on HP-UX, the C compiler may be generating 64-bit code or
           # 32-bit code. In the latter case, it defines _ILP32.
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#ifdef _ILP32
                   int ok;
                  #else
                   error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=yes],
             [gl_cv_host_cpu_c_abi_32bit=no])
           ;;

         mips* )
           # We should also check for (_MIPS_SZPTR == 64), but gcc keeps this
           # at 32.
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#if defined _MIPS_SZLONG && (_MIPS_SZLONG == 64)
                   int ok;
                  #else
                   error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=no],
             [gl_cv_host_cpu_c_abi_32bit=yes])
           ;;

         powerpc* )
           # Different ABIs are in use on AIX vs. Mac OS X vs. Linux,*BSD.
           # No need to distinguish them here; the caller may distinguish
           # them based on the OS.
           # On powerpc64 systems, the C compiler may still be generating
           # 32-bit code. And on powerpc-ibm-aix systems, the C compiler may
           # be generating 64-bit code.
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#if defined __powerpc64__ || defined _ARCH_PPC64
                   int ok;
                  #else
                   error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=no],
             [gl_cv_host_cpu_c_abi_32bit=yes])
           ;;

         rs6000 )
           gl_cv_host_cpu_c_abi_32bit=yes
           ;;

         riscv32 | riscv64 )
           # There are 6 ABIs: ilp32, ilp32f, ilp32d, lp64, lp64f, lp64d.
           # Size of 'long' and 'void *':
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#if defined __LP64__
                    int ok;
                  #else
                    error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=no],
             [gl_cv_host_cpu_c_abi_32bit=yes])
           ;;

         s390* )
           # On s390x, the C compiler may be generating 64-bit (= s390x) code
           # or 31-bit (= s390) code.
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#if defined __LP64__ || defined __s390x__
                    int ok;
                  #else
                    error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=no],
             [gl_cv_host_cpu_c_abi_32bit=yes])
           ;;

         sparc | sparc64 )
           # UltraSPARCs running Linux have `uname -m` = "sparc64", but the
           # C compiler still generates 32-bit code.
           AC_COMPILE_IFELSE(
             [AC_LANG_SOURCE(
                [[#if defined __sparcv9 || defined __arch64__
                   int ok;
                  #else
                   error fail
                  #endif
                ]])],
             [gl_cv_host_cpu_c_abi_32bit=no],
             [gl_cv_host_cpu_c_abi_32bit=yes])
           ;;

         *)
           gl_cv_host_cpu_c_abi_32bit=unknown
           ;;
       esac
     fi
    ])

  HOST_CPU_C_ABI_32BIT="$gl_cv_host_cpu_c_abi_32bit"
])

# lib-ld.m4 serial 9
dnl Copyright (C) 1996-2003, 2009-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Subroutines of libtool.m4,
dnl with replacements s/_*LT_PATH/AC_LIB_PROG/ and s/lt_/acl_/ to avoid
dnl collision with libtool.m4.

dnl From libtool-2.4. Sets the variable with_gnu_ld to yes or no.
AC_DEFUN([AC_LIB_PROG_LD_GNU],
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], [acl_cv_prog_gnu_ld],
[# I'd rather use --version here, but apparently some GNU lds only accept -v.
case `$LD -v 2>&1 </dev/null` in
*GNU* | *'with BFD'*)
  acl_cv_prog_gnu_ld=yes
  ;;
*)
  acl_cv_prog_gnu_ld=no
  ;;
esac])
with_gnu_ld=$acl_cv_prog_gnu_ld
])

dnl From libtool-2.4. Sets the variable LD.
AC_DEFUN([AC_LIB_PROG_LD],
[AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl

AC_ARG_WITH([gnu-ld],
    [AS_HELP_STRING([--with-gnu-ld],
        [assume the C compiler uses GNU ld [default=no]])],
    [test "$withval" = no || with_gnu_ld=yes],
    [with_gnu_ld=no])dnl

# Prepare PATH_SEPARATOR.
# The user is always right.
if test "${PATH_SEPARATOR+set}" != set; then
  # Determine PATH_SEPARATOR by trying to find /bin/sh in a PATH which
  # contains only /bin. Note that ksh looks also at the FPATH variable,
  # so we have to set that as well for the test.
  PATH_SEPARATOR=:
  (PATH='/bin;/bin'; FPATH=$PATH; sh -c :) >/dev/null 2>&1 \
    && { (PATH='/bin:/bin'; FPATH=$PATH; sh -c :) >/dev/null 2>&1 \
           || PATH_SEPARATOR=';'
       }
fi

if test -n "$LD"; then
  AC_MSG_CHECKING([for ld])
elif test "$GCC" = yes; then
  AC_MSG_CHECKING([for ld used by $CC])
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
if test -n "$LD"; then
  # Let the user override the test with a path.
  :
else
  AC_CACHE_VAL([acl_cv_path_LD],
  [
    acl_cv_path_LD= # Final result of this test
    ac_prog=ld # Program to search in $PATH
    if test "$GCC" = yes; then
      # Check if gcc -print-prog-name=ld gives a path.
      case $host in
        *-*-mingw*)
          # gcc leaves a trailing carriage return which upsets mingw
          acl_output=`($CC -print-prog-name=ld) 2>&5 | tr -d '\015'` ;;
        *)
          acl_output=`($CC -print-prog-name=ld) 2>&5` ;;
      esac
      case $acl_output in
        # Accept absolute paths.
        [[\\/]]* | ?:[[\\/]]*)
          re_direlt='/[[^/]][[^/]]*/\.\./'
          # Canonicalize the pathname of ld
          acl_output=`echo "$acl_output" | sed 's%\\\\%/%g'`
          while echo "$acl_output" | grep "$re_direlt" > /dev/null 2>&1; do
            acl_output=`echo $acl_output | sed "s%$re_direlt%/%"`
          done
          # Got the pathname. No search in PATH is needed.
          acl_cv_path_LD="$acl_output"
          ac_prog=
          ;;
        "")
          # If it fails, then pretend we aren't using GCC.
          ;;
        *)
          # If it is relative, then search for the first ld in PATH.
          with_gnu_ld=unknown
          ;;
      esac
    fi
    if test -n "$ac_prog"; then
      # Search for $ac_prog in $PATH.
      acl_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
      for ac_dir in $PATH; do
        IFS="$acl_save_ifs"
        test -z "$ac_dir" && ac_dir=.
        if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
          acl_cv_path_LD="$ac_dir/$ac_prog"
          # Check to see if the program is GNU ld.  I'd rather use --version,
          # but apparently some variants of GNU ld only accept -v.
          # Break only if it was the GNU/non-GNU ld that we prefer.
          case `"$acl_cv_path_LD" -v 2>&1 </dev/null` in
            *GNU* | *'with BFD'*)
              test "$with_gnu_ld" != no && break
              ;;
            *)
              test "$with_gnu_ld" != yes && break
              ;;
          esac
        fi
      done
      IFS="$acl_save_ifs"
    fi
    case $host in
      *-*-aix*)
        AC_COMPILE_IFELSE(
          [AC_LANG_SOURCE(
             [[#if defined __powerpc64__ || defined _ARCH_PPC64
                int ok;
               #else
                error fail
               #endif
             ]])],
          [# The compiler produces 64-bit code. Add option '-b64' so that the
           # linker groks 64-bit object files.
           case "$acl_cv_path_LD " in
             *" -b64 "*) ;;
             *) acl_cv_path_LD="$acl_cv_path_LD -b64" ;;
           esac
          ], [])
        ;;
      sparc64-*-netbsd*)
        AC_COMPILE_IFELSE(
          [AC_LANG_SOURCE(
             [[#if defined __sparcv9 || defined __arch64__
                int ok;
               #else
                error fail
               #endif
             ]])],
          [],
          [# The compiler produces 32-bit code. Add option '-m elf32_sparc'
           # so that the linker groks 32-bit object files.
           case "$acl_cv_path_LD " in
             *" -m elf32_sparc "*) ;;
             *) acl_cv_path_LD="$acl_cv_path_LD -m elf32_sparc" ;;
           esac
          ])
        ;;
    esac
  ])
  LD="$acl_cv_path_LD"
fi
if test -n "$LD"; then
  AC_MSG_RESULT([$LD])
else
  AC_MSG_RESULT([no])
  AC_MSG_ERROR([no acceptable ld found in \$PATH])
fi
AC_LIB_PROG_LD_GNU
])

# lib-link.m4 serial 31
dnl Copyright (C) 2001-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_PREREQ([2.61])

dnl AC_LIB_LINKFLAGS(name [, dependencies]) searches for libname and
dnl the libraries corresponding to explicit and implicit dependencies.
dnl Sets and AC_SUBSTs the LIB${NAME} and LTLIB${NAME} variables and
dnl augments the CPPFLAGS variable.
dnl Sets and AC_SUBSTs the LIB${NAME}_PREFIX variable to nonempty if libname
dnl was found in ${LIB${NAME}_PREFIX}/$acl_libdirstem.
AC_DEFUN([AC_LIB_LINKFLAGS],
[
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])
  pushdef([Name],[m4_translit([$1],[./+-], [____])])
  pushdef([NAME],[m4_translit([$1],[abcdefghijklmnopqrstuvwxyz./+-],
                                   [ABCDEFGHIJKLMNOPQRSTUVWXYZ____])])
  AC_CACHE_CHECK([how to link with lib[]$1], [ac_cv_lib[]Name[]_libs], [
    AC_LIB_LINKFLAGS_BODY([$1], [$2])
    ac_cv_lib[]Name[]_libs="$LIB[]NAME"
    ac_cv_lib[]Name[]_ltlibs="$LTLIB[]NAME"
    ac_cv_lib[]Name[]_cppflags="$INC[]NAME"
    ac_cv_lib[]Name[]_prefix="$LIB[]NAME[]_PREFIX"
  ])
  LIB[]NAME="$ac_cv_lib[]Name[]_libs"
  LTLIB[]NAME="$ac_cv_lib[]Name[]_ltlibs"
  INC[]NAME="$ac_cv_lib[]Name[]_cppflags"
  LIB[]NAME[]_PREFIX="$ac_cv_lib[]Name[]_prefix"
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INC]NAME)
  AC_SUBST([LIB]NAME)
  AC_SUBST([LTLIB]NAME)
  AC_SUBST([LIB]NAME[_PREFIX])
  dnl Also set HAVE_LIB[]NAME so that AC_LIB_HAVE_LINKFLAGS can reuse the
  dnl results of this search when this library appears as a dependency.
  HAVE_LIB[]NAME=yes
  popdef([NAME])
  popdef([Name])
])

dnl AC_LIB_HAVE_LINKFLAGS(name, dependencies, includes, testcode, [missing-message])
dnl searches for libname and the libraries corresponding to explicit and
dnl implicit dependencies, together with the specified include files and
dnl the ability to compile and link the specified testcode. The missing-message
dnl defaults to 'no' and may contain additional hints for the user.
dnl If found, it sets and AC_SUBSTs HAVE_LIB${NAME}=yes and the LIB${NAME}
dnl and LTLIB${NAME} variables and augments the CPPFLAGS variable, and
dnl #defines HAVE_LIB${NAME} to 1. Otherwise, it sets and AC_SUBSTs
dnl HAVE_LIB${NAME}=no and LIB${NAME} and LTLIB${NAME} to empty.
dnl Sets and AC_SUBSTs the LIB${NAME}_PREFIX variable to nonempty if libname
dnl was found in ${LIB${NAME}_PREFIX}/$acl_libdirstem.
AC_DEFUN([AC_LIB_HAVE_LINKFLAGS],
[
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])
  pushdef([Name],[m4_translit([$1],[./+-], [____])])
  pushdef([NAME],[m4_translit([$1],[abcdefghijklmnopqrstuvwxyz./+-],
                                   [ABCDEFGHIJKLMNOPQRSTUVWXYZ____])])

  dnl Search for lib[]Name and define LIB[]NAME, LTLIB[]NAME and INC[]NAME
  dnl accordingly.
  AC_LIB_LINKFLAGS_BODY([$1], [$2])

  dnl Add $INC[]NAME to CPPFLAGS before performing the following checks,
  dnl because if the user has installed lib[]Name and not disabled its use
  dnl via --without-lib[]Name-prefix, he wants to use it.
  ac_save_CPPFLAGS="$CPPFLAGS"
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INC]NAME)

  AC_CACHE_CHECK([for lib[]$1], [ac_cv_lib[]Name], [
    ac_save_LIBS="$LIBS"
    dnl If $LIB[]NAME contains some -l options, add it to the end of LIBS,
    dnl because these -l options might require -L options that are present in
    dnl LIBS. -l options benefit only from the -L options listed before it.
    dnl Otherwise, add it to the front of LIBS, because it may be a static
    dnl library that depends on another static library that is present in LIBS.
    dnl Static libraries benefit only from the static libraries listed after
    dnl it.
    case " $LIB[]NAME" in
      *" -l"*) LIBS="$LIBS $LIB[]NAME" ;;
      *)       LIBS="$LIB[]NAME $LIBS" ;;
    esac
    AC_LINK_IFELSE(
      [AC_LANG_PROGRAM([[$3]], [[$4]])],
      [ac_cv_lib[]Name=yes],
      [ac_cv_lib[]Name='m4_if([$5], [], [no], [[$5]])'])
    LIBS="$ac_save_LIBS"
  ])
  if test "$ac_cv_lib[]Name" = yes; then
    HAVE_LIB[]NAME=yes
    AC_DEFINE([HAVE_LIB]NAME, 1, [Define if you have the lib][$1 library.])
    AC_MSG_CHECKING([how to link with lib[]$1])
    AC_MSG_RESULT([$LIB[]NAME])
  else
    HAVE_LIB[]NAME=no
    dnl If $LIB[]NAME didn't lead to a usable library, we don't need
    dnl $INC[]NAME either.
    CPPFLAGS="$ac_save_CPPFLAGS"
    LIB[]NAME=
    LTLIB[]NAME=
    LIB[]NAME[]_PREFIX=
  fi
  AC_SUBST([HAVE_LIB]NAME)
  AC_SUBST([LIB]NAME)
  AC_SUBST([LTLIB]NAME)
  AC_SUBST([LIB]NAME[_PREFIX])
  popdef([NAME])
  popdef([Name])
])

dnl Determine the platform dependent parameters needed to use rpath:
dnl   acl_libext,
dnl   acl_shlibext,
dnl   acl_libname_spec,
dnl   acl_library_names_spec,
dnl   acl_hardcode_libdir_flag_spec,
dnl   acl_hardcode_libdir_separator,
dnl   acl_hardcode_direct,
dnl   acl_hardcode_minus_L.
AC_DEFUN([AC_LIB_RPATH],
[
  dnl Complain if config.rpath is missing.
  AC_REQUIRE_AUX_FILE([config.rpath])
  AC_REQUIRE([AC_PROG_CC])                dnl we use $CC, $GCC, $LDFLAGS
  AC_REQUIRE([AC_LIB_PROG_LD])            dnl we use $LD, $with_gnu_ld
  AC_REQUIRE([AC_CANONICAL_HOST])         dnl we use $host
  AC_REQUIRE([AC_CONFIG_AUX_DIR_DEFAULT]) dnl we use $ac_aux_dir
  AC_CACHE_CHECK([for shared library run path origin], [acl_cv_rpath], [
    CC="$CC" GCC="$GCC" LDFLAGS="$LDFLAGS" LD="$LD" with_gnu_ld="$with_gnu_ld" \
    ${CONFIG_SHELL-/bin/sh} "$ac_aux_dir/config.rpath" "$host" > conftest.sh
    . ./conftest.sh
    rm -f ./conftest.sh
    acl_cv_rpath=done
  ])
  wl="$acl_cv_wl"
  acl_libext="$acl_cv_libext"
  acl_shlibext="$acl_cv_shlibext"
  acl_libname_spec="$acl_cv_libname_spec"
  acl_library_names_spec="$acl_cv_library_names_spec"
  acl_hardcode_libdir_flag_spec="$acl_cv_hardcode_libdir_flag_spec"
  acl_hardcode_libdir_separator="$acl_cv_hardcode_libdir_separator"
  acl_hardcode_direct="$acl_cv_hardcode_direct"
  acl_hardcode_minus_L="$acl_cv_hardcode_minus_L"
  dnl Determine whether the user wants rpath handling at all.
  AC_ARG_ENABLE([rpath],
    [  --disable-rpath         do not hardcode runtime library paths],
    :, enable_rpath=yes)
])

dnl AC_LIB_FROMPACKAGE(name, package)
dnl declares that libname comes from the given package. The configure file
dnl will then not have a --with-libname-prefix option but a
dnl --with-package-prefix option. Several libraries can come from the same
dnl package. This declaration must occur before an AC_LIB_LINKFLAGS or similar
dnl macro call that searches for libname.
AC_DEFUN([AC_LIB_FROMPACKAGE],
[
  pushdef([NAME],[m4_translit([$1],[abcdefghijklmnopqrstuvwxyz./+-],
                                   [ABCDEFGHIJKLMNOPQRSTUVWXYZ____])])
  define([acl_frompackage_]NAME, [$2])
  popdef([NAME])
  pushdef([PACK],[$2])
  pushdef([PACKUP],[m4_translit(PACK,[abcdefghijklmnopqrstuvwxyz./+-],
                                     [ABCDEFGHIJKLMNOPQRSTUVWXYZ____])])
  define([acl_libsinpackage_]PACKUP,
    m4_ifdef([acl_libsinpackage_]PACKUP, [m4_defn([acl_libsinpackage_]PACKUP)[, ]],)[lib$1])
  popdef([PACKUP])
  popdef([PACK])
])

dnl AC_LIB_LINKFLAGS_BODY(name [, dependencies]) searches for libname and
dnl the libraries corresponding to explicit and implicit dependencies.
dnl Sets the LIB${NAME}, LTLIB${NAME} and INC${NAME} variables.
dnl Also, sets the LIB${NAME}_PREFIX variable to nonempty if libname was found
dnl in ${LIB${NAME}_PREFIX}/$acl_libdirstem.
AC_DEFUN([AC_LIB_LINKFLAGS_BODY],
[
  AC_REQUIRE([AC_LIB_PREPARE_MULTILIB])
  pushdef([NAME],[m4_translit([$1],[abcdefghijklmnopqrstuvwxyz./+-],
                                   [ABCDEFGHIJKLMNOPQRSTUVWXYZ____])])
  pushdef([PACK],[m4_ifdef([acl_frompackage_]NAME, [acl_frompackage_]NAME, lib[$1])])
  pushdef([PACKUP],[m4_translit(PACK,[abcdefghijklmnopqrstuvwxyz./+-],
                                     [ABCDEFGHIJKLMNOPQRSTUVWXYZ____])])
  pushdef([PACKLIBS],[m4_ifdef([acl_frompackage_]NAME, [acl_libsinpackage_]PACKUP, lib[$1])])
  dnl By default, look in $includedir and $libdir.
  use_additional=yes
  AC_LIB_WITH_FINAL_PREFIX([
    eval additional_includedir=\"$includedir\"
    eval additional_libdir=\"$libdir\"
    eval additional_libdir2=\"$exec_prefix/$acl_libdirstem2\"
    eval additional_libdir3=\"$exec_prefix/$acl_libdirstem3\"
  ])
  AC_ARG_WITH(PACK[-prefix],
[[  --with-]]PACK[[-prefix[=DIR]  search for ]PACKLIBS[ in DIR/include and DIR/lib
  --without-]]PACK[[-prefix     don't search for ]PACKLIBS[ in includedir and libdir]],
[
    if test "X$withval" = "Xno"; then
      use_additional=no
    else
      if test "X$withval" = "X"; then
        AC_LIB_WITH_FINAL_PREFIX([
          eval additional_includedir=\"$includedir\"
          eval additional_libdir=\"$libdir\"
          eval additional_libdir2=\"$exec_prefix/$acl_libdirstem2\"
          eval additional_libdir3=\"$exec_prefix/$acl_libdirstem3\"
        ])
      else
        additional_includedir="$withval/include"
        additional_libdir="$withval/$acl_libdirstem"
        additional_libdir2="$withval/$acl_libdirstem2"
        additional_libdir3="$withval/$acl_libdirstem3"
      fi
    fi
])
  if test "X$additional_libdir2" = "X$additional_libdir"; then
    additional_libdir2=
  fi
  if test "X$additional_libdir3" = "X$additional_libdir"; then
    additional_libdir3=
  fi
  dnl Search the library and its dependencies in $additional_libdir and
  dnl $LDFLAGS. Using breadth-first-seach.
  LIB[]NAME=
  LTLIB[]NAME=
  INC[]NAME=
  LIB[]NAME[]_PREFIX=
  dnl HAVE_LIB${NAME} is an indicator that LIB${NAME}, LTLIB${NAME} have been
  dnl computed. So it has to be reset here.
  HAVE_LIB[]NAME=
  rpathdirs=
  ltrpathdirs=
  names_already_handled=
  names_next_round='$1 $2'
  while test -n "$names_next_round"; do
    names_this_round="$names_next_round"
    names_next_round=
    for name in $names_this_round; do
      already_handled=
      for n in $names_already_handled; do
        if test "$n" = "$name"; then
          already_handled=yes
          break
        fi
      done
      if test -z "$already_handled"; then
        names_already_handled="$names_already_handled $name"
        dnl See if it was already located by an earlier AC_LIB_LINKFLAGS
        dnl or AC_LIB_HAVE_LINKFLAGS call.
        uppername=`echo "$name" | sed -e 'y|abcdefghijklmnopqrstuvwxyz./+-|ABCDEFGHIJKLMNOPQRSTUVWXYZ____|'`
        eval value=\"\$HAVE_LIB$uppername\"
        if test -n "$value"; then
          if test "$value" = yes; then
            eval value=\"\$LIB$uppername\"
            test -z "$value" || LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$value"
            eval value=\"\$LTLIB$uppername\"
            test -z "$value" || LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }$value"
          else
            dnl An earlier call to AC_LIB_HAVE_LINKFLAGS has determined
            dnl that this library doesn't exist. So just drop it.
            :
          fi
        else
          dnl Search the library lib$name in $additional_libdir and $LDFLAGS
          dnl and the already constructed $LIBNAME/$LTLIBNAME.
          found_dir=
          found_la=
          found_so=
          found_a=
          eval libname=\"$acl_libname_spec\"    # typically: libname=lib$name
          if test -n "$acl_shlibext"; then
            shrext=".$acl_shlibext"             # typically: shrext=.so
          else
            shrext=
          fi
          if test $use_additional = yes; then
            for additional_libdir_variable in additional_libdir additional_libdir2 additional_libdir3; do
              if test "X$found_dir" = "X"; then
                eval dir=\$$additional_libdir_variable
                if test -n "$dir"; then
                  dnl The same code as in the loop below:
                  dnl First look for a shared library.
                  if test -n "$acl_shlibext"; then
                    if test -f "$dir/$libname$shrext" && acl_is_expected_elfclass < "$dir/$libname$shrext"; then
                      found_dir="$dir"
                      found_so="$dir/$libname$shrext"
                    else
                      if test "$acl_library_names_spec" = '$libname$shrext$versuffix'; then
                        ver=`(cd "$dir" && \
                              for f in "$libname$shrext".*; do echo "$f"; done \
                              | sed -e "s,^$libname$shrext\\\\.,," \
                              | sort -t '.' -n -r -k1,1 -k2,2 -k3,3 -k4,4 -k5,5 \
                              | sed 1q ) 2>/dev/null`
                        if test -n "$ver" && test -f "$dir/$libname$shrext.$ver" && acl_is_expected_elfclass < "$dir/$libname$shrext.$ver"; then
                          found_dir="$dir"
                          found_so="$dir/$libname$shrext.$ver"
                        fi
                      else
                        eval library_names=\"$acl_library_names_spec\"
                        for f in $library_names; do
                          if test -f "$dir/$f" && acl_is_expected_elfclass < "$dir/$f"; then
                            found_dir="$dir"
                            found_so="$dir/$f"
                            break
                          fi
                        done
                      fi
                    fi
                  fi
                  dnl Then look for a static library.
                  if test "X$found_dir" = "X"; then
                    if test -f "$dir/$libname.$acl_libext" && ${AR-ar} -p "$dir/$libname.$acl_libext" | acl_is_expected_elfclass; then
                      found_dir="$dir"
                      found_a="$dir/$libname.$acl_libext"
                    fi
                  fi
                  if test "X$found_dir" != "X"; then
                    if test -f "$dir/$libname.la"; then
                      found_la="$dir/$libname.la"
                    fi
                  fi
                fi
              fi
            done
          fi
          if test "X$found_dir" = "X"; then
            for x in $LDFLAGS $LTLIB[]NAME; do
              AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
              case "$x" in
                -L*)
                  dir=`echo "X$x" | sed -e 's/^X-L//'`
                  dnl First look for a shared library.
                  if test -n "$acl_shlibext"; then
                    if test -f "$dir/$libname$shrext" && acl_is_expected_elfclass < "$dir/$libname$shrext"; then
                      found_dir="$dir"
                      found_so="$dir/$libname$shrext"
                    else
                      if test "$acl_library_names_spec" = '$libname$shrext$versuffix'; then
                        ver=`(cd "$dir" && \
                              for f in "$libname$shrext".*; do echo "$f"; done \
                              | sed -e "s,^$libname$shrext\\\\.,," \
                              | sort -t '.' -n -r -k1,1 -k2,2 -k3,3 -k4,4 -k5,5 \
                              | sed 1q ) 2>/dev/null`
                        if test -n "$ver" && test -f "$dir/$libname$shrext.$ver" && acl_is_expected_elfclass < "$dir/$libname$shrext.$ver"; then
                          found_dir="$dir"
                          found_so="$dir/$libname$shrext.$ver"
                        fi
                      else
                        eval library_names=\"$acl_library_names_spec\"
                        for f in $library_names; do
                          if test -f "$dir/$f" && acl_is_expected_elfclass < "$dir/$f"; then
                            found_dir="$dir"
                            found_so="$dir/$f"
                            break
                          fi
                        done
                      fi
                    fi
                  fi
                  dnl Then look for a static library.
                  if test "X$found_dir" = "X"; then
                    if test -f "$dir/$libname.$acl_libext" && ${AR-ar} -p "$dir/$libname.$acl_libext" | acl_is_expected_elfclass; then
                      found_dir="$dir"
                      found_a="$dir/$libname.$acl_libext"
                    fi
                  fi
                  if test "X$found_dir" != "X"; then
                    if test -f "$dir/$libname.la"; then
                      found_la="$dir/$libname.la"
                    fi
                  fi
                  ;;
              esac
              if test "X$found_dir" != "X"; then
                break
              fi
            done
          fi
          if test "X$found_dir" != "X"; then
            dnl Found the library.
            LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }-L$found_dir -l$name"
            if test "X$found_so" != "X"; then
              dnl Linking with a shared library. We attempt to hardcode its
              dnl directory into the executable's runpath, unless it's the
              dnl standard /usr/lib.
              if test "$enable_rpath" = no \
                 || test "X$found_dir" = "X/usr/$acl_libdirstem" \
                 || test "X$found_dir" = "X/usr/$acl_libdirstem2" \
                 || test "X$found_dir" = "X/usr/$acl_libdirstem3"; then
                dnl No hardcoding is needed.
                LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_so"
              else
                dnl Use an explicit option to hardcode DIR into the resulting
                dnl binary.
                dnl Potentially add DIR to ltrpathdirs.
                dnl The ltrpathdirs will be appended to $LTLIBNAME at the end.
                haveit=
                for x in $ltrpathdirs; do
                  if test "X$x" = "X$found_dir"; then
                    haveit=yes
                    break
                  fi
                done
                if test -z "$haveit"; then
                  ltrpathdirs="$ltrpathdirs $found_dir"
                fi
                dnl The hardcoding into $LIBNAME is system dependent.
                if test "$acl_hardcode_direct" = yes; then
                  dnl Using DIR/libNAME.so during linking hardcodes DIR into the
                  dnl resulting binary.
                  LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_so"
                else
                  if test -n "$acl_hardcode_libdir_flag_spec" && test "$acl_hardcode_minus_L" = no; then
                    dnl Use an explicit option to hardcode DIR into the resulting
                    dnl binary.
                    LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_so"
                    dnl Potentially add DIR to rpathdirs.
                    dnl The rpathdirs will be appended to $LIBNAME at the end.
                    haveit=
                    for x in $rpathdirs; do
                      if test "X$x" = "X$found_dir"; then
                        haveit=yes
                        break
                      fi
                    done
                    if test -z "$haveit"; then
                      rpathdirs="$rpathdirs $found_dir"
                    fi
                  else
                    dnl Rely on "-L$found_dir".
                    dnl But don't add it if it's already contained in the LDFLAGS
                    dnl or the already constructed $LIBNAME
                    haveit=
                    for x in $LDFLAGS $LIB[]NAME; do
                      AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
                      if test "X$x" = "X-L$found_dir"; then
                        haveit=yes
                        break
                      fi
                    done
                    if test -z "$haveit"; then
                      LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-L$found_dir"
                    fi
                    if test "$acl_hardcode_minus_L" != no; then
                      dnl FIXME: Not sure whether we should use
                      dnl "-L$found_dir -l$name" or "-L$found_dir $found_so"
                      dnl here.
                      LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_so"
                    else
                      dnl We cannot use $acl_hardcode_runpath_var and LD_RUN_PATH
                      dnl here, because this doesn't fit in flags passed to the
                      dnl compiler. So give up. No hardcoding. This affects only
                      dnl very old systems.
                      dnl FIXME: Not sure whether we should use
                      dnl "-L$found_dir -l$name" or "-L$found_dir $found_so"
                      dnl here.
                      LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-l$name"
                    fi
                  fi
                fi
              fi
            else
              if test "X$found_a" != "X"; then
                dnl Linking with a static library.
                LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_a"
              else
                dnl We shouldn't come here, but anyway it's good to have a
                dnl fallback.
                LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-L$found_dir -l$name"
              fi
            fi
            dnl Assume the include files are nearby.
            additional_includedir=
            case "$found_dir" in
              */$acl_libdirstem | */$acl_libdirstem/)
                basedir=`echo "X$found_dir" | sed -e 's,^X,,' -e "s,/$acl_libdirstem/"'*$,,'`
                if test "$name" = '$1'; then
                  LIB[]NAME[]_PREFIX="$basedir"
                fi
                additional_includedir="$basedir/include"
                ;;
              */$acl_libdirstem2 | */$acl_libdirstem2/)
                basedir=`echo "X$found_dir" | sed -e 's,^X,,' -e "s,/$acl_libdirstem2/"'*$,,'`
                if test "$name" = '$1'; then
                  LIB[]NAME[]_PREFIX="$basedir"
                fi
                additional_includedir="$basedir/include"
                ;;
              */$acl_libdirstem3 | */$acl_libdirstem3/)
                basedir=`echo "X$found_dir" | sed -e 's,^X,,' -e "s,/$acl_libdirstem3/"'*$,,'`
                if test "$name" = '$1'; then
                  LIB[]NAME[]_PREFIX="$basedir"
                fi
                additional_includedir="$basedir/include"
                ;;
            esac
            if test "X$additional_includedir" != "X"; then
              dnl Potentially add $additional_includedir to $INCNAME.
              dnl But don't add it
              dnl   1. if it's the standard /usr/include,
              dnl   2. if it's /usr/local/include and we are using GCC on Linux,
              dnl   3. if it's already present in $CPPFLAGS or the already
              dnl      constructed $INCNAME,
              dnl   4. if it doesn't exist as a directory.
              if test "X$additional_includedir" != "X/usr/include"; then
                haveit=
                if test "X$additional_includedir" = "X/usr/local/include"; then
                  if test -n "$GCC"; then
                    case $host_os in
                      linux* | gnu* | k*bsd*-gnu) haveit=yes;;
                    esac
                  fi
                fi
                if test -z "$haveit"; then
                  for x in $CPPFLAGS $INC[]NAME; do
                    AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
                    if test "X$x" = "X-I$additional_includedir"; then
                      haveit=yes
                      break
                    fi
                  done
                  if test -z "$haveit"; then
                    if test -d "$additional_includedir"; then
                      dnl Really add $additional_includedir to $INCNAME.
                      INC[]NAME="${INC[]NAME}${INC[]NAME:+ }-I$additional_includedir"
                    fi
                  fi
                fi
              fi
            fi
            dnl Look for dependencies.
            if test -n "$found_la"; then
              dnl Read the .la file. It defines the variables
              dnl dlname, library_names, old_library, dependency_libs, current,
              dnl age, revision, installed, dlopen, dlpreopen, libdir.
              save_libdir="$libdir"
              case "$found_la" in
                */* | *\\*) . "$found_la" ;;
                *) . "./$found_la" ;;
              esac
              libdir="$save_libdir"
              dnl We use only dependency_libs.
              for dep in $dependency_libs; do
                case "$dep" in
                  -L*)
                    dependency_libdir=`echo "X$dep" | sed -e 's/^X-L//'`
                    dnl Potentially add $dependency_libdir to $LIBNAME and $LTLIBNAME.
                    dnl But don't add it
                    dnl   1. if it's the standard /usr/lib,
                    dnl   2. if it's /usr/local/lib and we are using GCC on Linux,
                    dnl   3. if it's already present in $LDFLAGS or the already
                    dnl      constructed $LIBNAME,
                    dnl   4. if it doesn't exist as a directory.
                    if test "X$dependency_libdir" != "X/usr/$acl_libdirstem" \
                       && test "X$dependency_libdir" != "X/usr/$acl_libdirstem2" \
                       && test "X$dependency_libdir" != "X/usr/$acl_libdirstem3"; then
                      haveit=
                      if test "X$dependency_libdir" = "X/usr/local/$acl_libdirstem" \
                         || test "X$dependency_libdir" = "X/usr/local/$acl_libdirstem2" \
                         || test "X$dependency_libdir" = "X/usr/local/$acl_libdirstem3"; then
                        if test -n "$GCC"; then
                          case $host_os in
                            linux* | gnu* | k*bsd*-gnu) haveit=yes;;
                          esac
                        fi
                      fi
                      if test -z "$haveit"; then
                        haveit=
                        for x in $LDFLAGS $LIB[]NAME; do
                          AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
                          if test "X$x" = "X-L$dependency_libdir"; then
                            haveit=yes
                            break
                          fi
                        done
                        if test -z "$haveit"; then
                          if test -d "$dependency_libdir"; then
                            dnl Really add $dependency_libdir to $LIBNAME.
                            LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-L$dependency_libdir"
                          fi
                        fi
                        haveit=
                        for x in $LDFLAGS $LTLIB[]NAME; do
                          AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
                          if test "X$x" = "X-L$dependency_libdir"; then
                            haveit=yes
                            break
                          fi
                        done
                        if test -z "$haveit"; then
                          if test -d "$dependency_libdir"; then
                            dnl Really add $dependency_libdir to $LTLIBNAME.
                            LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }-L$dependency_libdir"
                          fi
                        fi
                      fi
                    fi
                    ;;
                  -R*)
                    dir=`echo "X$dep" | sed -e 's/^X-R//'`
                    if test "$enable_rpath" != no; then
                      dnl Potentially add DIR to rpathdirs.
                      dnl The rpathdirs will be appended to $LIBNAME at the end.
                      haveit=
                      for x in $rpathdirs; do
                        if test "X$x" = "X$dir"; then
                          haveit=yes
                          break
                        fi
                      done
                      if test -z "$haveit"; then
                        rpathdirs="$rpathdirs $dir"
                      fi
                      dnl Potentially add DIR to ltrpathdirs.
                      dnl The ltrpathdirs will be appended to $LTLIBNAME at the end.
                      haveit=
                      for x in $ltrpathdirs; do
                        if test "X$x" = "X$dir"; then
                          haveit=yes
                          break
                        fi
                      done
                      if test -z "$haveit"; then
                        ltrpathdirs="$ltrpathdirs $dir"
                      fi
                    fi
                    ;;
                  -l*)
                    dnl Handle this in the next round.
                    names_next_round="$names_next_round "`echo "X$dep" | sed -e 's/^X-l//'`
                    ;;
                  *.la)
                    dnl Handle this in the next round. Throw away the .la's
                    dnl directory; it is already contained in a preceding -L
                    dnl option.
                    names_next_round="$names_next_round "`echo "X$dep" | sed -e 's,^X.*/,,' -e 's,^lib,,' -e 's,\.la$,,'`
                    ;;
                  *)
                    dnl Most likely an immediate library name.
                    LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$dep"
                    LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }$dep"
                    ;;
                esac
              done
            fi
          else
            dnl Didn't find the library; assume it is in the system directories
            dnl known to the linker and runtime loader. (All the system
            dnl directories known to the linker should also be known to the
            dnl runtime loader, otherwise the system is severely misconfigured.)
            LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-l$name"
            LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }-l$name"
          fi
        fi
      fi
    done
  done
  if test "X$rpathdirs" != "X"; then
    if test -n "$acl_hardcode_libdir_separator"; then
      dnl Weird platform: only the last -rpath option counts, the user must
      dnl pass all path elements in one option. We can arrange that for a
      dnl single library, but not when more than one $LIBNAMEs are used.
      alldirs=
      for found_dir in $rpathdirs; do
        alldirs="${alldirs}${alldirs:+$acl_hardcode_libdir_separator}$found_dir"
      done
      dnl Note: acl_hardcode_libdir_flag_spec uses $libdir and $wl.
      acl_save_libdir="$libdir"
      libdir="$alldirs"
      eval flag=\"$acl_hardcode_libdir_flag_spec\"
      libdir="$acl_save_libdir"
      LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$flag"
    else
      dnl The -rpath options are cumulative.
      for found_dir in $rpathdirs; do
        acl_save_libdir="$libdir"
        libdir="$found_dir"
        eval flag=\"$acl_hardcode_libdir_flag_spec\"
        libdir="$acl_save_libdir"
        LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$flag"
      done
    fi
  fi
  if test "X$ltrpathdirs" != "X"; then
    dnl When using libtool, the option that works for both libraries and
    dnl executables is -R. The -R options are cumulative.
    for found_dir in $ltrpathdirs; do
      LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }-R$found_dir"
    done
  fi
  popdef([PACKLIBS])
  popdef([PACKUP])
  popdef([PACK])
  popdef([NAME])
])

dnl AC_LIB_APPENDTOVAR(VAR, CONTENTS) appends the elements of CONTENTS to VAR,
dnl unless already present in VAR.
dnl Works only for CPPFLAGS, not for LIB* variables because that sometimes
dnl contains two or three consecutive elements that belong together.
AC_DEFUN([AC_LIB_APPENDTOVAR],
[
  for element in [$2]; do
    haveit=
    for x in $[$1]; do
      AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
      if test "X$x" = "X$element"; then
        haveit=yes
        break
      fi
    done
    if test -z "$haveit"; then
      [$1]="${[$1]}${[$1]:+ }$element"
    fi
  done
])

dnl For those cases where a variable contains several -L and -l options
dnl referring to unknown libraries and directories, this macro determines the
dnl necessary additional linker options for the runtime path.
dnl AC_LIB_LINKFLAGS_FROM_LIBS([LDADDVAR], [LIBSVALUE], [USE-LIBTOOL])
dnl sets LDADDVAR to linker options needed together with LIBSVALUE.
dnl If USE-LIBTOOL evaluates to non-empty, linking with libtool is assumed,
dnl otherwise linking without libtool is assumed.
AC_DEFUN([AC_LIB_LINKFLAGS_FROM_LIBS],
[
  AC_REQUIRE([AC_LIB_RPATH])
  AC_REQUIRE([AC_LIB_PREPARE_MULTILIB])
  $1=
  if test "$enable_rpath" != no; then
    if test -n "$acl_hardcode_libdir_flag_spec" && test "$acl_hardcode_minus_L" = no; then
      dnl Use an explicit option to hardcode directories into the resulting
      dnl binary.
      rpathdirs=
      next=
      for opt in $2; do
        if test -n "$next"; then
          dir="$next"
          dnl No need to hardcode the standard /usr/lib.
          if test "X$dir" != "X/usr/$acl_libdirstem" \
             && test "X$dir" != "X/usr/$acl_libdirstem2" \
             && test "X$dir" != "X/usr/$acl_libdirstem3"; then
            rpathdirs="$rpathdirs $dir"
          fi
          next=
        else
          case $opt in
            -L) next=yes ;;
            -L*) dir=`echo "X$opt" | sed -e 's,^X-L,,'`
                 dnl No need to hardcode the standard /usr/lib.
                 if test "X$dir" != "X/usr/$acl_libdirstem" \
                    && test "X$dir" != "X/usr/$acl_libdirstem2" \
                    && test "X$dir" != "X/usr/$acl_libdirstem3"; then
                   rpathdirs="$rpathdirs $dir"
                 fi
                 next= ;;
            *) next= ;;
          esac
        fi
      done
      if test "X$rpathdirs" != "X"; then
        if test -n ""$3""; then
          dnl libtool is used for linking. Use -R options.
          for dir in $rpathdirs; do
            $1="${$1}${$1:+ }-R$dir"
          done
        else
          dnl The linker is used for linking directly.
          if test -n "$acl_hardcode_libdir_separator"; then
            dnl Weird platform: only the last -rpath option counts, the user
            dnl must pass all path elements in one option.
            alldirs=
            for dir in $rpathdirs; do
              alldirs="${alldirs}${alldirs:+$acl_hardcode_libdir_separator}$dir"
            done
            acl_save_libdir="$libdir"
            libdir="$alldirs"
            eval flag=\"$acl_hardcode_libdir_flag_spec\"
            libdir="$acl_save_libdir"
            $1="$flag"
          else
            dnl The -rpath options are cumulative.
            for dir in $rpathdirs; do
              acl_save_libdir="$libdir"
              libdir="$dir"
              eval flag=\"$acl_hardcode_libdir_flag_spec\"
              libdir="$acl_save_libdir"
              $1="${$1}${$1:+ }$flag"
            done
          fi
        fi
      fi
    fi
  fi
  AC_SUBST([$1])
])

# lib-prefix.m4 serial 17
dnl Copyright (C) 2001-2005, 2008-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

dnl AC_LIB_PREFIX adds to the CPPFLAGS and LDFLAGS the flags that are needed
dnl to access previously installed libraries. The basic assumption is that
dnl a user will want packages to use other packages he previously installed
dnl with the same --prefix option.
dnl This macro is not needed if only AC_LIB_LINKFLAGS is used to locate
dnl libraries, but is otherwise very convenient.
AC_DEFUN([AC_LIB_PREFIX],
[
  AC_BEFORE([$0], [AC_LIB_LINKFLAGS])
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([AC_LIB_PREPARE_MULTILIB])
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  dnl By default, look in $includedir and $libdir.
  use_additional=yes
  AC_LIB_WITH_FINAL_PREFIX([
    eval additional_includedir=\"$includedir\"
    eval additional_libdir=\"$libdir\"
  ])
  AC_ARG_WITH([lib-prefix],
[[  --with-lib-prefix[=DIR] search for libraries in DIR/include and DIR/lib
  --without-lib-prefix    don't search for libraries in includedir and libdir]],
[
    if test "X$withval" = "Xno"; then
      use_additional=no
    else
      if test "X$withval" = "X"; then
        AC_LIB_WITH_FINAL_PREFIX([
          eval additional_includedir=\"$includedir\"
          eval additional_libdir=\"$libdir\"
        ])
      else
        additional_includedir="$withval/include"
        additional_libdir="$withval/$acl_libdirstem"
      fi
    fi
])
  if test $use_additional = yes; then
    dnl Potentially add $additional_includedir to $CPPFLAGS.
    dnl But don't add it
    dnl   1. if it's the standard /usr/include,
    dnl   2. if it's already present in $CPPFLAGS,
    dnl   3. if it's /usr/local/include and we are using GCC on Linux,
    dnl   4. if it doesn't exist as a directory.
    if test "X$additional_includedir" != "X/usr/include"; then
      haveit=
      for x in $CPPFLAGS; do
        AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
        if test "X$x" = "X-I$additional_includedir"; then
          haveit=yes
          break
        fi
      done
      if test -z "$haveit"; then
        if test "X$additional_includedir" = "X/usr/local/include"; then
          if test -n "$GCC"; then
            case $host_os in
              linux* | gnu* | k*bsd*-gnu) haveit=yes;;
            esac
          fi
        fi
        if test -z "$haveit"; then
          if test -d "$additional_includedir"; then
            dnl Really add $additional_includedir to $CPPFLAGS.
            CPPFLAGS="${CPPFLAGS}${CPPFLAGS:+ }-I$additional_includedir"
          fi
        fi
      fi
    fi
    dnl Potentially add $additional_libdir to $LDFLAGS.
    dnl But don't add it
    dnl   1. if it's the standard /usr/lib,
    dnl   2. if it's already present in $LDFLAGS,
    dnl   3. if it's /usr/local/lib and we are using GCC on Linux,
    dnl   4. if it doesn't exist as a directory.
    if test "X$additional_libdir" != "X/usr/$acl_libdirstem"; then
      haveit=
      for x in $LDFLAGS; do
        AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
        if test "X$x" = "X-L$additional_libdir"; then
          haveit=yes
          break
        fi
      done
      if test -z "$haveit"; then
        if test "X$additional_libdir" = "X/usr/local/$acl_libdirstem"; then
          if test -n "$GCC"; then
            case $host_os in
              linux*) haveit=yes;;
            esac
          fi
        fi
        if test -z "$haveit"; then
          if test -d "$additional_libdir"; then
            dnl Really add $additional_libdir to $LDFLAGS.
            LDFLAGS="${LDFLAGS}${LDFLAGS:+ }-L$additional_libdir"
          fi
        fi
      fi
    fi
  fi
])

dnl AC_LIB_PREPARE_PREFIX creates variables acl_final_prefix,
dnl acl_final_exec_prefix, containing the values to which $prefix and
dnl $exec_prefix will expand at the end of the configure script.
AC_DEFUN([AC_LIB_PREPARE_PREFIX],
[
  dnl Unfortunately, prefix and exec_prefix get only finally determined
  dnl at the end of configure.
  if test "X$prefix" = "XNONE"; then
    acl_final_prefix="$ac_default_prefix"
  else
    acl_final_prefix="$prefix"
  fi
  if test "X$exec_prefix" = "XNONE"; then
    acl_final_exec_prefix='${prefix}'
  else
    acl_final_exec_prefix="$exec_prefix"
  fi
  acl_save_prefix="$prefix"
  prefix="$acl_final_prefix"
  eval acl_final_exec_prefix=\"$acl_final_exec_prefix\"
  prefix="$acl_save_prefix"
])

dnl AC_LIB_WITH_FINAL_PREFIX([statement]) evaluates statement, with the
dnl variables prefix and exec_prefix bound to the values they will have
dnl at the end of the configure script.
AC_DEFUN([AC_LIB_WITH_FINAL_PREFIX],
[
  acl_save_prefix="$prefix"
  prefix="$acl_final_prefix"
  acl_save_exec_prefix="$exec_prefix"
  exec_prefix="$acl_final_exec_prefix"
  $1
  exec_prefix="$acl_save_exec_prefix"
  prefix="$acl_save_prefix"
])

dnl AC_LIB_PREPARE_MULTILIB creates
dnl - a function acl_is_expected_elfclass, that tests whether standard input
dn;   has a 32-bit or 64-bit ELF header, depending on the host CPU ABI,
dnl - 3 variables acl_libdirstem, acl_libdirstem2, acl_libdirstem3, containing
dnl   the basename of the libdir to try in turn, either "lib" or "lib64" or
dnl   "lib/64" or "lib32" or "lib/sparcv9" or "lib/amd64" or similar.
AC_DEFUN([AC_LIB_PREPARE_MULTILIB],
[
  dnl There is no formal standard regarding lib, lib32, and lib64.
  dnl On most glibc systems, the current practice is that on a system supporting
  dnl 32-bit and 64-bit instruction sets or ABIs, 64-bit libraries go under
  dnl $prefix/lib64 and 32-bit libraries go under $prefix/lib. However, on
  dnl Arch Linux based distributions, it's the opposite: 32-bit libraries go
  dnl under $prefix/lib32 and 64-bit libraries go under $prefix/lib.
  dnl We determine the compiler's default mode by looking at the compiler's
  dnl library search path. If at least one of its elements ends in /lib64 or
  dnl points to a directory whose absolute pathname ends in /lib64, we use that
  dnl for 64-bit ABIs. Similarly for 32-bit ABIs. Otherwise we use the default,
  dnl namely "lib".
  dnl On Solaris systems, the current practice is that on a system supporting
  dnl 32-bit and 64-bit instruction sets or ABIs, 64-bit libraries go under
  dnl $prefix/lib/64 (which is a symlink to either $prefix/lib/sparcv9 or
  dnl $prefix/lib/amd64) and 32-bit libraries go under $prefix/lib.
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_HOST_CPU_C_ABI_32BIT])

  AC_CACHE_CHECK([for ELF binary format], [gl_cv_elf],
    [AC_EGREP_CPP([Extensible Linking Format],
       [#ifdef __ELF__
        Extensible Linking Format
        #endif
       ],
       [gl_cv_elf=yes],
       [gl_cv_elf=no])
     ])
  if test $gl_cv_elf; then
    # Extract the ELF class of a file (5th byte) in decimal.
    # Cf. https://en.wikipedia.org/wiki/Executable_and_Linkable_Format#File_header
    if od -A x < /dev/null >/dev/null 2>/dev/null; then
      # Use POSIX od.
      func_elfclass ()
      {
        od -A n -t d1 -j 4 -N 1
      }
    else
      # Use BSD hexdump.
      func_elfclass ()
      {
        dd bs=1 count=1 skip=4 2>/dev/null | hexdump -e '1/1 "%3d "'
        echo
      }
    fi
changequote(,)dnl
    case $HOST_CPU_C_ABI_32BIT in
      yes)
        # 32-bit ABI.
        acl_is_expected_elfclass ()
        {
          test "`func_elfclass | sed -e 's/[ 	]//g'`" = 1
        }
        ;;
      no)
        # 64-bit ABI.
        acl_is_expected_elfclass ()
        {
          test "`func_elfclass | sed -e 's/[ 	]//g'`" = 2
        }
        ;;
      *)
        # Unknown.
        acl_is_expected_elfclass ()
        {
          :
        }
        ;;
    esac
changequote([,])dnl
  else
    acl_is_expected_elfclass ()
    {
      :
    }
  fi

  dnl Allow the user to override the result by setting acl_cv_libdirstems.
  AC_CACHE_CHECK([for the common suffixes of directories in the library search path],
    [acl_cv_libdirstems],
    [dnl Try 'lib' first, because that's the default for libdir in GNU, see
     dnl <https://www.gnu.org/prep/standards/html_node/Directory-Variables.html>.
     acl_libdirstem=lib
     acl_libdirstem2=
     acl_libdirstem3=
     case "$host_os" in
       solaris*)
         dnl See Solaris 10 Software Developer Collection > Solaris 64-bit Developer's Guide > The Development Environment
         dnl <https://docs.oracle.com/cd/E19253-01/816-5138/dev-env/index.html>.
         dnl "Portable Makefiles should refer to any library directories using the 64 symbolic link."
         dnl But we want to recognize the sparcv9 or amd64 subdirectory also if the
         dnl symlink is missing, so we set acl_libdirstem2 too.
         if test $HOST_CPU_C_ABI_32BIT = no; then
           acl_libdirstem2=lib/64
           case "$host_cpu" in
             sparc*)        acl_libdirstem3=lib/sparcv9 ;;
             i*86 | x86_64) acl_libdirstem3=lib/amd64 ;;
           esac
         fi
         ;;
       *)
         dnl If $CC generates code for a 32-bit ABI, the libraries are
         dnl surely under $prefix/lib or $prefix/lib32, not $prefix/lib64.
         dnl Similarly, if $CC generates code for a 64-bit ABI, the libraries
         dnl are surely under $prefix/lib or $prefix/lib64, not $prefix/lib32.
         dnl Find the compiler's search path. However, non-system compilers
         dnl sometimes have odd library search paths. But we can't simply invoke
         dnl '/usr/bin/gcc -print-search-dirs' because that would not take into
         dnl account the -m32/-m31 or -m64 options from the $CC or $CFLAGS.
         searchpath=`(LC_ALL=C $CC $CPPFLAGS $CFLAGS -print-search-dirs) 2>/dev/null \
                     | sed -n -e 's,^libraries: ,,p' | sed -e 's,^=,,'`
         if test $HOST_CPU_C_ABI_32BIT != no; then
           # 32-bit or unknown ABI.
           if test -d /usr/lib32; then
             acl_libdirstem2=lib32
           fi
         fi
         if test $HOST_CPU_C_ABI_32BIT != yes; then
           # 64-bit or unknown ABI.
           if test -d /usr/lib64; then
             acl_libdirstem3=lib64
           fi
         fi
         if test -n "$searchpath"; then
           acl_save_IFS="${IFS= 	}"; IFS=":"
           for searchdir in $searchpath; do
             if test -d "$searchdir"; then
               case "$searchdir" in
                 */lib32/ | */lib32 ) acl_libdirstem2=lib32 ;;
                 */lib64/ | */lib64 ) acl_libdirstem3=lib64 ;;
                 */../ | */.. )
                   # Better ignore directories of this form. They are misleading.
                   ;;
                 *) searchdir=`cd "$searchdir" && pwd`
                    case "$searchdir" in
                      */lib32 ) acl_libdirstem2=lib32 ;;
                      */lib64 ) acl_libdirstem3=lib64 ;;
                    esac ;;
               esac
             fi
           done
           IFS="$acl_save_IFS"
           if test $HOST_CPU_C_ABI_32BIT = yes; then
             # 32-bit ABI.
             acl_libdirstem3=
           fi
           if test $HOST_CPU_C_ABI_32BIT = no; then
             # 64-bit ABI.
             acl_libdirstem2=
           fi
         fi
         ;;
     esac
     test -n "$acl_libdirstem2" || acl_libdirstem2="$acl_libdirstem"
     test -n "$acl_libdirstem3" || acl_libdirstem3="$acl_libdirstem"
     acl_cv_libdirstems="$acl_libdirstem,$acl_libdirstem2,$acl_libdirstem3"
    ])
  dnl Decompose acl_cv_libdirstems into acl_libdirstem, acl_libdirstem2, and
  dnl acl_libdirstem3.
changequote(,)dnl
  acl_libdirstem=`echo "$acl_cv_libdirstems" | sed -e 's/,.*//'`
  acl_libdirstem2=`echo "$acl_cv_libdirstems" | sed -e 's/^[^,]*,//' -e 's/,.*//'`
  acl_libdirstem3=`echo "$acl_cv_libdirstems" | sed -e 's/^[^,]*,[^,]*,//' -e 's/,.*//'`
changequote([,])dnl
])

# pkg.m4 - Macros to locate and use pkg-config.   -*- Autoconf -*-
# serial 12 (pkg-config-0.29.2)

dnl Copyright © 2004 Scott James Remnant <scott@netsplit.com>.
dnl Copyright © 2012-2015 Dan Nicholson <dbn.lists@gmail.com>
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA.
dnl
dnl As a special exception to the GNU General Public License, if you
dnl distribute this file as part of a program that contains a
dnl configuration script generated by Autoconf, you may include it under
dnl the same distribution terms that you use for the rest of that
dnl program.

dnl PKG_PREREQ(MIN-VERSION)
dnl -----------------------
dnl Since: 0.29
dnl
dnl Verify that the version of the pkg-config macros are at least
dnl MIN-VERSION. Unlike PKG_PROG_PKG_CONFIG, which checks the user's
dnl installed version of pkg-config, this checks the developer's version
dnl of pkg.m4 when generating configure.
dnl
dnl To ensure that this macro is defined, also add:
dnl m4_ifndef([PKG_PREREQ],
dnl     [m4_fatal([must install pkg-config 0.29 or later before running autoconf/autogen])])
dnl
dnl See the "Since" comment for each macro you use to see what version
dnl of the macros you require.
m4_defun([PKG_PREREQ],
[m4_define([PKG_MACROS_VERSION], [0.29.2])
m4_if(m4_version_compare(PKG_MACROS_VERSION, [$1]), -1,
    [m4_fatal([pkg.m4 version $1 or higher is required but ]PKG_MACROS_VERSION[ found])])
])dnl PKG_PREREQ

dnl PKG_PROG_PKG_CONFIG([MIN-VERSION])
dnl ----------------------------------
dnl Since: 0.16
dnl
dnl Search for the pkg-config tool and set the PKG_CONFIG variable to
dnl first found in the path. Checks that the version of pkg-config found
dnl is at least MIN-VERSION. If MIN-VERSION is not specified, 0.9.0 is
dnl used since that's the first version where most current features of
dnl pkg-config existed.
AC_DEFUN([PKG_PROG_PKG_CONFIG],
[m4_pattern_forbid([^_?PKG_[A-Z_]+$])
m4_pattern_allow([^PKG_CONFIG(_(PATH|LIBDIR|SYSROOT_DIR|ALLOW_SYSTEM_(CFLAGS|LIBS)))?$])
m4_pattern_allow([^PKG_CONFIG_(DISABLE_UNINSTALLED|TOP_BUILD_DIR|DEBUG_SPEW)$])
AC_ARG_VAR([PKG_CONFIG], [path to pkg-config utility])
AC_ARG_VAR([PKG_CONFIG_PATH], [directories to add to pkg-config's search path])
AC_ARG_VAR([PKG_CONFIG_LIBDIR], [path overriding pkg-config's built-in search path])

if test "x$ac_cv_env_PKG_CONFIG_set" != "xset"; then
	AC_PATH_TOOL([PKG_CONFIG], [pkg-config])
fi
if test -n "$PKG_CONFIG"; then
	_pkg_min_version=m4_default([$1], [0.9.0])
	AC_MSG_CHECKING([pkg-config is at least version $_pkg_min_version])
	if $PKG_CONFIG --atleast-pkgconfig-version $_pkg_min_version; then
		AC_MSG_RESULT([yes])
	else
		AC_MSG_RESULT([no])
		PKG_CONFIG=""
	fi
fi[]dnl
])dnl PKG_PROG_PKG_CONFIG

dnl PKG_CHECK_EXISTS(MODULES, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
dnl -------------------------------------------------------------------
dnl Since: 0.18
dnl
dnl Check to see whether a particular set of modules exists. Similar to
dnl PKG_CHECK_MODULES(), but does not set variables or print errors.
dnl
dnl Please remember that m4 expands AC_REQUIRE([PKG_PROG_PKG_CONFIG])
dnl only at the first occurrence in configure.ac, so if the first place
dnl it's called might be skipped (such as if it is within an "if", you
dnl have to call PKG_CHECK_EXISTS manually
AC_DEFUN([PKG_CHECK_EXISTS],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
if test -n "$PKG_CONFIG" && \
    AC_RUN_LOG([$PKG_CONFIG --exists --print-errors "$1"]); then
  m4_default([$2], [:])
m4_ifvaln([$3], [else
  $3])dnl
fi])

dnl _PKG_CONFIG([VARIABLE], [COMMAND], [MODULES])
dnl ---------------------------------------------
dnl Internal wrapper calling pkg-config via PKG_CONFIG and setting
dnl pkg_failed based on the result.
m4_define([_PKG_CONFIG],
[if test -n "$$1"; then
    pkg_cv_[]$1="$$1"
 elif test -n "$PKG_CONFIG"; then
    PKG_CHECK_EXISTS([$3],
                     [pkg_cv_[]$1=`$PKG_CONFIG --[]$2 "$3" 2>/dev/null`
		      test "x$?" != "x0" && pkg_failed=yes ],
		     [pkg_failed=yes])
 else
    pkg_failed=untried
fi[]dnl
])dnl _PKG_CONFIG

dnl _PKG_SHORT_ERRORS_SUPPORTED
dnl ---------------------------
dnl Internal check to see if pkg-config supports short errors.
AC_DEFUN([_PKG_SHORT_ERRORS_SUPPORTED],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])
if $PKG_CONFIG --atleast-pkgconfig-version 0.20; then
        _pkg_short_errors_supported=yes
else
        _pkg_short_errors_supported=no
fi[]dnl
])dnl _PKG_SHORT_ERRORS_SUPPORTED


dnl PKG_CHECK_MODULES(VARIABLE-PREFIX, MODULES, [ACTION-IF-FOUND],
dnl   [ACTION-IF-NOT-FOUND])
dnl --------------------------------------------------------------
dnl Since: 0.4.0
dnl
dnl Note that if there is a possibility the first call to
dnl PKG_CHECK_MODULES might not happen, you should be sure to include an
dnl explicit call to PKG_PROG_PKG_CONFIG in your configure.ac
AC_DEFUN([PKG_CHECK_MODULES],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
AC_ARG_VAR([$1][_CFLAGS], [C compiler flags for $1, overriding pkg-config])dnl
AC_ARG_VAR([$1][_LIBS], [linker flags for $1, overriding pkg-config])dnl

pkg_failed=no
AC_MSG_CHECKING([for $2])

_PKG_CONFIG([$1][_CFLAGS], [cflags], [$2])
_PKG_CONFIG([$1][_LIBS], [libs], [$2])

m4_define([_PKG_TEXT], [Alternatively, you may set the environment variables $1[]_CFLAGS
and $1[]_LIBS to avoid the need to call pkg-config.
See the pkg-config man page for more details.])

if test $pkg_failed = yes; then
        AC_MSG_RESULT([no])
        _PKG_SHORT_ERRORS_SUPPORTED
        if test $_pkg_short_errors_supported = yes; then
                $1[]_PKG_ERRORS=`$PKG_CONFIG --short-errors --print-errors --cflags --libs "$2" 2>&1`
        else
                $1[]_PKG_ERRORS=`$PKG_CONFIG --print-errors --cflags --libs "$2" 2>&1`
        fi
        # Put the nasty error message in config.log where it belongs
        echo "$$1[]_PKG_ERRORS" >&AS_MESSAGE_LOG_FD

        m4_default([$4], [AC_MSG_ERROR(
[Package requirements ($2) were not met:

$$1_PKG_ERRORS

Consider adjusting the PKG_CONFIG_PATH environment variable if you
installed software in a non-standard prefix.

_PKG_TEXT])[]dnl
        ])
elif test $pkg_failed = untried; then
        AC_MSG_RESULT([no])
        m4_default([$4], [AC_MSG_FAILURE(
[The pkg-config script could not be found or is too old.  Make sure it
is in your PATH or set the PKG_CONFIG environment variable to the full
path to pkg-config.

_PKG_TEXT

To get pkg-config, see <http://pkg-config.freedesktop.org/>.])[]dnl
        ])
else
        $1[]_CFLAGS=$pkg_cv_[]$1[]_CFLAGS
        $1[]_LIBS=$pkg_cv_[]$1[]_LIBS
        AC_MSG_RESULT([yes])
        $3
fi[]dnl
])dnl PKG_CHECK_MODULES


dnl PKG_CHECK_MODULES_STATIC(VARIABLE-PREFIX, MODULES, [ACTION-IF-FOUND],
dnl   [ACTION-IF-NOT-FOUND])
dnl ---------------------------------------------------------------------
dnl Since: 0.29
dnl
dnl Checks for existence of MODULES and gathers its build flags with
dnl static libraries enabled. Sets VARIABLE-PREFIX_CFLAGS from --cflags
dnl and VARIABLE-PREFIX_LIBS from --libs.
dnl
dnl Note that if there is a possibility the first call to
dnl PKG_CHECK_MODULES_STATIC might not happen, you should be sure to
dnl include an explicit call to PKG_PROG_PKG_CONFIG in your
dnl configure.ac.
AC_DEFUN([PKG_CHECK_MODULES_STATIC],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
_save_PKG_CONFIG=$PKG_CONFIG
PKG_CONFIG="$PKG_CONFIG --static"
PKG_CHECK_MODULES($@)
PKG_CONFIG=$_save_PKG_CONFIG[]dnl
])dnl PKG_CHECK_MODULES_STATIC


dnl PKG_INSTALLDIR([DIRECTORY])
dnl -------------------------
dnl Since: 0.27
dnl
dnl Substitutes the variable pkgconfigdir as the location where a module
dnl should install pkg-config .pc files. By default the directory is
dnl $libdir/pkgconfig, but the default can be changed by passing
dnl DIRECTORY. The user can override through the --with-pkgconfigdir
dnl parameter.
AC_DEFUN([PKG_INSTALLDIR],
[m4_pushdef([pkg_default], [m4_default([$1], ['${libdir}/pkgconfig'])])
m4_pushdef([pkg_description],
    [pkg-config installation directory @<:@]pkg_default[@:>@])
AC_ARG_WITH([pkgconfigdir],
    [AS_HELP_STRING([--with-pkgconfigdir], pkg_description)],,
    [with_pkgconfigdir=]pkg_default)
AC_SUBST([pkgconfigdir], [$with_pkgconfigdir])
m4_popdef([pkg_default])
m4_popdef([pkg_description])
])dnl PKG_INSTALLDIR


dnl PKG_NOARCH_INSTALLDIR([DIRECTORY])
dnl --------------------------------
dnl Since: 0.27
dnl
dnl Substitutes the variable noarch_pkgconfigdir as the location where a
dnl module should install arch-independent pkg-config .pc files. By
dnl default the directory is $datadir/pkgconfig, but the default can be
dnl changed by passing DIRECTORY. The user can override through the
dnl --with-noarch-pkgconfigdir parameter.
AC_DEFUN([PKG_NOARCH_INSTALLDIR],
[m4_pushdef([pkg_default], [m4_default([$1], ['${datadir}/pkgconfig'])])
m4_pushdef([pkg_description],
    [pkg-config arch-independent installation directory @<:@]pkg_default[@:>@])
AC_ARG_WITH([noarch-pkgconfigdir],
    [AS_HELP_STRING([--with-noarch-pkgconfigdir], pkg_description)],,
    [with_noarch_pkgconfigdir=]pkg_default)
AC_SUBST([noarch_pkgconfigdir], [$with_noarch_pkgconfigdir])
m4_popdef([pkg_default])
m4_popdef([pkg_description])
])dnl PKG_NOARCH_INSTALLDIR


dnl PKG_CHECK_VAR(VARIABLE, MODULE, CONFIG-VARIABLE,
dnl [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
dnl -------------------------------------------
dnl Since: 0.28
dnl
dnl Retrieves the value of the pkg-config variable for the given module.
AC_DEFUN([PKG_CHECK_VAR],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
AC_ARG_VAR([$1], [value of $3 for $2, overriding pkg-config])dnl

_PKG_CONFIG([$1], [variable="][$3]["], [$2])
AS_VAR_COPY([$1], [pkg_cv_][$1])

AS_VAR_IF([$1], [""], [$5], [$4])dnl
])dnl PKG_CHECK_VAR

dnl PKG_WITH_MODULES(VARIABLE-PREFIX, MODULES,
dnl   [ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND],
dnl   [DESCRIPTION], [DEFAULT])
dnl ------------------------------------------
dnl
dnl Prepare a "--with-" configure option using the lowercase
dnl [VARIABLE-PREFIX] name, merging the behaviour of AC_ARG_WITH and
dnl PKG_CHECK_MODULES in a single macro.
AC_DEFUN([PKG_WITH_MODULES],
[
m4_pushdef([with_arg], m4_tolower([$1]))

m4_pushdef([description],
           [m4_default([$5], [build with ]with_arg[ support])])

m4_pushdef([def_arg], [m4_default([$6], [auto])])
m4_pushdef([def_action_if_found], [AS_TR_SH([with_]with_arg)=yes])
m4_pushdef([def_action_if_not_found], [AS_TR_SH([with_]with_arg)=no])

m4_case(def_arg,
            [yes],[m4_pushdef([with_without], [--without-]with_arg)],
            [m4_pushdef([with_without],[--with-]with_arg)])

AC_ARG_WITH(with_arg,
     AS_HELP_STRING(with_without, description[ @<:@default=]def_arg[@:>@]),,
    [AS_TR_SH([with_]with_arg)=def_arg])

AS_CASE([$AS_TR_SH([with_]with_arg)],
            [yes],[PKG_CHECK_MODULES([$1],[$2],$3,$4)],
            [auto],[PKG_CHECK_MODULES([$1],[$2],
                                        [m4_n([def_action_if_found]) $3],
                                        [m4_n([def_action_if_not_found]) $4])])

m4_popdef([with_arg])
m4_popdef([description])
m4_popdef([def_arg])

])dnl PKG_WITH_MODULES

dnl PKG_HAVE_WITH_MODULES(VARIABLE-PREFIX, MODULES,
dnl   [DESCRIPTION], [DEFAULT])
dnl -----------------------------------------------
dnl
dnl Convenience macro to trigger AM_CONDITIONAL after PKG_WITH_MODULES
dnl check._[VARIABLE-PREFIX] is exported as make variable.
AC_DEFUN([PKG_HAVE_WITH_MODULES],
[
PKG_WITH_MODULES([$1],[$2],,,[$3],[$4])

AM_CONDITIONAL([HAVE_][$1],
               [test "$AS_TR_SH([with_]m4_tolower([$1]))" = "yes"])
])dnl PKG_HAVE_WITH_MODULES

dnl PKG_HAVE_DEFINE_WITH_MODULES(VARIABLE-PREFIX, MODULES,
dnl   [DESCRIPTION], [DEFAULT])
dnl ------------------------------------------------------
dnl
dnl Convenience macro to run AM_CONDITIONAL and AC_DEFINE after
dnl PKG_WITH_MODULES check. HAVE_[VARIABLE-PREFIX] is exported as make
dnl and preprocessor variable.
AC_DEFUN([PKG_HAVE_DEFINE_WITH_MODULES],
[
PKG_HAVE_WITH_MODULES([$1],[$2],[$3],[$4])

AS_IF([test "$AS_TR_SH([with_]m4_tolower([$1]))" = "yes"],
        [AC_DEFINE([HAVE_][$1], 1, [Enable ]m4_tolower([$1])[ support])])
])dnl PKG_HAVE_DEFINE_WITH_MODULES

# Copyright (C) 2002-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_AUTOMAKE_VERSION(VERSION)
# ----------------------------
# Automake X.Y traces this macro to ensure aclocal.m4 has been
# generated from the m4 files accompanying Automake X.Y.
# (This private macro should not be called outside this file.)
AC_DEFUN([AM_AUTOMAKE_VERSION],
[am__api_version='1.16'
dnl Some users find AM_AUTOMAKE_VERSION and mistake it for a way to
dnl require some minimum version.  Point them to the right macro.
m4_if([$1], [1.16.5], [],
      [AC_FATAL([Do not call $0, use AM_INIT_AUTOMAKE([$1]).])])dnl
])

# _AM_AUTOCONF_VERSION(VERSION)
# -----------------------------
# aclocal traces this macro to find the Autoconf version.
# This is a private macro too.  Using m4_define simplifies
# the logic in aclocal, which can simply ignore this definition.
m4_define([_AM_AUTOCONF_VERSION], [])

# AM_SET_CURRENT_AUTOMAKE_VERSION
# -------------------------------
# Call AM_AUTOMAKE_VERSION and AM_AUTOMAKE_VERSION so they can be traced.
# This function is AC_REQUIREd by AM_INIT_AUTOMAKE.
AC_DEFUN([AM_SET_CURRENT_AUTOMAKE_VERSION],
[AM_AUTOMAKE_VERSION([1.16.5])dnl
m4_ifndef([AC_AUTOCONF_VERSION],
  [m4_copy([m4_PACKAGE_VERSION], [AC_AUTOCONF_VERSION])])dnl
_AM_AUTOCONF_VERSION(m4_defn([AC_AUTOCONF_VERSION]))])

# AM_AUX_DIR_EXPAND                                         -*- Autoconf -*-

# Copyright (C) 2001-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# For projects using AC_CONFIG_AUX_DIR([foo]), Autoconf sets
# $ac_aux_dir to '$srcdir/foo'.  In other projects, it is set to
# '$srcdir', '$srcdir/..', or '$srcdir/../..'.
#
# Of course, Automake must honor this variable whenever it calls a
# tool from the auxiliary directory.  The problem is that $srcdir (and
# therefore $ac_aux_dir as well) can be either absolute or relative,
# depending on how configure is run.  This is pretty annoying, since
# it makes $ac_aux_dir quite unusable in subdirectories: in the top
# source directory, any form will work fine, but in subdirectories a
# relative path needs to be adjusted first.
#
# $ac_aux_dir/missing
#    fails when called from a subdirectory if $ac_aux_dir is relative
# $top_srcdir/$ac_aux_dir/missing
#    fails if $ac_aux_dir is absolute,
#    fails when called from a subdirectory in a VPATH build with
#          a relative $ac_aux_dir
#
# The reason of the latter failure is that $top_srcdir and $ac_aux_dir
# are both prefixed by $srcdir.  In an in-source build this is usually
# harmless because $srcdir is '.', but things will broke when you
# start a VPATH build or use an absolute $srcdir.
#
# So we could use something similar to $top_srcdir/$ac_aux_dir/missing,
# iff we strip the leading $srcdir from $ac_aux_dir.  That would be:
#   am_aux_dir='\$(top_srcdir)/'`expr "$ac_aux_dir" : "$srcdir//*\(.*\)"`
# and then we would define $MISSING as
#   MISSING="\${SHELL} $am_aux_dir/missing"
# This will work as long as MISSING is not called from configure, because
# unfortunately $(top_srcdir) has no meaning in configure.
# However there are other variables, like CC, which are often used in
# configure, and could therefore not use this "fixed" $ac_aux_dir.
#
# Another solution, used here, is to always expand $ac_aux_dir to an
# absolute PATH.  The drawback is that using absolute paths prevent a
# configured tree to be moved without reconfiguration.

AC_DEFUN([AM_AUX_DIR_EXPAND],
[AC_REQUIRE([AC_CONFIG_AUX_DIR_DEFAULT])dnl
# Expand $ac_aux_dir to an absolute path.
am_aux_dir=`cd "$ac_aux_dir" && pwd`
])

# AM_CONDITIONAL                                            -*- Autoconf -*-

# Copyright (C) 1997-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_CONDITIONAL(NAME, SHELL-CONDITION)
# -------------------------------------
# Define a conditional.
AC_DEFUN([AM_CONDITIONAL],
[AC_PREREQ([2.52])dnl
 m4_if([$1], [TRUE],  [AC_FATAL([$0: invalid condition: $1])],
       [$1], [FALSE], [AC_FATAL([$0: invalid condition: $1])])dnl
AC_SUBST([$1_TRUE])dnl
AC_SUBST([$1_FALSE])dnl
_AM_SUBST_NOTMAKE([$1_TRUE])dnl
_AM_SUBST_NOTMAKE([$1_FALSE])dnl
m4_define([_AM_COND_VALUE_$1], [$2])dnl
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi
AC_CONFIG_COMMANDS_PRE(
[if test -z "${$1_TRUE}" && test -z "${$1_FALSE}"; then
  AC_MSG_ERROR([[conditional "$1" was never defined.
Usually this means the macro was only invoked conditionally.]])
fi])])

# Copyright (C) 1999-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.


# There are a few dirty hacks below to avoid letting 'AC_PROG_CC' be
# written in clear, in which case automake, when reading aclocal.m4,
# will think it sees a *use*, and therefore will trigger all it's
# C support machinery.  Also note that it means that autoscan, seeing
# CC etc. in the Makefile, will ask for an AC_PROG_CC use...


# _AM_DEPENDENCIES(NAME)
# ----------------------
# See how the compiler implements dependency checking.
# NAME is "CC", "CXX", "OBJC", "OBJCXX", "UPC", or "GJC".
# We try a few techniques and use that to set a single cache variable.
#
# We don't AC_REQUIRE the corresponding AC_PROG_CC since the latter was
# modified to invoke _AM_DEPENDENCIES(CC); we would have a circular
# dependency, and given that the user is not expected to run this macro,
# just rely on AC_PROG_CC.
AC_DEFUN([_AM_DEPENDENCIES],
[AC_REQUIRE([AM_SET_DEPDIR])dnl
AC_REQUIRE([AM_OUTPUT_DEPENDENCY_COMMANDS])dnl
AC_REQUIRE([AM_MAKE_INCLUDE])dnl
AC_REQUIRE([AM_DEP_TRACK])dnl

m4_if([$1], [CC],   [depcc="$CC"   am_compiler_list=],
      [$1], [CXX],  [depcc="$CXX"  am_compiler_list=],
      [$1], [OBJC], [depcc="$OBJC" am_compiler_list='gcc3 gcc'],
      [$1], [OBJCXX], [depcc="$OBJCXX" am_compiler_list='gcc3 gcc'],
      [$1], [UPC],  [depcc="$UPC"  am_compiler_list=],
      [$1], [GCJ],  [depcc="$GCJ"  am_compiler_list='gcc3 gcc'],
                    [depcc="$$1"   am_compiler_list=])

AC_CACHE_CHECK([dependency style of $depcc],
               [am_cv_$1_dependencies_compiler_type],
[if test -z "$AMDEP_TRUE" && test -f "$am_depcomp"; then
  # We make a subdir and do the tests there.  Otherwise we can end up
  # making bogus files that we don't know about and never remove.  For
  # instance it was reported that on HP-UX the gcc test will end up
  # making a dummy file named 'D' -- because '-MD' means "put the output
  # in D".
  rm -rf conftest.dir
  mkdir conftest.dir
  # Copy depcomp to subdir because otherwise we won't find it if we're
  # using a relative directory.
  cp "$am_depcomp" conftest.dir
  cd conftest.dir
  # We will build objects and dependencies in a subdirectory because
  # it helps to detect inapplicable dependency modes.  For instance
  # both Tru64's cc and ICC support -MD to output dependencies as a
  # side effect of compilation, but ICC will put the dependencies in
  # the current directory while Tru64 will put them in the object
  # directory.
  mkdir sub

  am_cv_$1_dependencies_compiler_type=none
  if test "$am_compiler_list" = ""; then
     am_compiler_list=`sed -n ['s/^#*\([a-zA-Z0-9]*\))$/\1/p'] < ./depcomp`
  fi
  am__universal=false
  m4_case([$1], [CC],
    [case " $depcc " in #(
     *\ -arch\ *\ -arch\ *) am__universal=true ;;
     esac],
    [CXX],
    [case " $depcc " in #(
     *\ -arch\ *\ -arch\ *) am__universal=true ;;
     esac])

  for depmode in $am_compiler_list; do
    # Setup a source with many dependencies, because some compilers
    # like to wrap large dependency lists on column 80 (with \), and
    # we should not choose a depcomp mode which is confused by this.
    #
    # We need to recreate these files for each test, as the compiler may
    # overwrite some of them when testing with obscure command lines.
    # This happens at least with the AIX C compiler.
    : > sub/conftest.c
    for i in 1 2 3 4 5 6; do
      echo '#include "conftst'$i'.h"' >> sub/conftest.c
      # Using ": > sub/conftst$i.h" creates only sub/conftst1.h with
      # Solaris 10 /bin/sh.
      echo '/* dummy */' > sub/conftst$i.h
    done
    echo "${am__include} ${am__quote}sub/conftest.Po${am__quote}" > confmf

    # We check with '-c' and '-o' for the sake of the "dashmstdout"
    # mode.  It turns out that the SunPro C++ compiler does not properly
    # handle '-M -o', and we need to detect this.  Also, some Intel
    # versions had trouble with output in subdirs.
    am__obj=sub/conftest.${OBJEXT-o}
    am__minus_obj="-o $am__obj"
    case $depmode in
    gcc)
      # This depmode causes a compiler race in universal mode.
      test "$am__universal" = false || continue
      ;;
    nosideeffect)
      # After this tag, mechanisms are not by side-effect, so they'll
      # only be used when explicitly requested.
      if test "x$enable_dependency_tracking" = xyes; then
	continue
      else
	break
      fi
      ;;
    msvc7 | msvc7msys | msvisualcpp | msvcmsys)
      # This compiler won't grok '-c -o', but also, the minuso test has
      # not run yet.  These depmodes are late enough in the game, and
      # so weak that their functioning should not be impacted.
      am__obj=conftest.${OBJEXT-o}
      am__minus_obj=
      ;;
    none) break ;;
    esac
    if depmode=$depmode \
       source=sub/conftest.c object=$am__obj \
       depfile=sub/conftest.Po tmpdepfile=sub/conftest.TPo \
       $SHELL ./depcomp $depcc -c $am__minus_obj sub/conftest.c \
         >/dev/null 2>conftest.err &&
       grep sub/conftst1.h sub/conftest.Po > /dev/null 2>&1 &&
       grep sub/conftst6.h sub/conftest.Po > /dev/null 2>&1 &&
       grep $am__obj sub/conftest.Po > /dev/null 2>&1 &&
       ${MAKE-make} -s -f confmf > /dev/null 2>&1; then
      # icc doesn't choke on unknown options, it will just issue warnings
      # or remarks (even with -Werror).  So we grep stderr for any message
      # that says an option was ignored or not supported.
      # When given -MP, icc 7.0 and 7.1 complain thusly:
      #   icc: Command line warning: ignoring option '-M'; no argument required
      # The diagnosis changed in icc 8.0:
      #   icc: Command line remark: option '-MP' not supported
      if (grep 'ignoring option' conftest.err ||
          grep 'not supported' conftest.err) >/dev/null 2>&1; then :; else
        am_cv_$1_dependencies_compiler_type=$depmode
        break
      fi
    fi
  done

  cd ..
  rm -rf conftest.dir
else
  am_cv_$1_dependencies_compiler_type=none
fi
])
AC_SUBST([$1DEPMODE], [depmode=$am_cv_$1_dependencies_compiler_type])
AM_CONDITIONAL([am__fastdep$1], [
  test "x$enable_dependency_tracking" != xno \
  && test "$am_cv_$1_dependencies_compiler_type" = gcc3])
])


# AM_SET_DEPDIR
# -------------
# Choose a directory name for dependency files.
# This macro is AC_REQUIREd in _AM_DEPENDENCIES.
AC_DEFUN([AM_SET_DEPDIR],
[AC_REQUIRE([AM_SET_LEADING_DOT])dnl
AC_SUBST([DEPDIR], ["${am__leading_dot}deps"])dnl
])


# AM_DEP_TRACK
# ------------
AC_DEFUN([AM_DEP_TRACK],
[AC_ARG_ENABLE([dependency-tracking], [dnl
AS_HELP_STRING(
  [--enable-dependency-tracking],
  [do not reject slow dependency extractors])
AS_HELP_STRING(
  [--disable-dependency-tracking],
  [speeds up one-time build])])
if test "x$enable_dependency_tracking" != xno; then
  am_depcomp="$ac_aux_dir/depcomp"
  AMDEPBACKSLASH='\'
  am__nodep='_no'
fi
AM_CONDITIONAL([AMDEP], [test "x$enable_dependency_tracking" != xno])
AC_SUBST([AMDEPBACKSLASH])dnl
_AM_SUBST_NOTMAKE([AMDEPBACKSLASH])dnl
AC_SUBST([am__nodep])dnl
_AM_SUBST_NOTMAKE([am__nodep])dnl
])

# Generate code to set up dependency tracking.              -*- Autoconf -*-

# Copyright (C) 1999-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_OUTPUT_DEPENDENCY_COMMANDS
# ------------------------------
AC_DEFUN([_AM_OUTPUT_DEPENDENCY_COMMANDS],
[{
  # Older Autoconf quotes --file arguments for eval, but not when files
  # are listed without --file.  Let's play safe and only enable the eval
  # if we detect the quoting.
  # TODO: see whether this extra hack can be removed once we start
  # requiring Autoconf 2.70 or later.
  AS_CASE([$CONFIG_FILES],
          [*\'*], [eval set x "$CONFIG_FILES"],
          [*], [set x $CONFIG_FILES])
  shift
  # Used to flag and report bootstrapping failures.
  am_rc=0
  for am_mf
  do
    # Strip MF so we end up with the name of the file.
    am_mf=`AS_ECHO(["$am_mf"]) | sed -e 's/:.*$//'`
    # Check whether this is an Automake generated Makefile which includes
    # dependency-tracking related rules and includes.
    # Grep'ing the whole file directly is not great: AIX grep has a line
    # limit of 2048, but all sed's we know have understand at least 4000.
    sed -n 's,^am--depfiles:.*,X,p' "$am_mf" | grep X >/dev/null 2>&1 \
      || continue
    am_dirpart=`AS_DIRNAME(["$am_mf"])`
    am_filepart=`AS_BASENAME(["$am_mf"])`
    AM_RUN_LOG([cd "$am_dirpart" \
      && sed -e '/# am--include-marker/d' "$am_filepart" \
        | $MAKE -f - am--depfiles]) || am_rc=$?
  done
  if test $am_rc -ne 0; then
    AC_MSG_FAILURE([Something went wrong bootstrapping makefile fragments
    for automatic dependency tracking.  If GNU make was not used, consider
    re-running the configure script with MAKE="gmake" (or whatever is
    necessary).  You can also try re-running configure with the
    '--disable-dependency-tracking' option to at least be able to build
    the package (albeit without support for automatic dependency tracking).])
  fi
  AS_UNSET([am_dirpart])
  AS_UNSET([am_filepart])
  AS_UNSET([am_mf])
  AS_UNSET([am_rc])
  rm -f conftest-deps.mk
}
])# _AM_OUTPUT_DEPENDENCY_COMMANDS


# AM_OUTPUT_DEPENDENCY_COMMANDS
# -----------------------------
# This macro should only be invoked once -- use via AC_REQUIRE.
#
# This code is only required when automatic dependency tracking is enabled.
# This creates each '.Po' and '.Plo' makefile fragment that we'll need in
# order to bootstrap the dependency handling code.
AC_DEFUN([AM_OUTPUT_DEPENDENCY_COMMANDS],
[AC_CONFIG_COMMANDS([depfiles],
     [test x"$AMDEP_TRUE" != x"" || _AM_OUTPUT_DEPENDENCY_COMMANDS],
     [AMDEP_TRUE="$AMDEP_TRUE" MAKE="${MAKE-make}"])])

# Do all the work for Automake.                             -*- Autoconf -*-

# Copyright (C) 1996-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This macro actually does too much.  Some checks are only needed if
# your package does certain things.  But this isn't really a big deal.

dnl Redefine AC_PROG_CC to automatically invoke _AM_PROG_CC_C_O.
m4_define([AC_PROG_CC],
m4_defn([AC_PROG_CC])
[_AM_PROG_CC_C_O
])

# AM_INIT_AUTOMAKE(PACKAGE, VERSION, [NO-DEFINE])
# AM_INIT_AUTOMAKE([OPTIONS])
# -----------------------------------------------
# The call with PACKAGE and VERSION arguments is the old style
# call (pre autoconf-2.50), which is being phased out.  PACKAGE
# and VERSION should now be passed to AC_INIT and removed from
# the call to AM_INIT_AUTOMAKE.
# We support both call styles for the transition.  After
# the next Automake release, Autoconf can make the AC_INIT
# arguments mandatory, and then we can depend on a new Autoconf
# release and drop the old call support.
AC_DEFUN([AM_INIT_AUTOMAKE],
[AC_PREREQ([2.65])dnl
m4_ifdef([_$0_ALREADY_INIT],
  [m4_fatal([$0 expanded multiple times
]m4_defn([_$0_ALREADY_INIT]))],
  [m4_define([_$0_ALREADY_INIT], m4_expansion_stack)])dnl
dnl Autoconf wants to disallow AM_ names.  We explicitly allow
dnl the ones we care about.
m4_pattern_allow([^AM_[A-Z]+FLAGS$])dnl
AC_REQUIRE([AM_SET_CURRENT_AUTOMAKE_VERSION])dnl
AC_REQUIRE([AC_PROG_INSTALL])dnl
if test "`cd $srcdir && pwd`" != "`pwd`"; then
  # Use -I$(srcdir) only when $(srcdir) != ., so that make's output
  # is not polluted with repeated "-I."
  AC_SUBST([am__isrc], [' -I$(srcdir)'])_AM_SUBST_NOTMAKE([am__isrc])dnl
  # test to see if srcdir already configured
  if test -f $srcdir/config.status; then
    AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
  fi
fi

# test whether we have cygpath
if test -z "$CYGPATH_W"; then
  if (cygpath --version) >/dev/null 2>/dev/null; then
    CYGPATH_W='cygpath -w'
  else
    CYGPATH_W=echo
  fi
fi
AC_SUBST([CYGPATH_W])

# Define the identity of the package.
dnl Distinguish between old-style and new-style calls.
m4_ifval([$2],
[AC_DIAGNOSE([obsolete],
             [$0: two- and three-arguments forms are deprecated.])
m4_ifval([$3], [_AM_SET_OPTION([no-define])])dnl
 AC_SUBST([PACKAGE], [$1])dnl
 AC_SUBST([VERSION], [$2])],
[_AM_SET_OPTIONS([$1])dnl
dnl Diagnose old-style AC_INIT with new-style AM_AUTOMAKE_INIT.
m4_if(
  m4_ifset([AC_PACKAGE_NAME], [ok]):m4_ifset([AC_PACKAGE_VERSION], [ok]),
  [ok:ok],,
  [m4_fatal([AC_INIT should be called with package and version arguments])])dnl
 AC_SUBST([PACKAGE], ['AC_PACKAGE_TARNAME'])dnl
 AC_SUBST([VERSION], ['AC_PACKAGE_VERSION'])])dnl

_AM_IF_OPTION([no-define],,
[AC_DEFINE_UNQUOTED([PACKAGE], ["$PACKAGE"], [Name of package])
 AC_DEFINE_UNQUOTED([VERSION], ["$VERSION"], [Version number of package])])dnl

# Some tools Automake needs.
AC_REQUIRE([AM_SANITY_CHECK])dnl
AC_REQUIRE([AC_ARG_PROGRAM])dnl
AM_MISSING_PROG([ACLOCAL], [aclocal-${am__api_version}])
AM_MISSING_PROG([AUTOCONF], [autoconf])
AM_MISSING_PROG([AUTOMAKE], [automake-${am__api_version}])
AM_MISSING_PROG([AUTOHEADER], [autoheader])
AM_MISSING_PROG([MAKEINFO], [makeinfo])
AC_REQUIRE([AM_PROG_INSTALL_SH])dnl
AC_REQUIRE([AM_PROG_INSTALL_STRIP])dnl
AC_REQUIRE([AC_PROG_MKDIR_P])dnl
# For better backward compatibility.  To be removed once Automake 1.9.x
# dies out for good.  For more background, see:
# <https://lists.gnu.org/archive/html/automake/2012-07/msg00001.html>
# <https://lists.gnu.org/archive/html/automake/2012-07/msg00014.html>
AC_SUBST([mkdir_p], ['$(MKDIR_P)'])
# We need awk for the "check" target (and possibly the TAP driver).  The
# system "awk" is bad on some platforms.
AC_REQUIRE([AC_PROG_AWK])dnl
AC_REQUIRE([AC_PROG_MAKE_SET])dnl
AC_REQUIRE([AM_SET_LEADING_DOT])dnl
_AM_IF_OPTION([tar-ustar], [_AM_PROG_TAR([ustar])],
	      [_AM_IF_OPTION([tar-pax], [_AM_PROG_TAR([pax])],
			     [_AM_PROG_TAR([v7])])])
_AM_IF_OPTION([no-dependencies],,
[AC_PROVIDE_IFELSE([AC_PROG_CC],
		  [_AM_DEPENDENCIES([CC])],
		  [m4_define([AC_PROG_CC],
			     m4_defn([AC_PROG_CC])[_AM_DEPENDENCIES([CC])])])dnl
AC_PROVIDE_IFELSE([AC_PROG_CXX],
		  [_AM_DEPENDENCIES([CXX])],
		  [m4_define([AC_PROG_CXX],
			     m4_defn([AC_PROG_CXX])[_AM_DEPENDENCIES([CXX])])])dnl
AC_PROVIDE_IFELSE([AC_PROG_OBJC],
		  [_AM_DEPENDENCIES([OBJC])],
		  [m4_define([AC_PROG_OBJC],
			     m4_defn([AC_PROG_OBJC])[_AM_DEPENDENCIES([OBJC])])])dnl
AC_PROVIDE_IFELSE([AC_PROG_OBJCXX],
		  [_AM_DEPENDENCIES([OBJCXX])],
		  [m4_define([AC_PROG_OBJCXX],
			     m4_defn([AC_PROG_OBJCXX])[_AM_DEPENDENCIES([OBJCXX])])])dnl
])
# Variables for tags utilities; see am/tags.am
if test -z "$CTAGS"; then
  CTAGS=ctags
fi
AC_SUBST([CTAGS])
if test -z "$ETAGS"; then
  ETAGS=etags
fi
AC_SUBST([ETAGS])
if test -z "$CSCOPE"; then
  CSCOPE=cscope
fi
AC_SUBST([CSCOPE])

AC_REQUIRE([AM_SILENT_RULES])dnl
dnl The testsuite driver may need to know about EXEEXT, so add the
dnl 'am__EXEEXT' conditional if _AM_COMPILER_EXEEXT was seen.  This
dnl macro is hooked onto _AC_COMPILER_EXEEXT early, see below.
AC_CONFIG_COMMANDS_PRE(dnl
[m4_provide_if([_AM_COMPILER_EXEEXT],
  [AM_CONDITIONAL([am__EXEEXT], [test -n "$EXEEXT"])])])dnl

# POSIX will say in a future version that running "rm -f" with no argument
# is OK; and we want to be able to make that assumption in our Makefile
# recipes.  So use an aggressive probe to check that the usage we want is
# actually supported "in the wild" to an acceptable degree.
# See automake bug#10828.
# To make any issue more visible, cause the running configure to be aborted
# by default if the 'rm' program in use doesn't match our expectations; the
# user can still override this though.
if rm -f && rm -fr && rm -rf; then : OK; else
  cat >&2 <<'END'
Oops!

Your 'rm' program seems unable to run without file operands specified
on the command line, even when the '-f' option is present.  This is contrary
to the behaviour of most rm programs out there, and not conforming with
the upcoming POSIX standard: <http://austingroupbugs.net/view.php?id=542>

Please tell bug-automake@gnu.org about your system, including the value
of your $PATH and any error possibly output before this message.  This
can help us improve future automake versions.

END
  if test x"$ACCEPT_INFERIOR_RM_PROGRAM" = x"yes"; then
    echo 'Configuration will proceed anyway, since you have set the' >&2
    echo 'ACCEPT_INFERIOR_RM_PROGRAM variable to "yes"' >&2
    echo >&2
  else
    cat >&2 <<'END'
Aborting the configuration process, to ensure you take notice of the issue.

You can download and install GNU coreutils to get an 'rm' implementation
that behaves properly: <https://www.gnu.org/software/coreutils/>.

If you want to complete the configuration process using your problematic
'rm' anyway, export the environment variable ACCEPT_INFERIOR_RM_PROGRAM
to "yes", and re-run configure.

END
    AC_MSG_ERROR([Your 'rm' program is bad, sorry.])
  fi
fi
dnl The trailing newline in this macro's definition is deliberate, for
dnl backward compatibility and to allow trailing 'dnl'-style comments
dnl after the AM_INIT_AUTOMAKE invocation. See automake bug#16841.
])

dnl Hook into '_AC_COMPILER_EXEEXT' early to learn its expansion.  Do not
dnl add the conditional right here, as _AC_COMPILER_EXEEXT may be further
dnl mangled by Autoconf and run in a shell conditional statement.
m4_define([_AC_COMPILER_EXEEXT],
m4_defn([_AC_COMPILER_EXEEXT])[m4_provide([_AM_COMPILER_EXEEXT])])

# When config.status generates a header, we must update the stamp-h file.
# This file resides in the same directory as the config header
# that is generated.  The stamp files are numbered to have different names.

# Autoconf calls _AC_AM_CONFIG_HEADER_HOOK (when defined) in the
# loop where config.status creates the headers, so we can generate
# our stamp files there.
AC_DEFUN([_AC_AM_CONFIG_HEADER_HOOK],
[# Compute $1's index in $config_headers.
_am_arg=$1
_am_stamp_count=1
for _am_header in $config_headers :; do
  case $_am_header in
    $_am_arg | $_am_arg:* )
      break ;;
    * )
      _am_stamp_count=`expr $_am_stamp_count + 1` ;;
  esac
done
echo "timestamp for $_am_arg" >`AS_DIRNAME(["$_am_arg"])`/stamp-h[]$_am_stamp_count])

# Copyright (C) 2001-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_PROG_INSTALL_SH
# ------------------
# Define $install_sh.
AC_DEFUN([AM_PROG_INSTALL_SH],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
if test x"${install_sh+set}" != xset; then
  case $am_aux_dir in
  *\ * | *\	*)
    install_sh="\${SHELL} '$am_aux_dir/install-sh'" ;;
  *)
    install_sh="\${SHELL} $am_aux_dir/install-sh"
  esac
fi
AC_SUBST([install_sh])])

# Copyright (C) 2003-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Check whether the underlying file-system supports filenames
# with a leading dot.  For instance MS-DOS doesn't.
AC_DEFUN([AM_SET_LEADING_DOT],
[rm -rf .tst 2>/dev/null
mkdir .tst 2>/dev/null
if test -d .tst; then
  am__leading_dot=.
else
  am__leading_dot=_
fi
rmdir .tst 2>/dev/null
AC_SUBST([am__leading_dot])])

# Check to see how 'make' treats includes.	            -*- Autoconf -*-

# Copyright (C) 2001-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_MAKE_INCLUDE()
# -----------------
# Check whether make has an 'include' directive that can support all
# the idioms we need for our automatic dependency tracking code.
AC_DEFUN([AM_MAKE_INCLUDE],
[AC_MSG_CHECKING([whether ${MAKE-make} supports the include directive])
cat > confinc.mk << 'END'
am__doit:
	@echo this is the am__doit target >confinc.out
.PHONY: am__doit
END
am__include="#"
am__quote=
# BSD make does it like this.
echo '.include "confinc.mk" # ignored' > confmf.BSD
# Other make implementations (GNU, Solaris 10, AIX) do it like this.
echo 'include confinc.mk # ignored' > confmf.GNU
_am_result=no
for s in GNU BSD; do
  AM_RUN_LOG([${MAKE-make} -f confmf.$s && cat confinc.out])
  AS_CASE([$?:`cat confinc.out 2>/dev/null`],
      ['0:this is the am__doit target'],
      [AS_CASE([$s],
          [BSD], [am__include='.include' am__quote='"'],
          [am__include='include' am__quote=''])])
  if test "$am__include" != "#"; then
    _am_result="yes ($s style)"
    break
  fi
done
rm -f confinc.* confmf.*
AC_MSG_RESULT([${_am_result}])
AC_SUBST([am__include])])
AC_SUBST([am__quote])])

# Fake the existence of programs that GNU maintainers use.  -*- Autoconf -*-

# Copyright (C) 1997-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_MISSING_PROG(NAME, PROGRAM)
# ------------------------------
AC_DEFUN([AM_MISSING_PROG],
[AC_REQUIRE([AM_MISSING_HAS_RUN])
$1=${$1-"${am_missing_run}$2"}
AC_SUBST($1)])

# AM_MISSING_HAS_RUN
# ------------------
# Define MISSING if not defined so far and test if it is modern enough.
# If it is, set am_missing_run to use it, otherwise, to nothing.
AC_DEFUN([AM_MISSING_HAS_RUN],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
AC_REQUIRE_AUX_FILE([missing])dnl
if test x"${MISSING+set}" != xset; then
  MISSING="\${SHELL} '$am_aux_dir/missing'"
fi
# Use eval to expand $SHELL
if eval "$MISSING --is-lightweight"; then
  am_missing_run="$MISSING "
else
  am_missing_run=
  AC_MSG_WARN(['missing' script is too old or missing])
fi
])

# Helper functions for option handling.                     -*- Autoconf -*-

# Copyright (C) 2001-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_MANGLE_OPTION(NAME)
# -----------------------
AC_DEFUN([_AM_MANGLE_OPTION],
[[_AM_OPTION_]m4_bpatsubst($1, [[^a-zA-Z0-9_]], [_])])

# _AM_SET_OPTION(NAME)
# --------------------
# Set option NAME.  Presently that only means defining a flag for this option.
AC_DEFUN([_AM_SET_OPTION],
[m4_define(_AM_MANGLE_OPTION([$1]), [1])])

# _AM_SET_OPTIONS(OPTIONS)
# ------------------------
# OPTIONS is a space-separated list of Automake options.
AC_DEFUN([_AM_SET_OPTIONS],
[m4_foreach_w([_AM_Option], [$1], [_AM_SET_OPTION(_AM_Option)])])

# _AM_IF_OPTION(OPTION, IF-SET, [IF-NOT-SET])
# -------------------------------------------
# Execute IF-SET if OPTION is set, IF-NOT-SET otherwise.
AC_DEFUN([_AM_IF_OPTION],
[m4_ifset(_AM_MANGLE_OPTION([$1]), [$2], [$3])])

# Copyright (C) 1999-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_PROG_CC_C_O
# ---------------
# Like AC_PROG_CC_C_O, but changed for automake.  We rewrite AC_PROG_CC
# to automatically call this.
AC_DEFUN([_AM_PROG_CC_C_O],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
AC_REQUIRE_AUX_FILE([compile])dnl
AC_LANG_PUSH([C])dnl
AC_CACHE_CHECK(
  [whether $CC understands -c and -o together],
  [am_cv_prog_cc_c_o],
  [AC_LANG_CONFTEST([AC_LANG_PROGRAM([])])
  # Make sure it works both with $CC and with simple cc.
  # Following AC_PROG_CC_C_O, we do the test twice because some
  # compilers refuse to overwrite an existing .o file with -o,
  # though they will create one.
  am_cv_prog_cc_c_o=yes
  for am_i in 1 2; do
    if AM_RUN_LOG([$CC -c conftest.$ac_ext -o conftest2.$ac_objext]) \
         && test -f conftest2.$ac_objext; then
      : OK
    else
      am_cv_prog_cc_c_o=no
      break
    fi
  done
  rm -f core conftest*
  unset am_i])
if test "$am_cv_prog_cc_c_o" != yes; then
   # Losing compiler, so override with the script.
   # FIXME: It is wrong to rewrite CC.
   # But if we don't then we get into trouble of one sort or another.
   # A longer-term fix would be to have automake use am__CC in this case,
   # and then we could set am__CC="\$(top_srcdir)/compile \$(CC)"
   CC="$am_aux_dir/compile $CC"
fi
AC_LANG_POP([C])])

# For backward compatibility.
AC_DEFUN_ONCE([AM_PROG_CC_C_O], [AC_REQUIRE([AC_PROG_CC])])

# Copyright (C) 2001-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_RUN_LOG(COMMAND)
# -------------------
# Run COMMAND, save the exit status in ac_status, and log it.
# (This has been adapted from Autoconf's _AC_RUN_LOG macro.)
AC_DEFUN([AM_RUN_LOG],
[{ echo "$as_me:$LINENO: $1" >&AS_MESSAGE_LOG_FD
   ($1) >&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
   ac_status=$?
   echo "$as_me:$LINENO: \$? = $ac_status" >&AS_MESSAGE_LOG_FD
   (exit $ac_status); }])

# Check to make sure that the build environment is sane.    -*- Autoconf -*-

# Copyright (C) 1996-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_SANITY_CHECK
# ---------------
AC_DEFUN([AM_SANITY_CHECK],
[AC_MSG_CHECKING([whether build environment is sane])
# Reject unsafe characters in $srcdir or the absolute working directory
# name.  Accept space and tab only in the latter.
am_lf='
'
case `pwd` in
  *[[\\\"\#\$\&\'\`$am_lf]]*)
    AC_MSG_ERROR([unsafe absolute working directory name]);;
esac
case $srcdir in
  *[[\\\"\#\$\&\'\`$am_lf\ \	]]*)
    AC_MSG_ERROR([unsafe srcdir value: '$srcdir']);;
esac

# Do 'set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   am_has_slept=no
   for am_try in 1 2; do
     echo "timestamp, slept: $am_has_slept" > conftest.file
     set X `ls -Lt "$srcdir/configure" conftest.file 2> /dev/null`
     if test "$[*]" = "X"; then
	# -L didn't work.
	set X `ls -t "$srcdir/configure" conftest.file`
     fi
     if test "$[*]" != "X $srcdir/configure conftest.file" \
	&& test "$[*]" != "X conftest.file $srcdir/configure"; then

	# If neither matched, then we have a broken ls.  This can happen
	# if, for instance, CONFIG_SHELL is bash and it inherits a
	# broken ls alias from the environment.  This has actually
	# happened.  Such a system could not be considered "sane".
	AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
  alias in your environment])
     fi
     if test "$[2]" = conftest.file || test $am_try -eq 2; then
       break
     fi
     # Just in case.
     sleep 1
     am_has_slept=yes
   done
   test "$[2]" = conftest.file
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
AC_MSG_RESULT([yes])
# If we didn't sleep, we still need to ensure time stamps of config.status and
# generated files are strictly newer.
am_sleep_pid=
if grep 'slept: no' conftest.file >/dev/null 2>&1; then
  ( sleep 1 ) &
  am_sleep_pid=$!
fi
AC_CONFIG_COMMANDS_PRE(
  [AC_MSG_CHECKING([that generated files are newer than configure])
   if test -n "$am_sleep_pid"; then
     # Hide warnings about reused PIDs.
     wait $am_sleep_pid 2>/dev/null
   fi
   AC_MSG_RESULT([done])])
rm -f conftest.file
])

# Copyright (C) 2009-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_SILENT_RULES([DEFAULT])
# --------------------------
# Enable less verbose build rules; with the default set to DEFAULT
# ("yes" being less verbose, "no" or empty being verbose).
AC_DEFUN([AM_SILENT_RULES],
[AC_ARG_ENABLE([silent-rules], [dnl
AS_HELP_STRING(
  [--enable-silent-rules],
  [less verbose build output (undo: "make V=1")])
AS_HELP_STRING(
  [--disable-silent-rules],
  [verbose build output (undo: "make V=0")])dnl
])
case $enable_silent_rules in @%:@ (((
  yes) AM_DEFAULT_VERBOSITY=0;;
   no) AM_DEFAULT_VERBOSITY=1;;
    *) AM_DEFAULT_VERBOSITY=m4_if([$1], [yes], [0], [1]);;
esac
dnl
dnl A few 'make' implementations (e.g., NonStop OS and NextStep)
dnl do not support nested variable expansions.
dnl See automake bug#9928 and bug#10237.
am_make=${MAKE-make}
AC_CACHE_CHECK([whether $am_make supports nested variables],
   [am_cv_make_support_nested_variables],
   [if AS_ECHO([['TRUE=$(BAR$(V))
BAR0=false
BAR1=true
V=1
am__doit:
	@$(TRUE)
.PHONY: am__doit']]) | $am_make -f - >/dev/null 2>&1; then
  am_cv_make_support_nested_variables=yes
else
  am_cv_make_support_nested_variables=no
fi])
if test $am_cv_make_support_nested_variables = yes; then
  dnl Using '$V' instead of '$(V)' breaks IRIX make.
  AM_V='$(V)'
  AM_DEFAULT_V='$(AM_DEFAULT_VERBOSITY)'
else
  AM_V=$AM_DEFAULT_VERBOSITY
  AM_DEFAULT_V=$AM_DEFAULT_VERBOSITY
fi
AC_SUBST([AM_V])dnl
AM_SUBST_NOTMAKE([AM_V])dnl
AC_SUBST([AM_DEFAULT_V])dnl
AM_SUBST_NOTMAKE([AM_DEFAULT_V])dnl
AC_SUBST([AM_DEFAULT_VERBOSITY])dnl
AM_BACKSLASH='\'
AC_SUBST([AM_BACKSLASH])dnl
_AM_SUBST_NOTMAKE([AM_BACKSLASH])dnl
])

# Copyright (C) 2001-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_PROG_INSTALL_STRIP
# ---------------------
# One issue with vendor 'install' (even GNU) is that you can't
# specify the program used to strip binaries.  This is especially
# annoying in cross-compiling environments, where the build's strip
# is unlikely to handle the host's binaries.
# Fortunately install-sh will honor a STRIPPROG variable, so we
# always use install-sh in "make install-strip", and initialize
# STRIPPROG with the value of the STRIP variable (set by the user).
AC_DEFUN([AM_PROG_INSTALL_STRIP],
[AC_REQUIRE([AM_PROG_INSTALL_SH])dnl
# Installed binaries are usually stripped using 'strip' when the user
# run "make install-strip".  However 'strip' might not be the right
# tool to use in cross-compilation environments, therefore Automake
# will honor the 'STRIP' environment variable to overrule this program.
dnl Don't test for $cross_compiling = yes, because it might be 'maybe'.
if test "$cross_compiling" != no; then
  AC_CHECK_TOOL([STRIP], [strip], :)
fi
INSTALL_STRIP_PROGRAM="\$(install_sh) -c -s"
AC_SUBST([INSTALL_STRIP_PROGRAM])])

# Copyright (C) 2006-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_SUBST_NOTMAKE(VARIABLE)
# ---------------------------
# Prevent Automake from outputting VARIABLE = @VARIABLE@ in Makefile.in.
# This macro is traced by Automake.
AC_DEFUN([_AM_SUBST_NOTMAKE])

# AM_SUBST_NOTMAKE(VARIABLE)
# --------------------------
# Public sister of _AM_SUBST_NOTMAKE.
AC_DEFUN([AM_SUBST_NOTMAKE], [_AM_SUBST_NOTMAKE($@)])

# Check how to create a tarball.                            -*- Autoconf -*-

# Copyright (C) 2004-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_PROG_TAR(FORMAT)
# --------------------
# Check how to create a tarball in format FORMAT.
# FORMAT should be one of 'v7', 'ustar', or 'pax'.
#
# Substitute a variable $(am__tar) that is a command
# writing to stdout a FORMAT-tarball containing the directory
# $tardir.
#     tardir=directory && $(am__tar) > result.tar
#
# Substitute a variable $(am__untar) that extract such
# a tarball read from stdin.
#     $(am__untar) < result.tar
#
AC_DEFUN([_AM_PROG_TAR],
[# Always define AMTAR for backward compatibility.  Yes, it's still used
# in the wild :-(  We should find a proper way to deprecate it ...
AC_SUBST([AMTAR], ['$${TAR-tar}'])

# We'll loop over all known methods to create a tar archive until one works.
_am_tools='gnutar m4_if([$1], [ustar], [plaintar]) pax cpio none'

m4_if([$1], [v7],
  [am__tar='$${TAR-tar} chof - "$$tardir"' am__untar='$${TAR-tar} xf -'],

  [m4_case([$1],
    [ustar],
     [# The POSIX 1988 'ustar' format is defined with fixed-size fields.
      # There is notably a 21 bits limit for the UID and the GID.  In fact,
      # the 'pax' utility can hang on bigger UID/GID (see automake bug#8343
      # and bug#13588).
      am_max_uid=2097151 # 2^21 - 1
      am_max_gid=$am_max_uid
      # The $UID and $GID variables are not portable, so we need to resort
      # to the POSIX-mandated id(1) utility.  Errors in the 'id' calls
      # below are definitely unexpected, so allow the users to see them
      # (that is, avoid stderr redirection).
      am_uid=`id -u || echo unknown`
      am_gid=`id -g || echo unknown`
      AC_MSG_CHECKING([whether UID '$am_uid' is supported by ustar format])
      if test $am_uid -le $am_max_uid; then
         AC_MSG_RESULT([yes])
      else
         AC_MSG_RESULT([no])
         _am_tools=none
      fi
      AC_MSG_CHECKING([whether GID '$am_gid' is supported by ustar format])
      if test $am_gid -le $am_max_gid; then
         AC_MSG_RESULT([yes])
      else
        AC_MSG_RESULT([no])
        _am_tools=none
      fi],

  [pax],
    [],

  [m4_fatal([Unknown tar format])])

  AC_MSG_CHECKING([how to create a $1 tar archive])

  # Go ahead even if we have the value already cached.  We do so because we
  # need to set the values for the 'am__tar' and 'am__untar' variables.
  _am_tools=${am_cv_prog_tar_$1-$_am_tools}

  for _am_tool in $_am_tools; do
    case $_am_tool in
    gnutar)
      for _am_tar in tar gnutar gtar; do
        AM_RUN_LOG([$_am_tar --version]) && break
      done
      am__tar="$_am_tar --format=m4_if([$1], [pax], [posix], [$1]) -chf - "'"$$tardir"'
      am__tar_="$_am_tar --format=m4_if([$1], [pax], [posix], [$1]) -chf - "'"$tardir"'
      am__untar="$_am_tar -xf -"
      ;;
    plaintar)
      # Must skip GNU tar: if it does not support --format= it doesn't create
      # ustar tarball either.
      (tar --version) >/dev/null 2>&1 && continue
      am__tar='tar chf - "$$tardir"'
      am__tar_='tar chf - "$tardir"'
      am__untar='tar xf -'
      ;;
    pax)
      am__tar='pax -L -x $1 -w "$$tardir"'
      am__tar_='pax -L -x $1 -w "$tardir"'
      am__untar='pax -r'
      ;;
    cpio)
      am__tar='find "$$tardir" -print | cpio -o -H $1 -L'
      am__tar_='find "$tardir" -print | cpio -o -H $1 -L'
      am__untar='cpio -i -H $1 -d'
      ;;
    none)
      am__tar=false
      am__tar_=false
      am__untar=false
      ;;
    esac

    # If the value was cached, stop now.  We just wanted to have am__tar
    # and am__untar set.
    test -n "${am_cv_prog_tar_$1}" && break

    # tar/untar a dummy directory, and stop if the command works.
    rm -rf conftest.dir
    mkdir conftest.dir
    echo GrepMe > conftest.dir/file
    AM_RUN_LOG([tardir=conftest.dir && eval $am__tar_ >conftest.tar])
    rm -rf conftest.dir
    if test -s conftest.tar; then
      AM_RUN_LOG([$am__untar <conftest.tar])
      AM_RUN_LOG([cat conftest.dir/file])
      grep GrepMe conftest.dir/file >/dev/null 2>&1 && break
    fi
  done
  rm -rf conftest.dir

  AC_CACHE_VAL([am_cv_prog_tar_$1], [am_cv_prog_tar_$1=$_am_tool])
  AC_MSG_RESULT([$am_cv_prog_tar_$1])])

AC_SUBST([am__tar])
AC_SUBST([am__untar])
]) # _AM_PROG_TAR

