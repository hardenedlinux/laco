dnl -*- Autoconf -*-

AC_INIT(laco, 0.5)
AC_SUBST(HVERSION, "\"0.5\"")
AC_SUBST(AUTHOR, "\"Mu Lei known as NalaGinrut\"")
AC_SUBST(COPYRIGHT, "'(2025)")
AC_SUBST(LICENSE, gpl3+)
AC_CONFIG_SRCDIR(laco.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([gnu silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/laco],[chmod +x scripts/laco])
dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0])
GUILE_PROGS(3.0)
GUILE_FLAGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-3.x installation.])
fi

dnl Hall auto-generated guile-module dependencies


dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
