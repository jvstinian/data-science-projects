AC_PREREQ([2.62])
AC_INIT([jatk], [0.0.1], [jvstinian@gmail.com])
AM_INIT_AUTOMAKE([-Wall -Werror foreign tar-ustar])
# The root Makefile doesn't build anything, so maybe LT_INIT isn't needed here
# LT_PREREQ([2.2])
# LT_INIT

# AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SUBDIRS([vendor/mtwister utils statistics finance])

# Export absolute paths to the umbrella source and build trees
export JATK_TOP_SRCDIR="$abs_top_srcdir"
export JATK_TOP_BUILDDIR="$abs_top_builddir"

# Resolve the absolute source and build paths while configure is running
jatk_srcdir=$(cd "$srcdir" && pwd)
jatk_builddir=$(pwd)

# Append them as explicit arguments to all AC_CONFIG_SUBDIRS invocations
ac_configure_args="$ac_configure_args JATK_TOP_SRCDIR='$jatk_srcdir' JATK_TOP_BUILDDIR='$jatk_builddir'"

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
