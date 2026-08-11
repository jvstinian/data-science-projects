AC_PREREQ([2.61])
AC_INIT([statistics], [0.1.0], [jvstinian@gmail.com])
# Supress warnings about code in subdirectories
AC_CONFIG_SRCDIR([src/statistics.c])
AM_INIT_AUTOMAKE([foreign tar-ustar])
# AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIRS([m4])
LT_PREREQ([2.2])
LT_INIT # ([dlopen])

# Checks for programs.
AC_PROG_CC
AC_LANG([C])
AC_PROG_INSTALL

# Checks for libraries.
AC_CHECK_LIB([m], [cos])
# AC_CHECK_LIB([pthread], [pthread_create])
# AC_CHECK_LIB([rt], [clock_gettime])

# Checks for header files.
AC_CHECK_HEADERS([limits.h stdint.h stdlib.h string.h unistd.h])

# # Checks for libraries.
# #AC_SEARCH_LIBS([pthread_create], [pthread])

# Checks for typedefs, structures, and compiler characteristics.
# AC_CHECK_HEADER_STDBOOL
# AC_TYPE_INT64_T
AC_TYPE_SIZE_T
# AC_TYPE_UINT8_T
# AC_TYPE_UINT16_T
# AC_TYPE_UINT32_T
# AC_TYPE_UINT64_T
# AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_MALLOC
# AC_CHECK_FUNCS([bzero clock_gettime fesetround gethostbyname gettimeofday memset modf pow select socket sqrt strerror strspn])

# PKG_CHECK_MODULES([CHECK], [check])
# AC_SUBST([CHECK_CFLAGS])
# AC_SUBST([CHECK_LIBS])
# 
# PKG_CHECK_MODULES([CJSON], [libcjson])
# 
# PKG_CHECK_MODULES([MYSQL], [mysqlclient])

AC_CONFIG_FILES([Makefile
    src/Makefile
    vendor/Makefile
    vendor/mtwister/Makefile ])
AC_OUTPUT

echo \
"-------------------------------------------------

 ${PACKAGE_NAME} Version ${PACKAGE_VERSION}

 Prefix: '${prefix}'.
 Compiler: '${CC} ${CFLAGS}'
 CFLAGS: '${CFLAGS}'
 LDFLAGS: '${LDFLAGS}'
 Libs: '${LIBS}'

 Now type 'make @<:@<target>@:>@'
   where the optional <target> is:
     all                - build all binaries
     install            - install everything

--------------------------------------------------"
