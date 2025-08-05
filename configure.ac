AC_INIT([guile-cps-debugger], [0.1.0], [bugs@example.org])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign subdir-objects])

# Check for programs
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MKDIR_P

# Check for Guile 3.0
GUILE_PKG([3.0])
GUILE_FLAGS
GUILE_PROGS
GUILE_SITE_DIR
GUILE_EXTENSION

# Check for required Guile modules
GUILE_MODULE_REQUIRED([srfi srfi-64])
GUILE_MODULE_REQUIRED([ice-9 match])
GUILE_MODULE_REQUIRED([ice-9 format])

# Optional features
AC_ARG_ENABLE([visualization],
  [AS_HELP_STRING([--enable-visualization], [Enable visualization support (default: yes)])],
  [enable_visualization=$enableval],
  [enable_visualization=yes])

AC_ARG_ENABLE([memory-profiling],
  [AS_HELP_STRING([--enable-memory-profiling], [Enable memory profiling support (default: yes)])],
  [enable_memory_profiling=$enableval],
  [enable_memory_profiling=yes])

# Define conditionals
AM_CONDITIONAL([ENABLE_VISUALIZATION], [test "x$enable_visualization" = "xyes"])
AM_CONDITIONAL([ENABLE_MEMORY_PROFILING], [test "x$enable_memory_profiling" = "xyes"])

# Output files
AC_CONFIG_FILES([
  Makefile
  src/Makefile
  tests/Makefile
  docs/Makefile
  examples/Makefile
])

AC_OUTPUT

# Print configuration summary
echo ""
echo "Configuration summary:"
echo "====================="
echo "  Package:              $PACKAGE_NAME"
echo "  Version:              $PACKAGE_VERSION"
echo "  Guile version:        $GUILE_EFFECTIVE_VERSION"
echo "  Installation prefix:  $prefix"
echo "  Guile site dir:       $GUILE_SITE"
echo "  Guile site ccache:    $GUILE_SITE_CCACHE"
echo ""
echo "  Features:"
echo "    Visualization:      $enable_visualization"
echo "    Memory profiling:   $enable_memory_profiling"
echo ""