AC_INIT([SDL2-gfx], [0.1.0], [joelburget@gmail.com])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([includes/HsSDLConfig.h.in])

# Header file to place defines in
AC_CONFIG_HEADERS([includes/HsSDLConfig.h])

AC_PATH_PROGS([SDL_CONFIG], [sdl2-config sdl20-config], [none])

if test "x$SDL_CONFIG" = "xnone"; then

   AC_MSG_ERROR([*** SDL not found! Get SDL from www.libsdl.org.
If you already installed it, check it's in the path. If problem remains,
please send a mail to the address that appears in ./configure --version
indicating your platform, the version of configure script and the problem.])

fi

AC_CHECK_LIB([SDL_gfx],
             [SDL_initFramerate],
                  [if test -n "$LDPREFIX" -a -r `$SDL_CONFIG --prefix`/lib/libSDL_gfx.la
then SDL_LIBS=`$SDL_CONFIG --prefix`/lib/libSDL_gfx.la
else SDL_LIBS=-lSDL_gfx
fi],
             [AC_MSG_ERROR([*** SDL_gfx lib not found! Get SDL_gfx from
http://www.ferzkopp.net/joomla/content/view/19/14/])])
SDL_CFLAGS=`$SDL_CONFIG --cflags`

AC_SUBST([SDL_CFLAGS])
AC_SUBST([SDL_LIBS])

AC_CONFIG_FILES([config.mk SDL-gfx.buildinfo])
AC_OUTPUT
