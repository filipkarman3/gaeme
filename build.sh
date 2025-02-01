stack clean
stack build \
  --extra-include-dirs=$HOME/sdl2/include \
  --extra-lib-dirs=$HOME/sdl2/lib \
  --extra-lib-dirs=$HOME/sdl2/lib/pkgconfig
