# futswirl

![Screenshot](swirl.png)

futswirl is a small framework for working with
[IFS](https://en.wikipedia.org/wiki/Iterated_function_system) fractals.

Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
libraries with associated header files.


## Building and running

First run `futhark pkg sync` once.

Then run `make run` to build and run in a window.


## Controls

  - `1`: Generate a new random fractal (shown when the default fractal
    `random` is picked).
  - `2`, `3`, `4`: Do the same as `r`, but limit the kind of random
    fractals to 2, 3, or 4 transforms per iteration.
  - `0`: Toggle auto mode.  When enabled, futswirl will automatically
    generate new random fractals (when the default `random` is picked)
    once in a while.
  - Enter: Toggle between 2D fractals and 3D fractals.
  - Down/up: Adjust the number of iterations for the current kind of
    fractal.
  - Left/right: Change which fractal to show.
  - PageUp/PageDown: Zoom in/out.
  - Shift+Left/Right/Down/Up: Adjust the viewport center (useful when
    zooming in).
  - Space: pause/unpause


## Contributions

Define your fractals in `fractals_2d.fut` and `fractals_3d.fut`.
