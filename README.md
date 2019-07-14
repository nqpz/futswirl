# futswirl

[![Build Status](https://travis-ci.org/nqpz/futswirl.svg?branch=master)](https://travis-ci.org/nqpz/futswirl)

![Screenshot](swirl.png)

futswirl is a small framework for working with
[IFS](https://en.wikipedia.org/wiki/Iterated_function_system) fractals.

Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
libraries with associated header files.


## Building and running

**Initial setup:** Run `futhark pkg sync`.  Copy `settings_template.fut`
to `settings.fut` and maybe adjust values.

**General use:** Run `make run` to build and run in a window.


## Rendering approaches

futswirl supports two ways of rendering fractals:

  - **scalarloop**: Every point is calculated efficiently without any
    intermediate memory accesses.  This approach also allows rendering
    more points that can be manifested in memory, as the mapping over
    all points is fused with the later pixel rendering phase.

  - **cullbranches** (default): When zoomed in, this approach will
    choose to not calculate branches whose points are not shown.  This
    allows for much further zooming, but needs to loop over arrays, not
    scalars, and as such has more memory accesses than the
    **scalarloop** approach.  Note: You might run out of memory if you
    don't watch out.  Also, fractals have varying levels of locality, so
    this approach will not always be able to cull a lot (the printed
    "rotated square radius" value needs to be low).


## Controls

  - `ESC`: Exit the program.
  - `F1`: Toggle the text in the upper-left corner.
  - `1`: Generate a new random fractal (shown when the default fractal
    `random` is picked).
  - `2`, `3`, `4`: Do the same as `r`, but limit the kind of random
    fractals to 2, 3, or 4 transforms per iteration.
  - `0`: Toggle auto mode.  When enabled, futswirl will automatically
    generate new random fractals (when the default `random` is picked)
    once in a while.
  - `r`: Toggle between the two rendering approaches.
  - `d`: Toggle between 2D fractals and 3D fractals.
  - Left/right: Change which fractal to show.
  - Down/up: Adjust the number of iterations for the current kind of
    fractal (only affects the scalarloop rendering approach).
  - PageUp/PageDown: Zoom in/out.
  - Home: Reset viewport.
  - Left click and drag: Adjust the viewport.
  - Mouse wheel scroll: Zoom in/out.
  - Right click: Automatic zoom.
  - Right click and mouse wheel scroll: Adjust automatic zoom speed.
  - Shift+Left/Right/Down/Up: Adjust the viewport.
  - Space: Pause/unpause.

Time moves slower when zoomed in.  This keeps the fractal movement at a
manageable pace even when the viewport is small.


## Adding new fractals

Define your fractals in `fractals_2d.fut` and `fractals_3d.fut`.


## License

futswirl is free software under the terms of the GNU General Public
License version 3 (or any later version).  Copyright (C) 2019 Niels
G. W. Serup.
