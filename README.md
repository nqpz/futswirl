# futswirl

![Screenshot](swirl.png)

Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
libraries with associated header files.


## Building and running

First run `futhark pkg sync` once (and again upon new pulls).

Then run `make run` to build and run in a window.


## Controls

  - `r`: Generate a new random fractal (show when the default fractal
    `manual` is picked).
  - Left/right: Change which fractal to show.
  - Down/up: Adjust the number of iterations.


## Contributions

Define your fractals in `fractals.fut`.
