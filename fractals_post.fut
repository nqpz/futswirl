import "base"
import "fractals"

-- Mostly hacks that depend on the definitions in `fractals.fut`.

let fractal_choices = loop i = 0i32 while fractal_from_id i != #eof do i + 1

let fractal_n_transforms (f: fractal) (m: manual): i32 =
  (render_fractal f 0.0 m 0 0 0).1 -- hack!

let render_fractal' (f: fractal) (time: f32) (m: manual)
                    (height: i32) (width: i32)
                    (iterations: i32): [height][width]argb.colour =
  (render_fractal f time m height width iterations).2
