import "base"

let swirl time =
  fractal3
  (rotate (1.505 + time) >-> scale 0.7)
  (rotate (-0.4) >-> translate (0.1, 0.1) >-> scale 0.95)
  (scale (0.8) >-> translate_x 0.1)

let dissolving_sierpinski time =
  fractal3
  (rotate (-time / 4) >-> translate ( 0,   -0.25) >-> scale 0.5)
  (rotate (time / 4) >-> translate (-0.25, 0.25) >-> scale 0.5)
  (rotate (time / 4) >-> translate ( 0.25, 0.25) >-> scale 0.5)

let fireworks_geometry time =
  fractal2
  (rotate (0.5 + time / 5) >-> translate (0, -0.08) >-> scale 0.99)
  (rotate (-0.5 - time / 5) >-> translate (0, 0.08) >-> scale 0.99)

-- Note: If you add a new fractal, you need to extend both the type and the
-- three functions.

type fractal = #swirl
             | #dissolving_sierpinski
             | #fireworks_geometry
             | #eof -- end of fractals

let fractal_from_id (i: i32): fractal =
  match i
  case 0 -> #swirl
  case 1 -> #dissolving_sierpinski
  case 2 -> #fireworks_geometry
  case _ -> #eof

let fractal_name (f: fractal): string =
  match f
  case #swirl -> "swirl"
  case #dissolving_sierpinski -> "dissolving sierpinski"
  case #fireworks_geometry -> "fireworks geometry"
  case #eof -> ""

let render_fractal (f: fractal) (time: f32) (_rand: f32) -- XXX: use rand for fun
                   (height: i32) (width: i32)
                   (iterations: i32): [height][width]argb.colour =
  match f
  case #swirl ->
    swirl time height width iterations
  case #dissolving_sierpinski ->
    dissolving_sierpinski time height width iterations
  case #fireworks_geometry ->
    fireworks_geometry time height width iterations
  case #eof -> []
