import "base"

-- Typical pattern.
let manual time
           rotate0 tfac0 translatex0 translatey0 scale0
           rotate1 tfac1 translatex1 translatey1 scale1
           rotate2 tfac2 translatex2 translatey2 scale2 =
  fractal3
  (rotate (rotate0 + tfac0 * time)
   >-> translate (translatex0, translatey0)
   >-> scale scale0)
  (rotate (rotate1 + tfac1 * time)
   >-> translate (translatex1, translatey1)
   >-> scale scale1)
  (rotate (rotate2 + tfac2 * time)
   >-> translate (translatex2, translatey2)
   >-> scale scale2)

let swirl time =
  fractal3
  (rotate (1.505 + time) >-> scale 0.7)
  (rotate (-0.4) >-> translate (0.1, 0.1) >-> scale 0.95)
  (scale (0.8) >-> translate_x 0.1)

let dissolving_sierpinski time =
  fractal3
  (rotate (-time / 4) >-> translate (0, -0.25) >-> scale 0.5)
  (rotate (time / 4) >-> translate (-0.25, 0.25) >-> scale 0.5)
  (rotate (time / 4) >-> translate ( 0.25, 0.25) >-> scale 0.5)

let fireworks_geometry time =
  fractal2
  (rotate (0.5 + time / 5) >-> translate (0, -0.08) >-> scale 0.99)
  (rotate (-0.5 - time / 5) >-> translate (0, 0.08) >-> scale 0.99)

let plant time =
  fractal3
  (rotate (0 + time / 3) >-> translate (-0.4, -0.4) >-> scale 0.9)
  (rotate (0.1) >-> translate (0.2, 0.2) >-> scale 0.4)
  (rotate (-0.1) >-> translate (0.2, 0.2) >-> scale 0.3)

-- Note: If you add a new fractal, you need to extend both the type and the
-- three functions.

type fractal = #manual
             | #swirl
             | #dissolving_sierpinski
             | #fireworks_geometry
             | #plant
             | #eof -- end of fractals

let fractal_from_id (i: i32): fractal =
  match i
  case 0 -> #manual
  case 1 -> #swirl
  case 2 -> #dissolving_sierpinski
  case 3 -> #fireworks_geometry
  case 4 -> #plant
  case _ -> #eof

let fractal_name (f: fractal): string =
  match f
  case #manual -> "manual"
  case #swirl -> "swirl"
  case #dissolving_sierpinski -> "dissolving sierpinski"
  case #fireworks_geometry -> "fireworks geometry"
  case #plant -> "plant"
  case #eof -> ""

let render_fractal (f: fractal) (time: f32) (m: manual)
                   (height: i32) (width: i32)
                   (iterations: i32): [height][width]argb.colour =
  match f
  case #manual ->
    manual time
           m.rotate0 m.tfac0 m.translatex0 m.translatey0 m.scale0
           m.rotate1 m.tfac1 m.translatex1 m.translatey1 m.scale1
           m.rotate2 m.tfac2 m.translatex2 m.translatey2 m.scale2
           height width iterations
  case #swirl ->
    swirl time height width iterations
  case #dissolving_sierpinski ->
    dissolving_sierpinski time height width iterations
  case #fireworks_geometry ->
    fireworks_geometry time height width iterations
  case #plant ->
    plant time height width iterations
  case #eof -> replicate height (replicate width 0)
