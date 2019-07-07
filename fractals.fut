import "base"

-- Typical patterns.
let mt t a b c d e =
  rotate (a + b * t) >-> translate (c, d) >-> scale e

let manual2 t a0 b0 c0 d0 e0 a1 b1 c1 d1 e1 =
  fractal2 (mt t a0 b0 c0 d0 e0) (mt t a1 b1 c1 d1 e1)

let manual3 t a0 b0 c0 d0 e0 a1 b1 c1 d1 e1 a2 b2 c2 d2 e2 =
  fractal3 (mt t a0 b0 c0 d0 e0) (mt t a1 b1 c1 d1 e1)
           (mt t a2 b2 c2 d2 e2)

let manual4 t a0 b0 c0 d0 e0 a1 b1 c1 d1 e1 a2 b2 c2 d2 e2 a3 b3 c3 d3 e3 =
  fractal4 (mt t a0 b0 c0 d0 e0) (mt t a1 b1 c1 d1 e1)
           (mt t a2 b2 c2 d2 e2) (mt t a3 b3 c3 d3 e3)

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
  case #manual -> "random" -- more meaningful user-facing name than "manual"
  case #swirl -> "swirl"
  case #dissolving_sierpinski -> "dissolving sierpinski"
  case #fireworks_geometry -> "fireworks geometry"
  case #plant -> "plant"
  case #eof -> ""

let render_fractal (f: fractal) (time: f32) (m: manual)
                   (height: i32) (width: i32)
                   (iterations: i32): (i32, [height][width]argb.colour) =
  match f
  case #manual ->
    if m.n_trans == 2
    then manual2 time
                 m.rotate0 m.tfac0 m.translatex0 m.translatey0 m.scale0
                 m.rotate1 m.tfac1 m.translatex1 m.translatey1 m.scale1
                 height width iterations
    else if m.n_trans == 3
    then manual3 time
                 m.rotate0 m.tfac0 m.translatex0 m.translatey0 m.scale0
                 m.rotate1 m.tfac1 m.translatex1 m.translatey1 m.scale1
                 m.rotate2 m.tfac2 m.translatex2 m.translatey2 m.scale2
                 height width iterations
    else manual4 time
                 m.rotate0 m.tfac0 m.translatex0 m.translatey0 m.scale0
                 m.rotate1 m.tfac1 m.translatex1 m.translatey1 m.scale1
                 m.rotate2 m.tfac2 m.translatex2 m.translatey2 m.scale2
                 m.rotate3 m.tfac3 m.translatex3 m.translatey3 m.scale3
                 height width iterations
  case #swirl ->
    swirl time height width iterations
  case #dissolving_sierpinski ->
    dissolving_sierpinski time height width iterations
  case #fireworks_geometry ->
    fireworks_geometry time height width iterations
  case #plant ->
    plant time height width iterations
  case #eof -> (0, replicate height (replicate width 0))
