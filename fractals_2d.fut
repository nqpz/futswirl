import "base"
open fractal_utils_2d

-- Typical patterns.
let mt (t: f32) (a: f32) (b: f32) (c: (f32, f32)) (d: f32): point -> point =
  rotate (a + b * t) >-> translate c >-> scale d

let manual2 t a0 b0 c0 d0 a1 b1 c1 d1 =
  fractal2 (mt t a0 b0 c0 d0) (mt t a1 b1 c1 d1)

let manual3 t a0 b0 c0 d0 a1 b1 c1 d1 a2 b2 c2 d2 =
  fractal3 (mt t a0 b0 c0 d0) (mt t a1 b1 c1 d1)
           (mt t a2 b2 c2 d2)

let manual4 t a0 b0 c0 d0 a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 =
  fractal4 (mt t a0 b0 c0 d0) (mt t a1 b1 c1 d1)
           (mt t a2 b2 c2 d2) (mt t a3 b3 c3 d3)

let swirl time =
  fractal3
  (rotate (1.505 + time) >-> scale 0.7)
  (rotate (-0.4) >-> translate (0.1, 0.1) >-> scale 0.95)
  (scale (0.8) >-> translate (0.1, 0))

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

let trigonometry time =
  fractal3
  (rotate (f32.cos time) >-> scale 0.9)
  (rotate (f32.sin time) >-> scale 0.9)
  (translate (f32.sin time / 2, f32.cos time / 2) >-> scale 0.5)

-- Note: If you add a new fractal, you need to extend both the type and the
-- three functions.

type fractal = #manual
             | #swirl
             | #dissolving_sierpinski
             | #fireworks_geometry
             | #plant
             | #trigonometry
             | #eof -- end of fractals
let fractals_end (f: fractal): bool = f == #eof

let fractal_from_id (i: i32): fractal =
  match i
  case 0 -> #manual
  case 1 -> #swirl
  case 2 -> #dissolving_sierpinski
  case 3 -> #fireworks_geometry
  case 4 -> #plant
  case 5 -> #trigonometry
  case _ -> #eof

let fractal_name (f: fractal): string =
  match f
  case #manual -> "random" -- more meaningful user-facing name than "manual"
  case #swirl -> "swirl"
  case #dissolving_sierpinski -> "dissolving sierpinski"
  case #fireworks_geometry -> "fireworks geometry"
  case #plant -> "plant"
  case #trigonometry -> "trigonometry"
  case #eof -> ""

let render_fractal (f: fractal) (time: f32) (m: manual)
                   (height: i32) (width: i32)
                   (iterations: i32)
                   (vp_zoom: f32) (vp_center: vec2.vector):
                   (i32, [height][width]argb.colour) =
  match f
  case #manual ->
    (match m.n_trans
     case #trans2 ->
       manual2 time
               m.rotate0 m.tfac0 m.translate0 m.scale0
               m.rotate1 m.tfac1 m.translate1 m.scale1
               height width iterations vp_zoom vp_center
     case #trans3 ->
       manual3 time
               m.rotate0 m.tfac0 m.translate0 m.scale0
               m.rotate1 m.tfac1 m.translate1 m.scale1
               m.rotate2 m.tfac2 m.translate2 m.scale2
               height width iterations vp_zoom vp_center
     case #trans4 ->
       manual4 time
               m.rotate0 m.tfac0 m.translate0 m.scale0
               m.rotate1 m.tfac1 m.translate1 m.scale1
               m.rotate2 m.tfac2 m.translate2 m.scale2
               m.rotate3 m.tfac3 m.translate3 m.scale3
               height width iterations vp_zoom vp_center)
  case #swirl ->
    swirl time height width iterations vp_zoom vp_center
  case #dissolving_sierpinski ->
    dissolving_sierpinski time height width iterations vp_zoom vp_center
  case #fireworks_geometry ->
    fireworks_geometry time height width iterations vp_zoom vp_center
  case #plant ->
    plant time height width iterations vp_zoom vp_center
  case #trigonometry ->
    trigonometry time height width iterations vp_zoom vp_center
  case #eof -> (0, replicate height (replicate width 0))
