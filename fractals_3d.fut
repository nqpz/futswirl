import "base"
open fractal_utils_3d

-- Typical patterns.
let mt (t: f32) (a: (f32, f32, f32)) (b: (f32, f32, f32))
       (c: (f32, f32, f32)) (d: f32): point -> point =
  rotate (a.1 + b.1 * t, a.2 + b.2 * t, a.3 + b.3 * t) >-> translate c >-> scale d

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
  (rotate (0, 0, 1.505 + time) >-> scale 0.7)
  (rotate (0, 0, -0.4) >-> translate (0.1, 0.1, 0) >-> scale 0.95)
  (scale (0.8) >-> translate (0.1, 0, 0))

let rotating_sierpinski time =
  fractal3
  (rotate (0, -time / 3, 0) >-> translate (0, -0.25, 0) >-> scale 0.5)
  (rotate (0, time / 3, 0) >-> translate (-0.25, 0.25, -0.1) >-> scale 0.5)
  (rotate (0, time / 3, 0) >-> translate ( 0.25, 0.25, 0.1) >-> scale 0.5)

-- Note: If you add a new fractal, you need to extend both the type and the
-- three functions.

type fractal = #manual
             | #swirl
             | #rotating_sierpinski
             | #eof -- end of fractals
let fractals_end (f: fractal): bool = f == #eof

let fractal_from_id (i: i32): fractal =
  match i
  case 0 -> #rotating_sierpinski -- XXX: We would probably like to have 'manual'
                                 -- first, but let's wait till it's actually
                                 -- good.
  case 1 -> #manual -- XXX: Generate better 3D fractals.
  case 2 -> #swirl
  case _ -> #eof

let fractal_name (f: fractal): string =
  match f
  case #manual -> "random" -- more meaningful user-facing name than "manual"
  case #swirl -> "swirl"
  case #rotating_sierpinski -> "rotating sierpinski"
  case #eof -> ""

let render_fractal (f: fractal) (time: f32) (m: manual)
                   (height: i32) (width: i32)
                   (iterations: i32)
                   (vp_zoom: f32) (vp_center: vec2.vector)
                   (render_approach: render_approach): render_result =
  match f
  case #manual ->
    (match m.n_trans
     case #trans2 ->
       manual2 time
               m.rotate0 m.tfac0 m.translate0 m.scale0
               m.rotate1 m.tfac1 m.translate1 m.scale1
               height width iterations vp_zoom vp_center
               render_approach
     case #trans3 ->
       manual3 time
               m.rotate0 m.tfac0 m.translate0 m.scale0
               m.rotate1 m.tfac1 m.translate1 m.scale1
               m.rotate2 m.tfac2 m.translate2 m.scale2
               height width iterations vp_zoom vp_center
               render_approach
     case #trans4 ->
       manual4 time
               m.rotate0 m.tfac0 m.translate0 m.scale0
               m.rotate1 m.tfac1 m.translate1 m.scale1
               m.rotate2 m.tfac2 m.translate2 m.scale2
               m.rotate3 m.tfac3 m.translate3 m.scale3
               height width iterations vp_zoom vp_center
               render_approach)
  case #swirl ->
    swirl time height width iterations vp_zoom vp_center render_approach
  case #rotating_sierpinski ->
    rotating_sierpinski time height width iterations vp_zoom vp_center render_approach
  case #eof -> {n_trans=0, n_points=0, n_iterations=0, rot_square_radius=0,
                render=replicate height (replicate width 0)}
