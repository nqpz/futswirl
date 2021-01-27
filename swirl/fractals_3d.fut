import "../lib/github.com/diku-dk/lys/lys"
import "base"
import "dim_utils"

module fractals (float: float_extended) = {
  open fractal_utils_3d float

  let f = float.f64

  -- Typical patterns.
  let mt (t: float) (a: (float, float, float)) (b: (float, float, float))
         (c: (float, float, float)) (d: float): point -> point =
    rotate (a.0 float.+ b.0 float.* t, a.1 float.+ b.1 float.* t, a.2 float.+ b.2 float.* t) >-> translate c >-> scale d

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
    (rotate (f 0, f 0, f 1.505 float.+ time) >-> scale (f 0.7))
    (rotate (f 0, f 0, f (-0.4)) >-> translate (f 0.1, f 0.1, f 0) >-> scale (f 0.95))
    (scale (f 0.8) >-> translate (f 0.1, f 0, f 0))

  let rotating_sierpinski time =
    fractal3
    (rotate (f 0, (float.negate (time float./ f 3)), f 0) >-> translate (f 0, f (-0.25), f 0) >-> scale (f 0.5))
    (rotate (f 0, (time float./ f 3), f 0) >-> translate (f (-0.25), f (0.25), f (-0.1)) >-> scale (f 0.5))
    (rotate (f 0, (time float./ f 3), f 0) >-> translate (f 0.25, f 0.25, f 0.1) >-> scale (f 0.5))

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

  let fractal_name (f: fractal): string [] =
    match f
    case #manual -> "random" -- more meaningful user-facing name than "manual"
    case #swirl -> "swirl"
    case #rotating_sierpinski -> "rotating sierpinski"
    case #eof -> ""

  let render_fractal (f: fractal) (time: float) (m: manual)
                     (height: i64) (width: i64) (iterations: i32)
                     (vp_zoom: float) (vp_center: vec2.vector)
                     (render_approach: render_approach): float.render_result [height][width] =
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
    case #eof -> {n_trans=0, n_points=0, n_iterations=0, rot_square_radius=float.f64 0,
                  render=replicate height (replicate width 0)}
}
