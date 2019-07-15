import "dim_utils"

module fractals (float: float_extended) = {
  open fractal_utils_2d float

  let f = float.f64

  -- Typical patterns.
  let mt (t: float) (a: float) (b: float) (c: (float, float)) (d: float): point -> point =
    rotate (a float.+ b float.* t) >-> translate c >-> scale d

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
    (rotate (f 1.505 float.+ time) >-> scale (f 0.7))
    (rotate (f (-0.4)) >-> translate (f 0.1, f 0.1) >-> scale (f 0.95))
    (scale (f 0.8) >-> translate (f 0.1, f 0))

  let dissolving_sierpinski time =
    fractal3
    (rotate (float.negate time float./ f 4) >-> translate (f 0, f (-0.25)) >-> scale (f 0.5))
    (rotate (time float./ f 4) >-> translate (f (-0.25), f 0.25) >-> scale (f 0.5))
    (rotate (time float./ f 4) >-> translate (f 0.25, f 0.25) >-> scale (f 0.5))

  let fireworks_geometry time =
    fractal2
    (rotate (f 0.5 float.+ time float./ f 5) >-> translate (f 0, f (-0.08)) >-> scale (f 0.99))
    (rotate (f (-0.5) float.- time float./ f 5) >-> translate (f 0, f 0.08) >-> scale (f 0.99))

  let plant time =
    fractal3
    (rotate (f 0 float.+ time float./ f 3) >-> translate (f (-0.4), f (-0.4)) >-> scale (f 0.9))
    (rotate (f 0.1) >-> translate (f 0.2, f 0.2) >-> scale (f 0.4))
    (rotate (f (-0.1)) >-> translate (f 0.2, f 0.2) >-> scale (f 0.3))

  let trigonometry time =
    fractal3
    (rotate (float.cos time) >-> scale (f 0.9))
    (rotate (float.sin time) >-> scale (f 0.9))
    (translate ((float.sin time float./ f 2), (float.cos time float./ f 2)) >-> scale (f 0.5))

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

  let render_fractal (f: fractal) (time: float) (m: manual)
                     (height: i32) (width: i32) (iterations: i32)
                     (vp_zoom: float) (vp_center: vec2.vector)
                     (render_approach: render_approach): float.render_result =
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
    case #dissolving_sierpinski ->
      dissolving_sierpinski time height width iterations vp_zoom vp_center render_approach
    case #fireworks_geometry ->
      fireworks_geometry time height width iterations vp_zoom vp_center render_approach
    case #plant ->
      plant time height width iterations vp_zoom vp_center render_approach
    case #trigonometry ->
      trigonometry time height width iterations vp_zoom vp_center render_approach
    case #eof -> {n_trans=0, n_points=0, n_iterations=0, rot_square_radius=float.f64 0,
                  render=replicate height (replicate width 0)}
}
