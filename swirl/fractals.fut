import "base"
import "manual"

module type fractals_base = {
  type float
  type vec2_vector
  type manual_base 'f
  type manual
  type fractal

  val gen_manual: rng.rng -> gen_manual_constraint -> (rng.rng, manual_base f64)

  val manual_from_f64: manual_base f64 -> manual_base f32

  val fractals_end: fractal -> bool

  val fractal_from_id: i32 -> fractal
  val fractal_name: fractal -> string
  val render_fractal: fractal -> float -> manual -> i32 -> i32 -> i32 ->
                      float -> vec2_vector -> render_approach ->
                      render_result_base float
}

module fractals_wrapper (manual: manual)
                        (b32: fractals_base
                              with float = f32
                              with manual = manual.manual_base f32
                              with vec2_vector = vec2_f32.vector)
                        (b64: fractals_base
                              with float = f64
                              with manual = manual.manual_base f64
                              with vec2_vector = vec2_f64.vector
                              with fractal = b32.fractal) = {

  type fractal = b32.fractal
  let fractal_from_id = b32.fractal_from_id
  let fractal_name = b32.fractal_name

  type manual_base 't = manual.manual_base t
  type manual32 = manual_base f32
  type manual64 = manual_base f64
  type manual = (manual32, manual64)

  let fractal_choices = loop i = 0i32
                        while ! (b32.fractals_end (b32.fractal_from_id i))
                        do i + 1

  let gen_manual (rng: rng.rng) (c: gen_manual_constraint): (rng.rng, manual) =
    let (rng, manual64) = manual.gen_manual rng c
    let manual32 = manual.manual_from_f64 manual64
    in (rng, (manual32, manual64))

  let render_fractal (fb: float_bits) (f: fractal) (time: f64) (m: manual)
                     (height: i32) (width: i32) (iterations: i32)
                     (vp_zoom: f64) (vp_center: vec2_f64.vector)
                     (render_approach: render_approach): f64e.render_result =
    match fb
    case #f32 ->
      let res = b32.render_fractal f (f32.f64 time) m.1 height width iterations (f32.f64 vp_zoom) {x=f32.f64 vp_center.x, y=f32.f64 vp_center.y} render_approach
      in {n_trans=res.n_trans, n_points=res.n_points, n_iterations=res.n_iterations,
          rot_square_radius=f64.f32 res.rot_square_radius, render=res.render}
    case #f64 ->
      b64.render_fractal f time m.2 height width iterations vp_zoom vp_center render_approach
}

-- module fractals_wrapper_2d (b32: fractals_base
--                                  with float = f32
--                                  with manual = manual_2d.manual_base f32
--                                  with vec2_vector = vec2_f32.vector)
--                            (b64: fractals_base
--                                  with float = f64
--                                  with manual = manual_2d.manual_base f64
--                                  with vec2_vector = vec2_f64.vector
--                                  with fractal = b32.fractal) = {
--   module manual = manual_2d
--   type fractal = b32.fractal
--   let fractal_from_id = b32.fractal_from_id
--   let fractal_name = b32.fractal_name

--   type manual_base 't = manual.manual_base t
--   type manual32 = manual_base f32
--   type manual64 = manual_base f64
--   type manual = (manual32, manual64)

--   let fractal_choices = loop i = 0i32
--                         while ! (b32.fractals_end (b32.fractal_from_id i))
--                         do i + 1

--   let gen_manual (rng: rng.rng) (c: gen_manual_constraint): (rng.rng, manual) =
--     let (rng, manual64) = manual.gen_manual rng c
--     let manual32 = manual.manual_from_f64 manual64
--     in (rng, (manual32, manual64))

--   let render_fractal (fb: float_bits) (f: fractal) (time: f64) (m: manual)
--                      (height: i32) (width: i32) (iterations: i32)
--                      (vp_zoom: f64) (vp_center: vec2_f64.vector)
--                      (render_approach: render_approach): f64e.render_result =
--     match fb
--     case #f32 ->
--       let res = b32.render_fractal f (f32.f64 time) m.1 height width iterations (f32.f64 vp_zoom) {x=f32.f64 vp_center.x, y=f32.f64 vp_center.y} render_approach
--       in {n_trans=res.n_trans, n_points=res.n_points, n_iterations=res.n_iterations,
--           rot_square_radius=f64.f32 res.rot_square_radius, render=res.render}
--     case #f64 ->
--       b64.render_fractal f time m.2 height width iterations vp_zoom vp_center render_approach
-- }

-- module fractals_wrapper_3d (b32: fractals_base
--                                  with float = f32
--                                  with manual = manual_3d.manual_base f32
--                                  with vec2_vector = vec2_f32.vector)
--                            (b64: fractals_base
--                                  with float = f64
--                                  with manual = manual_3d.manual_base f64
--                                  with vec2_vector = vec2_f64.vector
--                                  with fractal = b32.fractal) = {
--   module manual = manual_3d
--   type fractal = b32.fractal
--   let fractal_from_id = b32.fractal_from_id
--   let fractal_name = b32.fractal_name

--   type manual_base 't = manual.manual_base t
--   type manual32 = manual_base f32
--   type manual64 = manual_base f64
--   type manual = (manual32, manual64)

--   let fractal_choices = loop i = 0i32
--                         while ! (b32.fractals_end (b32.fractal_from_id i))
--                         do i + 1

--   let gen_manual (rng: rng.rng) (c: gen_manual_constraint): (rng.rng, manual) =
--     let (rng, manual64) = manual.gen_manual rng c
--     let manual32 = manual.manual_from_f64 manual64
--     in (rng, (manual32, manual64))

--   let render_fractal (fb: float_bits) (f: fractal) (time: f64) (m: manual)
--                      (height: i32) (width: i32) (iterations: i32)
--                      (vp_zoom: f64) (vp_center: vec2_f64.vector)
--                      (render_approach: render_approach): f64e.render_result =
--     match fb
--     case #f32 ->
--       let res = b32.render_fractal f (f32.f64 time) m.1 height width iterations (f32.f64 vp_zoom) {x=f32.f64 vp_center.x, y=f32.f64 vp_center.y} render_approach
--       in {n_trans=res.n_trans, n_points=res.n_points, n_iterations=res.n_iterations,
--           rot_square_radius=f64.f32 res.rot_square_radius, render=res.render}
--     case #f64 ->
--       b64.render_fractal f time m.2 height width iterations vp_zoom vp_center render_approach
-- }
