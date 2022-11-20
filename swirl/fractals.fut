import "../lib/github.com/diku-dk/lys/lys"
import "base"
import "manual"
import "float_dual"

module type fractals_base = {
  type float
  type vec2_vector
  type manual_base 'f
  type manual
  type fractal

  val gen_manual: rng -> gen_manual_constraint -> (rng, manual_base f64)

  val manual_from_f64: manual_base f64 -> manual_base f32

  val fractals_end: fractal -> bool

  val fractal_from_id: i32 -> fractal
  val fractal_name: fractal -> string []
  val render_fractal: fractal -> float -> manual ->
                      (h: i64) -> (w: i64) -> i32 ->
                      float -> vec2_vector -> render_approach ->
                      render_result_base [h][w] float
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
  def fractal_from_id = b32.fractal_from_id
  def fractal_name = b32.fractal_name

  type manual_base 't = manual.manual_base t
  type manual32 = manual_base f32
  type manual64 = manual_base f64
  type manual = (manual32, manual64)

  def fractal_choices = loop i = 0i32
                        while ! (b32.fractals_end (b32.fractal_from_id i))
                        do i + 1

  def gen_manual (rng: rng) (c: gen_manual_constraint): (rng, manual) =
    let (rng, manual64) = manual.gen_manual rng c
    let manual32 = manual.manual_from_f64 manual64
    in (rng, (manual32, manual64))

  def render_fractal (fb: float_bits) (f: fractal) (time: float_dual) (m: manual)
                     (height: i64) (width: i64) (iterations: i32)
                     (vp_zoom: float_dual) (vp_center: vec2_float_dual.vector)
                     (render_approach: render_approach): render_result_base [height][width] float_dual =
    match fb
    case #f32 ->
      let res = b32.render_fractal f (float_dual.to_f32 time) m.0 height width iterations
                (float_dual.to_f32 vp_zoom) {x=float_dual.to_f32 vp_center.x, y=float_dual.to_f32 vp_center.y}
                render_approach
      in {n_trans=res.n_trans, n_points=res.n_points, n_iterations=res.n_iterations,
          rot_square_radius={f32=res.rot_square_radius, f64=f64.f32 res.rot_square_radius}, render=res.render}
    case #f64 ->
      if settings.enable_f64
      then let res = b64.render_fractal f (float_dual.to_f64 time) m.1 height width iterations
                     (float_dual.to_f64 vp_zoom) {x=float_dual.to_f64 vp_center.x, y=float_dual.to_f64 vp_center.y}
                     render_approach
           in {n_trans=res.n_trans, n_points=res.n_points, n_iterations=res.n_iterations,
               rot_square_radius={f32=f32.f64 res.rot_square_radius, f64=res.rot_square_radius}, render=res.render}
      else {n_trans=0, n_points=0, n_iterations=0, rot_square_radius=fdc 0,
            render=replicate height (replicate width 0)} -- dummy value
}
