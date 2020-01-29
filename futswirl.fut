import "swirl/base"
import "swirl/manual"
import "swirl/fractals"
import "swirl/float_dual"

module f2d_base = import "swirl/fractals_2d"
module f3d_base = import "swirl/fractals_3d"

module f2d = fractals_wrapper manual_2d (f2d_base.fractals f32e) (f2d_base.fractals f64e)
module f3d = fractals_wrapper manual_3d (f3d_base.fractals f32e) (f3d_base.fractals f64e)

type text_content = (i32, i32, i32, i32, i32, i32, i32, i32, i32, f32, i32, f32, f32, f32,
                     i32, i32, i32, f32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32)
module lys: lys with text_content = text_content = {
  type text_content = text_content

  type dim_info 'manual32 'manual64 =
    {auto_mode: bool, cur_start: float_dual, fractal_id: i32,
     manual: (manual32, manual64)}

  -- A hack because we need this type in a module type as well.
  type sized_state [h][w] =
    {height: i32, width: i32, rng: rng,
     iterations2: i32, iterations3: i32, iterations4: i32,
     time: float_dual, vp_zoom: float_dual, vp_center: vec2_float_dual.vector,
     render: render_result_base [h][w] float_dual, render_approach: render_approach,
     paused: bool, shift_key: bool, mouse: (i32, i32),
     auto_zoom: bool, auto_zoom_zoom_factor: float_dual,
     float_bits: float_bits,
     dim: #dim2 | #dim3,
     dim2_info: dim_info f2d.manual32 f2d.manual64,
     dim3_info: dim_info f3d.manual32 f3d.manual64}

  type~ state = sized_state [][]

  module type lys_fractals_base = {
    type fractal
    type manual32
    type manual64
    type manual = (manual32, manual64)

    val gen_manual: rng -> gen_manual_constraint -> (rng, manual)

    val fractal_from_id: i32 -> fractal
    val fractal_name: fractal -> string []
    val render_fractal: float_bits -> fractal -> float_dual -> manual ->
                        (h: i32) -> (w: i32) -> i32 ->
                        float_dual -> vec2_float_dual.vector -> render_approach ->
                        render_result_base [h][w] float_dual

    val fractal_choices: i32
    val dim_info [h][w]: sized_state [h][w] -> dim_info manual32 manual64
    val set_dim_info [h][w]: sized_state [h][w] -> dim_info manual32 manual64 -> sized_state [h][w]
    val dim_id: i32
    val fid_offset: i32
  }

  module fractals_extended (f: lys_fractals_base) = {
    -- | Set the id of the fractal to show.
    let fractal_id (s: state): i32 =
      (f.dim_info s).fractal_id % f.fractal_choices

    -- | Get the id of the fractal to show.
    let set_fractal_id (s: state) (i: i32): state =
      f.set_dim_info s (f.dim_info s with fractal_id = i)

    -- | Get the number of iterations for the current fractal.
    let scalarloop_iterations (s: state): i32 =
      if s.render.n_trans == 2
      then s.iterations2
      else if s.render.n_trans == 3
      then s.iterations3
      else s.iterations4

    -- We are currently limited by Futhark using 32-bit integers.  We could work
    -- around this, but in practice having that many particles is very slow, so
    -- we we use this check for now.  Only applies to the scalarloop render
    -- approach.
    let max_iterations (n_trans: i32): i32 =
      t32 (f32.log (2**31 - 4096) / f32.log (r32 n_trans))

    -- | Set the number of iterations for the current fractal.
    let set_scalarloop_iterations (s: state) (iter: i32): state =
      if s.render.n_trans == 2 then s with iterations2 = iter
      else if s.render.n_trans == 3 then s with iterations3 = iter
      else s with iterations4 = iter

    let zoom_at_mouse (zoom_factor: float_dual) (s: state): state =
      let xy_factor = float_dual.i32 (i32.min s.height s.width) float_dual.* s.vp_zoom
      let xb = float_dual.i32 (s.mouse.0 - s.width / 2)
      let xd = xb float_dual./ xy_factor float_dual.- xb float_dual./ (xy_factor float_dual.* zoom_factor)
      let yb = float_dual.i32 (s.mouse.1 - s.height / 2)
      let yd = yb float_dual./ xy_factor float_dual.- yb float_dual./ (xy_factor float_dual.* zoom_factor)
      in s with vp_zoom = s.vp_zoom float_dual.* zoom_factor
           with vp_center.x = s.vp_center.x float_dual.+ xd
           with vp_center.y = s.vp_center.y float_dual.+ yd

    let event (e: event) (s: state): state =
      match e
      case #step td ->
        let (rng, manual, cur_start) =
          if (f.dim_info s).auto_mode
          then let tdiff = s.time float_dual.- (f.dim_info s).cur_start
               let (rng, x) = f32dist.rand (0, 10000 / float_dual.to_f32 tdiff) s.rng
               in if x < 0.95
                  -- There is 95% chance of generating a new fractal after showing
                  -- the current one for 10 seconds.
                  then let (rng, manual) = f.gen_manual rng #none
                       in (rng, manual, s.time)
                  else (rng, (f.dim_info s).manual, (f.dim_info s).cur_start)
          else (s.rng, (f.dim_info s).manual, (f.dim_info s).cur_start)
        let s = s with time = (if s.paused then s.time else s.time float_dual.+ float_dual.f32 td float_dual./ s.vp_zoom)
                  with rng = rng
                  with render = let iter = scalarloop_iterations s
                                let iter' = match s.render_approach
                                            case #scalarloop ->
                                              let max_iter = max_iterations (s.render.n_trans)
                                              in i32.min iter max_iter
                                            case #cullbranches -> iter
                                in f.render_fractal s.float_bits (f.fractal_from_id (fractal_id s))
                                                    s.time (f.dim_info s).manual
                                                    s.height s.width iter'
                                                    s.vp_zoom s.vp_center
                                                    s.render_approach
        let s = if s.auto_zoom
                then zoom_at_mouse s.auto_zoom_zoom_factor s
                else s
        in f.set_dim_info s (f.dim_info s
                             with manual = manual
                             with cur_start = cur_start)
      case #keydown {key} ->
        if key == SDLK_SPACE
        then s with paused = !s.paused
        else if key == SDLK_LSHIFT || key == SDLK_RSHIFT
        then s with shift_key = true
        else if key == SDLK_PAGEUP
        then s with vp_zoom = s.vp_zoom float_dual.* (fdc 1.1)
        else if key == SDLK_PAGEDOWN
        then s with vp_zoom = float_dual.max (fdc 0.001) (s.vp_zoom float_dual./ (fdc 1.1))
        else if key == SDLK_HOME
        then s with vp_zoom = fdc 1
               with vp_center = {x=fdc 0, y=fdc 0}
        else if key == SDLK_LEFT && s.shift_key
        then s with vp_center.x = s.vp_center.x float_dual.- fdc 0.01 float_dual./ s.vp_zoom
        else if key == SDLK_RIGHT && s.shift_key
        then s with vp_center.x = s.vp_center.x float_dual.+ fdc 0.01 float_dual./ s.vp_zoom
        else if key == SDLK_UP && s.shift_key
        then s with vp_center.y = s.vp_center.y float_dual.- fdc 0.01 float_dual./ s.vp_zoom
        else if key == SDLK_DOWN && s.shift_key
        then s with vp_center.y = s.vp_center.y float_dual.+ fdc 0.01 float_dual./ s.vp_zoom
        else if key == SDLK_LEFT
        then set_fractal_id s (fractal_id s - 1)
        else if key == SDLK_RIGHT
        then set_fractal_id s (fractal_id s + 1)
        else if key == SDLK_DOWN
        then set_scalarloop_iterations s (i32.max 0 (scalarloop_iterations s - 1))
        else if key == SDLK_UP
        then set_scalarloop_iterations s (scalarloop_iterations s + 1)
        else if key == SDLK_d
        then s with dim = match s.dim
                          case #dim2 -> #dim3
                          case #dim3 -> #dim2
        else if key == SDLK_f
        then s with float_bits = if settings.enable_f64
                                 then match s.float_bits
                                      case #f32 -> #f64
                                      case #f64 -> #f32
                                 else s.float_bits
        else if key == SDLK_r
        then s with render_approach = match s.render_approach
                                      case #scalarloop -> #cullbranches
                                      case #cullbranches -> #scalarloop
        else if key == SDLK_0 || key == SDLK_KP_0
        then f.set_dim_info s (f.dim_info s
                               with auto_mode = !(f.dim_info s).auto_mode)
        else let (k1, k2, k3, k4) =
               (key == SDLK_1 || key == SDLK_KP_1,
                key == SDLK_2 || key == SDLK_KP_2,
                key == SDLK_3 || key == SDLK_KP_3,
                key == SDLK_4 || key == SDLK_KP_4)
             in if k1 || k2 || k3 || k4
                then let c = if k1 then #none
                             else if k2 then #trans 2i32
                             else if k3 then #trans 3i32
                             else #trans 4i32
                     let (rng, manual) = f.gen_manual s.rng c
                     in f.set_dim_info (s with rng = rng)
                                       (f.dim_info s with manual = manual
                                                     with cur_start = s.time)
                else s
      case #keyup {key} ->
        if key == SDLK_LSHIFT || key == SDLK_RSHIFT
        then s with shift_key = false
        else s
      case #mouse {buttons, x, y} ->
        let x_diff = s.mouse.0 - x
        let y_diff = s.mouse.1 - y
        let s = s with mouse = (x, y)

        let xy_factor = float_dual.i32 (i32.min s.height s.width) float_dual.* s.vp_zoom

        let s = if buttons & 1 == 1 || buttons & 4 == 4
                then s with vp_center.x = s.vp_center.x float_dual.+ float_dual.i32 x_diff float_dual./ xy_factor
                       with vp_center.y = s.vp_center.y float_dual.+ float_dual.i32 y_diff float_dual./ xy_factor
                else s
        let s = if buttons & 4 == 4
                then s with auto_zoom = true
                else s with auto_zoom = false
        in s
      case #wheel {dx=_, dy} ->
        if s.auto_zoom
        then s with auto_zoom_zoom_factor = s.auto_zoom_zoom_factor float_dual.+ float_dual.i32 dy float_dual.* fdc 0.01
        else let zoom_factor = fdc 1 float_dual.+ float_dual.i32 dy float_dual.* fdc 0.01
             in zoom_at_mouse zoom_factor s
      case _ -> s

    let render (s: state) =
      s.render.render

    let text_content (fps: f32) (s: state): text_content =
      let n_trans = s.render.n_trans
      in (i32.bool (f.dim_id == 0), i32.bool (f.dim_id == 1),
          i32.bool (s.float_bits == #f32), i32.bool (s.float_bits == #f64),
          f.fid_offset + fractal_id s,
          n_trans,
          s.render.n_iterations,
          n_trans, s.render.n_iterations, f32.i32 n_trans**f32.i32 s.render.n_iterations,
          s.render.n_points,
          float_dual.to_f32 s.vp_center.x, float_dual.to_f32 s.vp_center.y, float_dual.to_f32 s.vp_zoom,
          i32.bool (f.dim_info s).auto_mode,
          t32 fps,
          i32.bool (s.render_approach == #cullbranches),
          float_dual.to_f32 s.render.rot_square_radius,
          i32.bool (s.render_approach == #scalarloop),
          i32.bool (n_trans == 2), s.iterations2, max_iterations 2,
          i32.bool (n_trans == 3), s.iterations3, max_iterations 3,
          i32.bool (n_trans == 4), s.iterations4, max_iterations 4)
  }

  module f2de = fractals_extended {
    open f2d
    let dim_id = 0i32
    let fid_offset = 0i32
    let dim_info (s: state): dim_info manual32 manual64 = s.dim2_info
    let set_dim_info (s: state) (d: dim_info manual32 manual64): state =
      s with dim2_info = d
  }

  module f3de = fractals_extended {
    open f3d
    let dim_id = 1i32
    let fid_offset = f2d.fractal_choices
    let dim_info (s: state): dim_info manual32 manual64 = s.dim3_info
    let set_dim_info (s: state) (d: dim_info manual32 manual64): state =
      s with dim3_info = d
  }

  let grab_mouse = false

  let init (seed: u32) (h: i32) (w: i32): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let (rng, manual_2d) = f2d.gen_manual rng (#trans 3)
    let (rng, manual_3d) = f3d.gen_manual rng (#trans 3)
    in {height=h, width=w,
        rng=rng,
        iterations2=settings.iterations2,
        iterations3=settings.iterations3,
        iterations4=settings.iterations4,
        time=fdc 0, vp_zoom=fdc 1, vp_center={x=fdc 0, y=fdc 0},
        paused=false, shift_key=false,
        mouse=(0, 0), auto_zoom=false, auto_zoom_zoom_factor=fdc 1.01,
        render={n_trans=0, n_points=0, n_iterations=0,
                rot_square_radius=fdc 0,
                render=replicate h (replicate w 0)},
        render_approach=#cullbranches,
        float_bits=#f32,
        dim=#dim2,
        dim2_info={auto_mode=false, cur_start=fdc 0,
                   fractal_id=0, manual=manual_2d},
        dim3_info={auto_mode=false, cur_start=fdc 0,
                   fractal_id=0, manual=manual_3d}}

  let resize h w (s: state) =
    s with height = h with width = w

  let event (e: event) (s: state): state =
    match s.dim
    case #dim2 -> f2de.event e s
    case #dim3 -> f3de.event e s

  let render (s: state) =
    match s.dim
    case #dim2 -> f2de.render s
    case #dim3 -> f3de.render s

  let text_content (fps: f32) (s: state): text_content =
    match s.dim
    case #dim2 -> f2de.text_content fps s
    case #dim3 -> f3de.text_content fps s

  let text_format () =
    "Dimensions: [%[ |X]] 2D  [%[ |X]] 3D\n"
    ++ "Float size:" ++ " [%[ |X]] 32 bits" ++ (if settings.enable_f64 then "  [%[ |X]] 64 bits" else "%[]") ++ "\n"
    ++ "Fractal: %["
    ++ (loop s = "" for i < f2d.fractal_choices do
          s ++ "|" ++ f2d.fractal_name (f2d.fractal_from_id i))[1:]
    ++ (loop s = "" for i < f3d.fractal_choices do
          s ++ "|" ++ f3d.fractal_name (f3d.fractal_from_id i))
    ++ "]\n"
    ++ "Branch factor: %d\n"
    ++ "Iterations: %d\n"
    ++ "Particles (without culling): %d^%d ≈ %.03le\n"
    ++ "Particles (with culling): %d\n"
    ++ "Viewport: center (%.03le, %.03le); zoom %.03le\n"
    ++ "Auto mode: %[disabled|enabled]\n"
    ++ "FPS: %d\n"
    ++ "\n"
    ++ "[%[ |X]] cullbranches rendering:\n"
    ++ "Rotated square radius: %.03f\n"
    ++ "\n"
    ++ "[%[ |X]] scalarloop rendering:\n"
    ++ "%[        |Current:] Iterations (2 transforms): %d (max: %d)\n"
    ++ "%[        |Current:] Iterations (3 transforms): %d (max: %d)\n"
    ++ "%[        |Current:] Iterations (4 transforms): %d (max: %d)"

  let text_colour = const argb.white
}
