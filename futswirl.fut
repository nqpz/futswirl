import "base"

module f2d = fractals (import "fractals_2d")
module f3d = fractals (import "fractals_3d")

type text_content = (i32, i32, i32, i32, i32, i32, i32, i32, i32, i32,
                     i32, i32, i32, i32, i32, f32, f32, f32, i32, i32)
module lys: lys with text_content = text_content = {
  type text_content = text_content

  type dim_info 'manual = {auto_mode: bool, cur_start: f32, fractal_id: i32,
                           manual: manual}

  type state = {height: i32, width: i32, rng: rng.rng,
                iterations2: i32, iterations3: i32, iterations4: i32,
                time: f32, vp_zoom: f32, vp_center: vec2.vector,
                paused: bool, shift_key: bool, dim: #dim2 | #dim3,
                dim2_info: dim_info f2d.manual,
                dim3_info: dim_info f3d.manual}

  module type fractals_base = {
    include fractals

    val dim_info: state -> dim_info manual
    val set_dim_info: state -> dim_info manual -> state
    val dim_id: i32
    val fid_offset: i32
  }

  module fractals_extended (f: fractals_base) = {
    -- | Set the id of the fractal to show.
    let fractal_id (s: state): i32 =
      (f.dim_info s).fractal_id % f.fractal_choices

    -- | Get the id of the fractal to show.
    let set_fractal_id (s: state) (i: i32): state =
      f.set_dim_info s (f.dim_info s with fractal_id = i)

    -- | Get the number of transforms per iteration for the current fractal.
    let n_transforms (s: state): i32 =
      f.fractal_n_transforms (f.fractal_from_id (fractal_id s))
                             (f.dim_info s).manual

    -- | Get the number of iterations for the current fractal.
    let iterations (s: state): i32 =
      if n_transforms s == 2
      then s.iterations2
      else if n_transforms s == 3
      then s.iterations3
      else s.iterations4

    -- | Set the number of iterations for the current fractal.
    let set_iterations (s: state) (iter: i32): state =
      if n_transforms s == 2 then s with iterations2 = iter
      else if n_transforms s == 3 then s with iterations3 = iter
      else s with iterations4 = iter

    let event (e: event) (s: state): state =
      match e
      case #step td ->
        let (rng, manual, cur_start) =
          if (f.dim_info s).auto_mode
          then let tdiff = s.time - (f.dim_info s).cur_start
               let (rng, x) = f32dist.rand (0, 10000 / tdiff) s.rng
               in if x < 0.95
                  -- There is 95% chance of generating a new fractal after showing
                  -- the current one for 10 seconds.
                  then let (rng, manual) = f.gen_manual rng #none
                       in (rng, manual, s.time)
                  else (rng, (f.dim_info s).manual, (f.dim_info s).cur_start)
          else (s.rng, (f.dim_info s).manual, (f.dim_info s).cur_start)
        in f.set_dim_info (s with time = (if s.paused then s.time else s.time + td)
                             with rng = rng)
                          (f.dim_info s
                           with manual = manual
                           with cur_start = cur_start)
      case #keydown {key} ->
        if key == SDLK_SPACE
        then s with paused = !s.paused
        else if key == SDLK_LSHIFT || key == SDLK_RSHIFT
        then s with shift_key = true
        else if key == SDLK_PAGEUP
        then s with vp_zoom = s.vp_zoom + 0.1
        else if key == SDLK_PAGEDOWN
        then s with vp_zoom = f32.max 0.1 (s.vp_zoom - 0.1)
        else if key == SDLK_LEFT && s.shift_key
        then s with vp_center.x = s.vp_center.x - 0.01 / (f32.sqrt s.vp_zoom)
        else if key == SDLK_RIGHT && s.shift_key
        then s with vp_center.x = s.vp_center.x + 0.01 / (f32.sqrt s.vp_zoom)
        else if key == SDLK_UP && s.shift_key
        then s with vp_center.y = s.vp_center.y - 0.01 / (f32.sqrt s.vp_zoom)
        else if key == SDLK_DOWN && s.shift_key
        then s with vp_center.y = s.vp_center.y + 0.01 / (f32.sqrt s.vp_zoom)
        else if key == SDLK_LEFT
        then set_fractal_id s (fractal_id s - 1)
        else if key == SDLK_RIGHT
        then set_fractal_id s (fractal_id s + 1)
        else if key == SDLK_DOWN
        then set_iterations s (i32.max 0 (iterations s - 1))
        else if key == SDLK_UP
        then set_iterations s (iterations s + 1)
        else if key == SDLK_RETURN || key == SDLK_KP_ENTER
        then s with dim = match s.dim
                          case #dim2 -> #dim3
                          case #dim3 -> #dim2
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
      case _ -> s

    let render (s: state) =
      let iter = iterations s
      let max_iter = max_iterations (n_transforms s)
      let iter' = i32.min iter max_iter
      in f.render_fractal (f.fractal_from_id (fractal_id s))
                          s.time (f.dim_info s).manual
                          s.height s.width iter'
                          s.vp_zoom s.vp_center

    let text_content (fps: f32) (s: state): text_content =
      let n_trans = n_transforms s
      let iter = iterations s
      let max_iter = max_iterations n_trans
      let iter' = i32.min iter max_iter
      in (f.dim_id, f.fid_offset + fractal_id s, n_trans,
          i32.bool (n_trans == 2), s.iterations2, max_iterations 2,
          i32.bool (n_trans == 3), s.iterations3, max_iterations 3,
          i32.bool (n_trans == 4), s.iterations4, max_iterations 4,
          n_trans, iter', n_trans**iter',
          s.vp_center.x, s.vp_center.y, s.vp_zoom,
          i32.bool (f.dim_info s).auto_mode, t32 fps)
  }

  module f2de = fractals_extended {
    open f2d
    let dim_id = 0i32
    let fid_offset = 0i32
    let dim_info (s: state): dim_info manual = s.dim2_info
    let set_dim_info (s: state) (d: dim_info manual): state =
      s with dim2_info = d
  }

  module f3de = fractals_extended {
    open f3d
    let dim_id = 1i32
    let fid_offset = f2d.fractal_choices
    let dim_info (s: state): dim_info manual = s.dim3_info
    let set_dim_info (s: state) (d: dim_info manual): state =
      s with dim3_info = d
  }

  let grab_mouse = false

  let init (seed: i32) (h: i32) (w: i32): state =
    let rng = rng.rng_from_seed [seed]
    let (rng, manual_2d) = f2d.gen_manual rng #none
    let (rng, manual_3d) = f3d.gen_manual rng #none
    in {height=h, width=w,
        rng=rng,
        iterations2=22, iterations3=14, iterations4=11,
        time=0.0, vp_zoom=1.0, vp_center={x=0, y=0},
        paused=false, shift_key=false, dim=#dim3, -- XXX: Should 2D or 3D be the default?
        dim2_info={auto_mode=false, cur_start=0.0,
                   fractal_id=0, manual=manual_2d},
        dim3_info={auto_mode=false, cur_start=0.0,
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

  let text_format = "Fractal (%[2D|3D]): %["
                    ++ (loop s = "" for i < f2d.fractal_choices do
                          s ++ "|" ++ f2d.fractal_name (f2d.fractal_from_id i))[1:]
                    ++ (loop s = "" for i < f3d.fractal_choices do
                          s ++ "|" ++ f3d.fractal_name (f3d.fractal_from_id i))
                    ++ "]\nTransforms: %d\n"
                    ++ "%[        |Current:] Iterations (2 transforms): %d (max: %d)\n"
                    ++ "%[        |Current:] Iterations (3 transforms): %d (max: %d)\n"
                    ++ "%[        |Current:] Iterations (4 transforms): %d (max: %d)\n"
                    ++ "Particles: %d^%d = %d\n"
                    ++ "Viewport: center (%.03f, %.03f); zoom %.03f\n"
                    ++ "Auto mode: %[disabled|enabled]\n"
                    ++ "FPS: %d"

  let text_colour = const argb.white
}
