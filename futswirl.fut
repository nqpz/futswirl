import "base"
import "fractals_post"

type text_content = (i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32)
module lys: lys with text_content = text_content = {
  type text_content = text_content

  type state = {height: i32, width: i32,
                rng: rng.rng,
                iterations2: i32, iterations3: i32, iterations4: i32,
                time: f32, fractal_id: i32, manual: manual,
                auto_mode: bool}

  -- | Get the number of transforms per iteration for the current fractal.
  let n_transforms (s: state): i32 =
    fractal_n_transforms (fractal_from_id s.fractal_id) s.manual

  -- | Get the number of iterations for the current fractal.
  let get_iterations (s: state): i32 =
    if n_transforms s == 2
    then s.iterations2
    else if n_transforms s == 3
    then s.iterations3
    else s.iterations4

  -- | Set the number of iterations for the current fractal.
  let set_iterations (s: state) (iter: i32): state =
    if n_transforms s == 2
    then s with iterations2 = iter
    else if n_transforms s == 3
    then s with iterations3 = iter
    else s with iterations4 = iter

  let grab_mouse = false

  let init (seed: i32) (h: i32) (w: i32): state =
    let rng = rng.rng_from_seed [seed]
    let (rng, manual) = gen_manual rng
    in {height=h, width=w,
        rng=rng,
        iterations2=22, iterations3=14, iterations4=11,
        time=0, fractal_id=0, manual=manual, auto_mode=false}

  let resize h w (s: state) =
    s with height = h with width = w

  let event (e: event) (s: state) =
    match e
    case #step td ->
      let (rng, manual) =
        if s.auto_mode
        then let (rng, x) = f32dist.rand (0, 1) s.rng
             in if x < 0.01
                then gen_manual rng
                else (rng, s.manual)
        else (s.rng, s.manual)
      in s with time = s.time + td
           with rng = rng
           with manual = manual
    case #keydown {key} ->
      if key == SDLK_LEFT
      then s with fractal_id = (s.fractal_id - 1) % fractal_choices
      else if key == SDLK_RIGHT
      then s with fractal_id = (s.fractal_id + 1) % fractal_choices
      else if key == SDLK_DOWN
      then set_iterations s (i32.max 0 (get_iterations s - 1))
      else if key == SDLK_UP
      then set_iterations s (get_iterations s + 1)
      else if key == SDLK_0 || key == SDLK_KP_0
      then s with auto_mode = !s.auto_mode
      else if key == SDLK_1 || key == SDLK_KP_1
      then let (rng, manual) = gen_manual s.rng
           in s with rng = rng with manual = manual
      else if key == SDLK_2 || key == SDLK_KP_2
      then let (rng, manual) = gen_manual s.rng
           in s with rng = rng with manual = (manual with n_trans = 2)
      else if key == SDLK_3 || key == SDLK_KP_3
      then let (rng, manual) = gen_manual s.rng
           in s with rng = rng with manual = (manual with n_trans = 3)
      else if key == SDLK_4 || key == SDLK_KP_4
      then let (rng, manual) = gen_manual s.rng
           in s with rng = rng with manual = (manual with n_trans = 4)
      else s
    case _ -> s

  let render (s: state) =
    let iter = get_iterations s
    let max_iter = max_iterations (n_transforms s)
    let iter' = i32.min iter max_iter
    in render_fractal' (fractal_from_id s.fractal_id)
                       s.time s.manual s.height s.width iter'

  let text_format = "Fractal: %["
                    ++ (loop s = "" for i < fractal_choices do
                          s ++ "|" ++ fractal_name (fractal_from_id i))[1:]
                    ++ "]\nTransforms: %d\n"
                    ++ "%[        |Current:] Iterations (2 transforms): %d (max: %d)\n"
                    ++ "%[        |Current:] Iterations (3 transforms): %d (max: %d)\n"
                    ++ "%[        |Current:] Iterations (4 transforms): %d (max: %d)\n"
                    ++ "Particles: %d^%d = %d\n"
                    ++ "Auto mode: %[disabled|enabled]\n"
                    ++ "FPS: %d"

  let text_content (fps: f32) (s: state): text_content =
    let n_trans = n_transforms s
    let iter = get_iterations s
    let max_iter = max_iterations n_trans
    let iter' = i32.min iter max_iter
    in (s.fractal_id, n_trans,
        i32.bool (n_trans == 2), s.iterations2, max_iterations 2,
        i32.bool (n_trans == 3), s.iterations3, max_iterations 3,
        i32.bool (n_trans == 4), s.iterations4, max_iterations 4,
        n_trans, iter', n_trans**iter', i32.bool s.auto_mode, t32 fps)

  let text_colour = const argb.white
}
