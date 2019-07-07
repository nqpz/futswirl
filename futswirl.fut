import "base"
import "fractals"

let fractal_choices = loop i = 0i32 while fractal_from_id i != #eof do i + 1

let fractal_n_transforms (f: fractal) (m: manual): i32 =
  (render_fractal f 0.0 m 0 0 0).1 -- hack!

let render_fractal' (f: fractal) (time: f32) (m: manual)
                    (height: i32) (width: i32)
                    (iterations: i32): [height][width]argb.colour =
  (render_fractal f time m height width iterations).2

type text_content = (i32, i32, i32, i32, i32, i32, i32, i32)
module lys: lys with text_content = text_content = {
  type text_content = text_content

  type state = {height: i32, width: i32,
                rng: rng.rng,
                iterations: i32, time: f32,
                fractal_id: i32, manual: manual}

  let grab_mouse = false

  let init (seed: i32) (h: i32) (w: i32): state =
    let rng = rng.rng_from_seed [seed]
    let (rng, manual) = gen_manual rng
    in {height=h, width=w,
        rng=rng,
        iterations=13, time=0,
        fractal_id=0, manual=manual}

  let resize h w (s: state) =
    s with height = h with width = w

  let event (e: event) (s: state) =
    match e
    case #step td ->
      s with time = s.time + td
    case #keydown {key} ->
      if key == SDLK_LEFT
      then s with fractal_id = (s.fractal_id - 1) % fractal_choices
      else if key == SDLK_RIGHT
      then s with fractal_id = (s.fractal_id + 1) % fractal_choices
      else if key == SDLK_DOWN
      then s with iterations = i32.max 0 (s.iterations - 1)
      else if key == SDLK_UP
      then s with iterations = s.iterations + 1
      else if key == SDLK_r
      then let (rng, manual) = gen_manual s.rng
           in s with rng = rng with manual = manual
      else s
    case _ -> s

  let render (s: state) =
    let n_transforms = fractal_n_transforms (fractal_from_id s.fractal_id) s.manual
    let max_iter = max_iterations n_transforms
    let iter = i32.min max_iter s.iterations
    in render_fractal' (fractal_from_id s.fractal_id)
                       s.time s.manual s.height s.width iter

  let text_format = "Fractal: %["
                    ++ (loop s = "" for i < fractal_choices do
                          s ++ "|" ++ fractal_name (fractal_from_id i))[1:]
                    ++ "]\nTransforms: %d\nIterations: %d (max: %d)\nParticles: %d^%d = %d\nFPS: %d"

  let text_content (fps: f32) (s: state): text_content =
    let n_transforms = fractal_n_transforms (fractal_from_id s.fractal_id) s.manual
    let max_iter = max_iterations n_transforms
    let iterations' = i32.min max_iter s.iterations
    in (s.fractal_id, n_transforms, s.iterations, max_iter,
        n_transforms, iterations', n_transforms**iterations', t32 fps)

  let text_colour = const argb.white
}
