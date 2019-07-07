import "base"
import "fractals"

let fractal_choices = loop i = 0i32 while fractal_from_id i != #eof do i + 1

type text_content = (i32, i32, i32, f32)
module lys: lys with text_content = text_content = {
  type text_content = text_content

  type state = {height: i32, width: i32,
                rng: rng.rng, rand: f32,
                iterations: i32, time: f32,
                fractal_id: i32}

  let grab_mouse = false

  let init (seed: i32) (h: i32) (w: i32): state =
    let rng = rng.rng_from_seed [seed]
    let (rng', rand) = dist.rand (-0.5, 0.5) rng
    in {height=h, width=w,
        rng=rng', rand=rand,
        iterations=12, time=0,
        fractal_id=0}

  let resize h w (s: state) =
    s with height = h with width = w

  let event (e: event) (s: state) =
    match e
    case #step td ->
      let (rng, rand) = dist.rand (-0.5, 0.5) s.rng
      in s with time = s.time + td
           with rng = rng
           with rand = s.rand * 0.99 + rand * 0.01 -- smooth changes
    case #keydown {key} ->
      if key == SDLK_LEFT then s with fractal_id = (s.fractal_id - 1) % fractal_choices
      else if key == SDLK_RIGHT then s with fractal_id = (s.fractal_id + 1) % fractal_choices
      else if key == SDLK_DOWN then s with iterations = i32.max 0 (s.iterations - 1)
      else if key == SDLK_UP then s with iterations = s.iterations + 1
      else s
    case _ -> s

  let render (s: state) =
    render_fractal (fractal_from_id s.fractal_id)
                   s.time s.rand s.height s.width s.iterations

  let text_format = "Fractal: %["
                    ++ (loop s = "" for i < fractal_choices do
                          s ++ "|" ++ fractal_name (fractal_from_id i))[1:]
                    ++ "]\nIterations: %d\nFPS: %d\nTime: %f"

  let text_content (fps: f32) (s: state): text_content =
    (s.fractal_id, s.iterations, t32 fps, s.time)

  let text_colour = const argb.white
}
