import "base"
import "fractals"

let fractal_choices = loop i = 0i32 while fractal_from_id i != #eof do i + 1

type text_content = (i32, i32, i32, f32)
module lys: lys with text_content = text_content = {
  type text_content = text_content

  type state = {height: i32, width: i32,
                rng: rng.rng,
                iterations: i32, time: f32,
                fractal_id: i32, manual: manual}

  let grab_mouse = false

  -- | Generate a f32 between -0.5 and +0.5, with most values close to 0.0.
  let gen_f32 (rng: rng.rng): (rng.rng, f32) =
    let (rng, x) = norm_dist.rand {mean=0, stddev=1} rng
    in (rng, x / 2)

  let gen_manual (rng: rng.rng): (rng.rng, manual) =
    let (rng, rotate0) = gen_f32 rng
    let (rng, tfac0) = gen_f32 rng
    let (rng, translatex0) = gen_f32 rng
    let (rng, translatey0) = gen_f32 rng
    let (rng, scale0) = gen_f32 rng
    let scale0 = scale0 + 0.5
    let (rng, rotate1) = gen_f32 rng
    let (rng, tfac1) = gen_f32 rng
    let (rng, translatex1) = gen_f32 rng
    let (rng, translatey1) = gen_f32 rng
    let (rng, scale1) = gen_f32 rng
    let scale1 = scale1 + 0.5
    let (rng, rotate2) = gen_f32 rng
    let (rng, tfac2) = gen_f32 rng
    let (rng, translatex2) = gen_f32 rng
    let (rng, translatey2) = gen_f32 rng
    let (rng, scale2) = gen_f32 rng
    let scale2 = scale2 + 0.5
    in (rng, {rotate0=rotate0, tfac0=tfac0, translatex0=translatex0,
              translatey0=translatey0, scale0=scale0,
              rotate1=rotate1, tfac1=tfac1, translatex1=translatex1,
              translatey1=translatey1, scale1=scale1,
              rotate2=rotate2, tfac2=tfac2, translatex2=translatex2,
              translatey2=translatey2, scale2=scale2})

  let init (seed: i32) (h: i32) (w: i32): state =
    let rng = rng.rng_from_seed [seed]
    let (rng, manual) = gen_manual rng
    in {height=h, width=w,
        rng=rng,
        iterations=12, time=0,
        fractal_id=0, manual=manual}

  let resize h w (s: state) =
    s with height = h with width = w

  let event (e: event) (s: state) =
    match e
    case #step td ->
      s with time = s.time + td
    case #keydown {key} ->
      if key == SDLK_LEFT then s with fractal_id = (s.fractal_id - 1) % fractal_choices
      else if key == SDLK_RIGHT then s with fractal_id = (s.fractal_id + 1) % fractal_choices
      else if key == SDLK_DOWN then s with iterations = i32.max 0 (s.iterations - 1)
      else if key == SDLK_UP then s with iterations = s.iterations + 1
      else if key == SDLK_r then let (rng, manual) = gen_manual s.rng
                                 in s with rng = rng
                                      with manual = manual
      else s
    case _ -> s

  let render (s: state) =
    render_fractal (fractal_from_id s.fractal_id)
                   s.time s.manual s.height s.width s.iterations

  let text_format = "Fractal: %["
                    ++ (loop s = "" for i < fractal_choices do
                          s ++ "|" ++ fractal_name (fractal_from_id i))[1:]
                    ++ "]\nIterations: %d\nFPS: %d\nTime: %f"

  let text_content (fps: f32) (s: state): text_content =
    (s.fractal_id, s.iterations, t32 fps, s.time)

  let text_colour = const argb.white
}
