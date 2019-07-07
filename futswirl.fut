import "base"
import "fractals"

let fractal_choices = loop i = 0i32 while fractal_from_id i != #eof do i + 1

let fractal_n_transforms (f: fractal): i32 =
  let m = {rotate0=0, tfac0=0, translatex0=0, translatey0=0, scale0=0,
           rotate1=0, tfac1=0, translatex1=0, translatey1=0, scale1=0,
           rotate2=0, tfac2=0, translatex2=0, translatey2=0, scale2=0}
  in (render_fractal f 0.0 m 0 0 0).1 -- hack!

let render_fractal' (f: fractal) (time: f32) (m: manual)
                    (height: i32) (width: i32)
                    (iterations: i32): [height][width]argb.colour =
  (render_fractal f time m height width iterations).2

-- We are currently limited by Futhark using 32-bit integers.  We could work
-- around this, but in practice having that many particles is very slow, so we
-- we use this check for now.
let max_iterations (n_trans: i32): i32 =
  t32 (f32.log (2**31 - 4096) / f32.log (r32 n_trans))

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
    let n_transforms = fractal_n_transforms (fractal_from_id s.fractal_id)
    let max_iter = max_iterations n_transforms
    let iter = i32.min max_iter s.iterations
    in render_fractal' (fractal_from_id s.fractal_id)
                       s.time s.manual s.height s.width iter

  let text_format = "Fractal: %["
                    ++ (loop s = "" for i < fractal_choices do
                          s ++ "|" ++ fractal_name (fractal_from_id i))[1:]
                    ++ "]\nTransforms: %d\nIterations: %d (max: %d)\nParticles: %d^%d = %d\nFPS: %d"

  let text_content (fps: f32) (s: state): text_content =
    let n_transforms = fractal_n_transforms (fractal_from_id s.fractal_id)
    let max_iter = max_iterations n_transforms
    let iterations' = i32.min max_iter s.iterations
    in (s.fractal_id, n_transforms, s.iterations, max_iter,
        n_transforms, iterations', n_transforms**iterations', t32 fps)

  let text_colour = const argb.white
}
