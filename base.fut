import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/lys/lys"

module rng = xorshift128plus
module i32dist = uniform_int_distribution i32 rng
module f32dist = uniform_real_distribution f32 rng
module norm_dist = normal_distribution f32 rng

let hsl_value (n1: f32) (n2: f32) (hue: f32): f32 =
  let hue' = if hue > 6.0
             then hue - 6.0
             else if hue < 0.0
             then hue + 6.0
             else hue
  in if hue' < 1.0
     then n1 + (n2 - n1) * hue'
     else if hue' < 3.0
     then n2
     else if hue' < 4.0
     then n1 + (n2 - n1) * (4.0 - hue')
     else n1

let hsl_to_rgb (h: f32) (s: f32) (l: f32): (f32, f32, f32) =
  if s == 0.0
  then (l, l, l)
  else let m2 = if l <= 0.5
                then l * (1.0 + s)
                else l + s - l * s
       let m1 = 2.0 * l - m2
       let r = hsl_value m1 m2 (h * 6.0 + 2.0)
       let g = hsl_value m1 m2 (h * 6.0)
       let b = hsl_value m1 m2 (h * 6.0 - 2.0)
       in (r, g, b)

-- | Generate a f32 between -0.5 and +0.5, with most values close to 0.0.
let gen_f32 (rng: rng.rng): (rng.rng, f32) =
  let (rng, x) = norm_dist.rand {mean=0, stddev=1} rng
  in (rng, x / 2)

type gen_manual_constraint = #none | #trans i32

module fractal_utils_2d = {
  module vec2 = mk_vspace_2d f32

  -- | Inputs to the 2D `manual` fractal.
  type manual = {
      rotate0: f32, tfac0: f32, translate0: (f32, f32), scale0: f32,
      rotate1: f32, tfac1: f32, translate1: (f32, f32), scale1: f32,
      rotate2: f32, tfac2: f32, translate2: (f32, f32), scale2: f32,
      rotate3: f32, tfac3: f32, translate3: (f32, f32), scale3: f32,
      n_trans: #trans2 | #trans3 | #trans4
  }

  let gen_manual (rng: rng.rng) (c: gen_manual_constraint): (rng.rng, manual) =
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
    let (rng, rotate3) = gen_f32 rng
    let (rng, tfac3) = gen_f32 rng
    let (rng, translatex3) = gen_f32 rng
    let (rng, translatey3) = gen_f32 rng
    let (rng, scale3) = gen_f32 rng
    let scale3 = scale3 + 0.5
    let (rng, n_trans) = match c
                         case #none -> i32dist.rand (2, 4) rng
                         case #trans n -> (rng, n)
    let n_trans = match n_trans
                  case 2 -> #trans2
                  case 3 -> #trans3
                  case _ -> #trans4
    in (rng, {rotate0, tfac0, translate0=(translatex0, translatey0), scale0,
              rotate1, tfac1, translate1=(translatex1, translatey1), scale1,
              rotate2, tfac2, translate2=(translatex2, translatey2), scale2,
              rotate3, tfac3, translate3=(translatex3, translatey3), scale3,
              n_trans=n_trans})

  type point = {pos: vec2.vector, scale: f32, rotate: f32}

  let scale (s: f32) (p: point): point =
    p with scale = p.scale * s

  let rotate (rotd: f32) (p: point): point =
    p with rotate = p.rotate + rotd

  let translate' (posd: vec2.vector) (p: point): point =
    let rot {x, y} = {x=x * f32.cos p.rotate - y * f32.sin p.rotate,
                      y=y * f32.cos p.rotate + x * f32.sin p.rotate}
    in p with pos = vec2.(p.pos + rot (scale p.scale posd))

  let translate (xd: f32, yd: f32) = translate' {x=xd, y=yd}
  let translate_x (xd: f32) = translate (xd, 0)
  let translate_y (yd: f32) = translate (0, yd)

  let fractal (n_trans: i32) (pick_trans: i32 -> i32 -> point -> point)
              (height: i32) (width: i32) (steps: i32):
              (i32, [height][width]argb.colour) =
    let particle_point (i: i32): point =
      let (p, _) = loop (p, k) = ({pos={x=0, y=0}, scale=1, rotate=0},
                                  n_trans**steps) for _step < steps do
                   let k' = k / n_trans
                   in (pick_trans (i % k) k' p, k')
      in p
    let n_points = n_trans**steps
    let points = map particle_point (0..<n_points)
    let xy_factor = r32 (i32.min height width)
    let y_offset = 0.5 + r32 (i32.max 0 (height - width)) / xy_factor / 2
    let x_offset = 0.5 + r32 (i32.max 0 (width - height)) / xy_factor / 2
    let is = map (\p ->
                    let y = t32 ((p.pos.y + y_offset) * xy_factor)
                    let x = t32 ((p.pos.x + x_offset) * xy_factor)
                    in if x < 0 || x >= width || y < 0 || y >= height
                       then -1
                       else y * width + x) points
    let frame = replicate (height * width) 0
    let vs = replicate n_points 1
    let frame' = reduce_by_index frame (+) 0 is vs
    let max_depth = reduce i32.max 0 frame'
    let frame'' = map (\depth ->
                         let (r, g, b) = hsl_to_rgb
                                         (0.5 + r32 depth / r32 max_depth) 0.5 0.5
                         in i32.sgn depth * argb_colour.from_rgba r g b 1.0) frame'
    in (n_trans, unflatten height width frame'')

  let fractal2 trans0 trans1 =
    fractal 2 (\base factor p ->
                 if base < factor then trans0 p
                 else trans1 p)

  let fractal3 trans0 trans1 trans2 =
    fractal 3 (\base factor p ->
                 if base < factor then trans0 p
                 else if factor <= base && base < 2 * factor then trans1 p
                 else trans2 p)

  let fractal4 trans0 trans1 trans2 trans3 =
    fractal 4 (\base factor p ->
                 if base < factor then trans0 p
                 else if factor <= base && base < 2 * factor then trans1 p
                 else if 2 * factor <= base && base < 3 * factor then trans2 p
                 else trans3 p)
}

module fractal_utils_3d = {
  module vec3 = mk_vspace_3d f32

  -- | Inputs to the 3D `manual` fractal.
  type manual = {
      rotate0: (f32, f32, f32), tfac0: (f32, f32, f32), translate0: (f32, f32, f32), scale0: f32,
      rotate1: (f32, f32, f32), tfac1: (f32, f32, f32), translate1: (f32, f32, f32), scale1: f32,
      rotate2: (f32, f32, f32), tfac2: (f32, f32, f32), translate2: (f32, f32, f32), scale2: f32,
      rotate3: (f32, f32, f32), tfac3: (f32, f32, f32), translate3: (f32, f32, f32), scale3: f32,
      n_trans: #trans2 | #trans3 | #trans4
  }

  let gen_manual (rng: rng.rng) (c: gen_manual_constraint): (rng.rng, manual) =
    let (rng, rotatex0) = gen_f32 rng
    let (rng, rotatey0) = gen_f32 rng
    let (rng, rotatez0) = gen_f32 rng
    let (rng, tfacx0) = gen_f32 rng
    let (rng, tfacy0) = gen_f32 rng
    let (rng, tfacz0) = gen_f32 rng
    let (rng, translatex0) = gen_f32 rng
    let (rng, translatey0) = gen_f32 rng
    let (rng, translatez0) = gen_f32 rng
    let (rng, scale0) = gen_f32 rng
    let scale0 = scale0 + 0.5
    let (rng, rotatex1) = gen_f32 rng
    let (rng, rotatey1) = gen_f32 rng
    let (rng, rotatez1) = gen_f32 rng
    let (rng, tfacx1) = gen_f32 rng
    let (rng, tfacy1) = gen_f32 rng
    let (rng, tfacz1) = gen_f32 rng
    let (rng, translatex1) = gen_f32 rng
    let (rng, translatey1) = gen_f32 rng
    let (rng, translatez1) = gen_f32 rng
    let (rng, scale1) = gen_f32 rng
    let scale1 = scale1 + 0.5
    let (rng, rotatex2) = gen_f32 rng
    let (rng, rotatey2) = gen_f32 rng
    let (rng, rotatez2) = gen_f32 rng
    let (rng, tfacx2) = gen_f32 rng
    let (rng, tfacy2) = gen_f32 rng
    let (rng, tfacz2) = gen_f32 rng
    let (rng, translatex2) = gen_f32 rng
    let (rng, translatey2) = gen_f32 rng
    let (rng, translatez2) = gen_f32 rng
    let (rng, scale2) = gen_f32 rng
    let scale2 = scale2 + 0.5
    let (rng, rotatex3) = gen_f32 rng
    let (rng, rotatey3) = gen_f32 rng
    let (rng, rotatez3) = gen_f32 rng
    let (rng, tfacx3) = gen_f32 rng
    let (rng, tfacy3) = gen_f32 rng
    let (rng, tfacz3) = gen_f32 rng
    let (rng, translatex3) = gen_f32 rng
    let (rng, translatey3) = gen_f32 rng
    let (rng, translatez3) = gen_f32 rng
    let (rng, scale3) = gen_f32 rng
    let scale3 = scale3 + 0.5
    let (rng, n_trans) = match c
                         case #none -> i32dist.rand (2, 4) rng
                         case #trans n -> (rng, n)
    let n_trans = match n_trans
                  case 2 -> #trans2
                  case 3 -> #trans3
                  case _ -> #trans4
    in (rng, {rotate0=(rotatex0, rotatey0, rotatez0),
              tfac0=(tfacx0, tfacy0, tfacz0),
              translate0=(translatex0, translatey0, translatez0), scale0,
              rotate1=(rotatex1, rotatey1, rotatez1),
              tfac1=(tfacx1, tfacy1, tfacz1),
              translate1=(translatex1, translatey1, translatez1), scale1,
              rotate2=(rotatex2, rotatey2, rotatez2),
              tfac2=(tfacx2, tfacy2, tfacz2),
              translate2=(translatex2, translatey2, translatez2), scale2,
              rotate3=(rotatex3, rotatey3, rotatez3),
              tfac3=(tfacx3, tfacy3, tfacz3),
              translate3=(translatex3, translatey3, translatez3), scale3,
              n_trans=n_trans})

  type point = {pos: vec3.vector, scale: f32, rotate: vec3.vector}

  let scale (s: f32) (p: point): point =
    p with scale = p.scale * s

  let rotate' (rotd: vec3.vector) (p: point): point =
    p with rotate = p.rotate vec3.+ rotd

  let rotate (xd: f32, yd: f32, zd: f32) = rotate' {x=xd, y=yd, z=zd}
  let rotate_x (xd: f32) = rotate (xd, 0, 0)
  let rotate_y (yd: f32) = rotate (0, yd, 0)
  let rotate_z (zd: f32) = rotate (0, 0, zd)

  let translate' (posd: vec3.vector) (p: point): point =
    let {x=ax, y=ay, z=az} = p.rotate
    let rot {x=x0, y=y0, z=z0} =
      let (sin_x, cos_x) = (f32.sin ax, f32.cos ax)
      let (sin_y, cos_y) = (f32.sin ay, f32.cos ay)
      let (sin_z, cos_z) = (f32.sin az, f32.cos az)

      -- X axis.
      let (x1, y1, z1) = (x0,
                          y0 * cos_x - z0 * sin_x,
                          y0 * sin_x + z0 * cos_x)
      -- Y axis.
      let (x2, y2, z2) = (z1 * sin_y + x1 * cos_y,
                          y1,
                          z1 * cos_y - x1 * sin_y)
      -- Z axis.
      let (x3, y3, z3) = (x2 * cos_z - y2 * sin_z,
                          x2 * sin_z + y2 * cos_z,
                          z2)

      in {x=x3, y=y3, z=z3}
    in p with pos = vec3.(p.pos + rot (scale p.scale posd))

  let translate (xd: f32, yd: f32, zd: f32) = translate' {x=xd, y=yd, z=zd}
  let translate_x (xd: f32) = translate (xd, 0, 0)
  let translate_y (yd: f32) = translate (0, yd, 0)
  let translate_z (zd: f32) = translate (0, 0, zd)

  let fractal (n_trans: i32) (pick_trans: i32 -> i32 -> point -> point)
              (height: i32) (width: i32) (steps: i32):
              (i32, [height][width]argb.colour) =
    let particle_point (i: i32): point =
      let (p, _) = loop (p, k) = ({pos={x=0, y=0, z=0.5}, scale=1, rotate={x=0, y=0, z=0}},
                                  n_trans**steps) for _step < steps do
                   let k' = k / n_trans
                   in (pick_trans (i % k) k' p, k')
      in p
    let n_points = n_trans**steps
    let points = map particle_point (0..<n_points)
    let xy_factor = r32 (i32.min height width)
    let y_offset = 0.5 + r32 (i32.max 0 (height - width)) / xy_factor / 2
    let x_offset = 0.5 + r32 (i32.max 0 (width - height)) / xy_factor / 2

    let project_point {x, y, z} =
      let z_ratio = if z >= 0
                    then (1 + z) / 1
                    else 1 / ((1 - z) / 1)
      let x_projected = x / z_ratio
      let y_projected = y / z_ratio
      in {x=x_projected, y=y_projected}

    let is = map (\p ->
                    let pos = project_point p.pos
                    let y = t32 ((pos.y + y_offset) * xy_factor)
                    let x = t32 ((pos.x + x_offset) * xy_factor)
                    in if x < 0 || x >= width || y < 0 || y >= height
                       then -1
                       else y * width + x) points
    let frame = replicate (height * width) 0
    let vs = replicate n_points 1
    let frame' = reduce_by_index frame (+) 0 is vs
    let max_depth = reduce i32.max 0 frame'
    let frame'' = map (\depth ->
                         let (r, g, b) = hsl_to_rgb
                                         (0.5 + r32 depth / r32 max_depth) 0.5 0.5
                         in i32.sgn depth * argb_colour.from_rgba r g b 1.0) frame'
    in (n_trans, unflatten height width frame'')

  let fractal2 trans0 trans1 =
    fractal 2 (\base factor p ->
                 if base < factor then trans0 p
                 else trans1 p)

  let fractal3 trans0 trans1 trans2 =
    fractal 3 (\base factor p ->
                 if base < factor then trans0 p
                 else if factor <= base && base < 2 * factor then trans1 p
                 else trans2 p)

  let fractal4 trans0 trans1 trans2 trans3 =
    fractal 4 (\base factor p ->
                 if base < factor then trans0 p
                 else if factor <= base && base < 2 * factor then trans1 p
                 else if 2 * factor <= base && base < 3 * factor then trans2 p
                 else trans3 p)
}

module type fractals_base = {
  type fractal
  type manual
  val gen_manual: rng.rng -> gen_manual_constraint -> (rng.rng, manual)

  val fractals_end: fractal -> bool

  val fractal_from_id: i32 -> fractal
  val fractal_name: fractal -> string
  val render_fractal: fractal -> f32 -> manual -> i32 -> i32 -> i32 ->
                      (i32, [][]argb.colour)
}

module type fractals = {
  type fractal
  type manual
  val gen_manual: rng.rng -> gen_manual_constraint -> (rng.rng, manual)

  val fractal_from_id: i32 -> fractal
  val fractal_name: fractal -> string
  val fractal_choices: i32
  val fractal_n_transforms: fractal -> manual -> i32
  val render_fractal: fractal -> f32 -> manual -> i32 -> i32 -> i32 ->
                      [][]argb.colour
}

module fractals (base: fractals_base): fractals = {
  type fractal = base.fractal
  type manual = base.manual
  let gen_manual = base.gen_manual

  let fractal_from_id = base.fractal_from_id
  let fractal_name = base.fractal_name

  let fractal_choices = loop i = 0i32 while ! (base.fractals_end (base.fractal_from_id i)) do i + 1

  let fractal_n_transforms (f: base.fractal) (m: base.manual): i32 =
    (base.render_fractal f 0.0 m 0 0 0).1 -- hack!

  let render_fractal (f: base.fractal) (time: f32) (m: base.manual)
                     (height: i32) (width: i32)
                     (iterations: i32): [height][width]argb.colour =
    (base.render_fractal f time m height width iterations).2
}

-- We are currently limited by Futhark using 32-bit integers.  We could work
-- around this, but in practice having that many particles is very slow, so we
-- we use this check for now.
let max_iterations (n_trans: i32): i32 =
  t32 (f32.log (2**31 - 4096) / f32.log (r32 n_trans))
