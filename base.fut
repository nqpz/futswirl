import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/lys/lys"

module rng = xorshift128plus
module dist = uniform_real_distribution f32 rng
module norm_dist = normal_distribution f32 rng
module vec2 = mk_vspace_2d f32
module vec3 = mk_vspace_3d f32

type render_approach = #scalarloop | #cullbranches
type render_result = {n_trans: i32, n_points: i32,
                      n_iterations: i32,
                      rot_square_radius: f32,
                      render: [][]argb.colour}

module settings: {
  val cullbranches_bytes: i32
  val iterations2: i32
  val iterations3: i32
  val iterations4: i32
} = import "settings"

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
  let (rng, x) = norm_dist.rand {mean=0, stddev=0.1} rng
  let x = if x < -0.5 then -0.5
          else if x > 0.5 then 0.5
          else x
  in (rng, x)

type gen_manual_constraint = #none | #trans i32

let gen_manual_trans (rng: rng.rng): (rng.rng, i32) =
  let (rng, t) = dist.rand (0, 1) rng
  in (rng, if t < 0.1 then 2
           else if t < 0.6 then 3
           else 4)

module type fractal_utils = {
  type manual
  val gen_manual: rng.rng -> gen_manual_constraint -> (rng.rng, manual)
  type point
  val point_bytes: i32
  val scale: f32 -> point -> point
  type rotate_t
  val rotate: rotate_t -> point -> point
  type translate_t
  val translate: translate_t -> point -> point
  val start_point: f32 -> point
  val xypos: point -> f32 -> (f32, f32)
  val set_pos_x: point -> f32 -> point
  val unit_eval: (point -> point) -> (f32, f32)
}

module fractal_utils_extended (u: fractal_utils) = {
  open u

  let fractal (n_trans: i32) (total_scale: f32) (rot_square_radius: f32)
              (pick_trans: i32 -> i32 -> point -> point)
              (all_trans: point -> vec2.vector -> f32 -> f32 -> (f32, f32) -> []point)
              (height: i32) (width: i32) (iterations: i32)
              (vp_zoom: f32) (vp_center: vec2.vector)
              (render_approach: render_approach): render_result =
    let xy_factor = r32 (i32.min height width)
    let x_offset_base = r32 (i32.max 0 (width - height)) / xy_factor
    let y_offset_base = r32 (i32.max 0 (height - width)) / xy_factor
    let x_offset = x_offset_base / 2
    let y_offset = y_offset_base / 2

    let make_pixels points =
      let is = map (\p ->
                      let (x0, y0) = xypos p vp_zoom
                      let x = t32 (xy_factor * ((x0 + 0.5 + x_offset) - vp_center.x * vp_zoom))
                      let y = t32 (xy_factor * ((y0 + 0.5 + y_offset) - vp_center.y * vp_zoom))
                      in if x < 0 || x >= width || y < 0 || y >= height
                         then -1
                         else y * width + x) points
      let frame = replicate (height * width) 0
      let vs = replicate (length points) 1
      let frame' = reduce_by_index frame (+) 0 is vs
      let max_depth = reduce i32.max 0 frame'
      let frame'' = map (\depth ->
                           let (r, g, b) = hsl_to_rgb
                                           (0.5 + r32 depth / r32 max_depth) 0.5 0.5
                           in i32.sgn depth * argb_colour.from_rgba r g b 1.0) frame'
      in frame''

    let (n_points, iterations', frame) =
      match render_approach
      case #scalarloop ->
        let particle_point (i: i32): point =
          let (p, _) = loop (p, k) = (start_point vp_zoom,
                                      n_trans**iterations) for _step < iterations do
                       let k' = k / n_trans
                       in (pick_trans (i % k) k' p, k')
          in p
        let n_points = n_trans**iterations
        let points = map particle_point (0..<n_points)
        in (n_points, iterations, make_pixels points)

      case #cullbranches ->
        let (points, _, iterations') =
          loop (points, cur_scale, step) = ([start_point vp_zoom], vp_zoom, 0)
          while length points * n_trans * point_bytes < settings.cullbranches_bytes
          do let points = flatten (map (\p -> all_trans p vp_center cur_scale vp_zoom
                                                        (x_offset, y_offset)) points)
             let points = filter (\p -> !(f32.isinf (xypos p vp_zoom).1)) points
             in (points, cur_scale * total_scale, step + 1)
        in (length points, iterations', make_pixels points)

    in {n_trans=n_trans, n_points=n_points,
        n_iterations=iterations',
        rot_square_radius=rot_square_radius,
        render=unflatten height width frame}

  -- Pretty conservative.
  let branch_visible (c: vec2.vector) (cur_scale: f32) (vp_zoom: f32)
                     ((x_offset, y_offset): (f32, f32))
                     (rot_square_radius: f32) (q: point): bool =
    let qp = xypos q vp_zoom
    let r = {x=qp.1, y=qp.2}
    let k_base = 0.5 + (rot_square_radius / 2) * cur_scale
    let kx = k_base + x_offset
    let ky = k_base + y_offset
    in r.x >= -kx + c.x * vp_zoom && r.x < kx + c.x * vp_zoom
       && r.y >= -ky + c.y * vp_zoom && r.y < ky + c.y * vp_zoom

  let branch_visible' c cur_scale vp_zoom off rr q: point =
    if branch_visible c cur_scale vp_zoom off rr q
    then q
    else set_pos_x q f32.inf

  let rot_square_radius (max_translation: f32) (total_scale: f32): f32 =
    if total_scale < 1
    then let square_radius = max_translation / (1 - total_scale) -- geometric series
         in f32.sqrt (2 * (square_radius * 2)**2) / 2
    else f32.inf -- really should not happen

  let fractal2 trans1 trans2 =
    let ((mt1, ts1), (mt2, ts2)) =
      (unit_eval trans1, unit_eval trans2)
    let (max_translation, total_scale) =
      (f32.max mt1 mt2, f32.max ts1 ts2)
    let rr = rot_square_radius max_translation total_scale
    in fractal 2 total_scale rr
               (\base factor p ->
                  if base < factor then trans1 p
                  else trans2 p)
               (\p c cur_scale vp_zoom off ->
                  let ensure q = branch_visible' c cur_scale vp_zoom off rr q
                  in [ensure (trans1 p), ensure (trans2 p)])

  let fractal3 trans1 trans2 trans3 =
    let ((mt1, ts1), (mt2, ts2), (mt3, ts3)) =
      (unit_eval trans1, unit_eval trans2, unit_eval trans3)
    let (max_translation, total_scale) =
      (f32.max (f32.max mt1 mt2) mt3, f32.max (f32.max ts1 ts2) ts3)
    let rr = rot_square_radius max_translation total_scale
    in fractal 3 total_scale rr
               (\base factor p ->
                  if base < factor then trans1 p
                  else if factor <= base && base < 2 * factor then trans2 p
                  else trans3 p)
               (\p c cur_scale vp_zoom off ->
                  let ensure q = branch_visible' c cur_scale vp_zoom off rr q
                  in [ensure (trans1 p), ensure (trans2 p), ensure (trans3 p)])

  let fractal4 trans1 trans2 trans3 trans4 =
    let ((mt1, ts1), (mt2, ts2), (mt3, ts3), (mt4, ts4)) =
      (unit_eval trans1, unit_eval trans2,
       unit_eval trans3, unit_eval trans4)
    let (max_translation, total_scale) =
      (f32.max (f32.max (f32.max mt1 mt2) mt3) mt4,
       f32.max (f32.max (f32.max ts1 ts2) ts3) ts4)
    let rr = rot_square_radius max_translation total_scale
    in fractal 4 total_scale rr
               (\base factor p ->
                 if base < factor then trans1 p
                 else if factor <= base && base < 2 * factor then trans2 p
                 else if 2 * factor <= base && base < 3 * factor then trans3 p
                 else trans4 p)
               (\p c cur_scale vp_zoom off ->
                  let ensure q = branch_visible' c cur_scale vp_zoom off rr q
                  in [ensure (trans1 p), ensure (trans2 p),
                      ensure (trans3 p), ensure (trans4 p)])
}

module fractal_utils_2d = fractal_utils_extended {
  -- | Inputs to the 2D `manual` fractal.
  type manual = {
      rotate0: f32, tfac0: f32, translate0: (f32, f32), scale0: f32,
      rotate1: f32, tfac1: f32, translate1: (f32, f32), scale1: f32,
      rotate2: f32, tfac2: f32, translate2: (f32, f32), scale2: f32,
      rotate3: f32, tfac3: f32, translate3: (f32, f32), scale3: f32,
      n_trans: #trans2 | #trans3 | #trans4
  }

  let gen_manual (rng: rng.rng) (c: gen_manual_constraint): (rng.rng, manual) =
    let gen_one rng =
      let (rng, rotate) = gen_f32 rng
      let (rng, tfac) = gen_f32 rng
      let (rng, translatex) = gen_f32 rng
      let (rng, translatey) = gen_f32 rng
      let (rng, scale) = gen_f32 rng
      let scale = scale + 0.5
      in (rng, rotate, tfac, translatex, translatey, scale)
    let (rng, rotate0, tfac0, translatex0, translatey0, scale0) = gen_one rng
    let (rng, rotate1, tfac1, translatex1, translatey1, scale1) = gen_one rng
    let (rng, rotate2, tfac2, translatex2, translatey2, scale2) = gen_one rng
    let (rng, rotate3, tfac3, translatex3, translatey3, scale3) = gen_one rng
    let (rng, n_trans) = match c
                         case #none -> gen_manual_trans rng
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

  let point_bytes = 4i32 * 4

  let scale (s: f32) (p: point): point =
    p with scale = p.scale * s

  type rotate_t = f32
  let rotate (rotd: rotate_t) (p: point): point =
    p with rotate = p.rotate + rotd

  type translate_t = (f32, f32)
  let translate ((xd, yd): translate_t) (p: point): point =
    let rot {x, y} = {x=x * f32.cos p.rotate - y * f32.sin p.rotate,
                      y=y * f32.cos p.rotate + x * f32.sin p.rotate}
    in p with pos = vec2.(p.pos + rot (scale p.scale {x=xd, y=yd}))

  let start_point (s: f32): point = {pos={x=0, y=0}, scale=s, rotate=0}

  let xypos (p: point) _: (f32, f32) = (p.pos.x, p.pos.y)

  let set_pos_x (p: point) (x: f32): point = p with pos.x = x

  let unit_eval (trans: point -> point): (f32, f32) =
    let p = trans (start_point 1.0)
    let (x, y) = (p.pos.x, p.pos.y)
    in (f32.max (f32.abs x) (f32.abs y), p.scale)

}

module fractal_utils_3d = fractal_utils_extended {
  -- | Inputs to the 3D `manual` fractal.
  type manual = {
      rotate0: (f32, f32, f32), tfac0: (f32, f32, f32), translate0: (f32, f32, f32), scale0: f32,
      rotate1: (f32, f32, f32), tfac1: (f32, f32, f32), translate1: (f32, f32, f32), scale1: f32,
      rotate2: (f32, f32, f32), tfac2: (f32, f32, f32), translate2: (f32, f32, f32), scale2: f32,
      rotate3: (f32, f32, f32), tfac3: (f32, f32, f32), translate3: (f32, f32, f32), scale3: f32,
      n_trans: #trans2 | #trans3 | #trans4
  }

  let gen_manual (rng: rng.rng) (c: gen_manual_constraint): (rng.rng, manual) =
    let gen_one rng =
      let (rng, rotatex) = gen_f32 rng
      let (rng, rotatey) = gen_f32 rng
      let (rng, rotatez) = gen_f32 rng
      let (rng, tfacx) = gen_f32 rng
      let (rng, tfacy) = gen_f32 rng
      let (rng, tfacz) = gen_f32 rng
      let (rng, translatex) = gen_f32 rng
      let (rng, translatey) = gen_f32 rng
      let (rng, translatez) = gen_f32 rng
      let (rng, scale) = gen_f32 rng
      let scale = scale + 0.5
      in (rng, rotatex, rotatey, rotatez,
          tfacx, tfacy, tfacz,
          translatex, translatey, translatez, scale)
    let (rng, rotatex0, rotatey0, rotatez0,
         tfacx0, tfacy0, tfacz0,
         translatex0, translatey0, translatez0,
         scale0) = gen_one rng
    let (rng, rotatex1, rotatey1, rotatez1,
         tfacx1, tfacy1, tfacz1,
         translatex1, translatey1, translatez1,
         scale1) = gen_one rng
    let (rng, rotatex2, rotatey2, rotatez2,
         tfacx2, tfacy2, tfacz2,
         translatex2, translatey2, translatez2,
         scale2) = gen_one rng
    let (rng, rotatex3, rotatey3, rotatez3,
         tfacx3, tfacy3, tfacz3,
         translatex3, translatey3, translatez3,
         scale3) = gen_one rng
    let (rng, n_trans) = match c
                         case #none -> gen_manual_trans rng
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

  let point_bytes = 7i32 * 4

  let scale (s: f32) (p: point): point =
    p with scale = p.scale * s

  type rotate_t = (f32, f32, f32)
  let rotate ((xd, yd, zd): rotate_t) (p: point): point =
    p with rotate = p.rotate vec3.+ {x=xd, y=yd, z=zd}

  type translate_t = (f32, f32, f32)
  let translate ((xd, yd, zd): translate_t) (p: point): point =
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
    in p with pos = vec3.(p.pos + rot (scale p.scale {x=xd, y=yd, z=zd}))

  let start_point (s: f32): point = {pos={x=0, y=0, z=0.5}, scale=s,
                                     rotate={x=0, y=0, z=0}}

  let xypos (p: point) (vp_zoom: f32): (f32, f32) =
    let {x, y, z} = p.pos
    let z' = z / vp_zoom
    let z_ratio = if z' >= 0
                  then (1 + z') / 1
                  else 1 / ((1 - z') / 1)
    let x_projected = x / z_ratio
    let y_projected = y / z_ratio
    in (x_projected, y_projected)

  let set_pos_x (p: point) (x: f32): point = p with pos.x = x

  let unit_eval (trans: point -> point): (f32, f32) =
    let p = trans (start_point 1.0)
    let (x, y, z) = (p.pos.x, p.pos.y, p.pos.z)
    in (f32.max (f32.max (f32.abs x) (f32.abs y)) (f32.abs z), p.scale)

}

module type fractals_base = {
  type fractal
  type manual
  val gen_manual: rng.rng -> gen_manual_constraint -> (rng.rng, manual)

  val fractals_end: fractal -> bool

  val fractal_from_id: i32 -> fractal
  val fractal_name: fractal -> string
  val render_fractal: fractal -> f32 -> manual -> i32 -> i32 -> i32 ->
                     f32 -> vec2.vector -> render_approach ->
                     render_result
}

module type fractals = {
  type fractal
  type manual
  val gen_manual: rng.rng -> gen_manual_constraint -> (rng.rng, manual)

  val fractal_from_id: i32 -> fractal
  val fractal_name: fractal -> string
  val fractal_choices: i32
  val render_fractal: fractal -> f32 -> manual -> i32 -> i32 -> i32 ->
                      f32 -> vec2.vector -> render_approach -> render_result
}

module fractals (base: fractals_base): fractals = {
  type fractal = base.fractal
  type manual = base.manual
  let gen_manual = base.gen_manual

  let fractal_from_id = base.fractal_from_id
  let fractal_name = base.fractal_name

  let fractal_choices = loop i = 0i32
                        while ! (base.fractals_end (base.fractal_from_id i))
                        do i + 1

  let render_fractal (f: base.fractal) (time: f32) (m: base.manual)
                     (height: i32) (width: i32)
                     (iterations: i32) (vp_zoom: f32) (vp_center: vec2.vector)
                     (render_approach: render_approach): render_result =
    base.render_fractal f time m height width iterations vp_zoom vp_center render_approach
}

-- We are currently limited by Futhark using 32-bit integers.  We could work
-- around this, but in practice having that many particles is very slow, so we
-- we use this check for now.  Only applies to the scalarloop render approach.
let max_iterations (n_trans: i32): i32 =
  t32 (f32.log (2**31 - 4096) / f32.log (r32 n_trans))
