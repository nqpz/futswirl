import "base"
import "manual"
import "hsl"

module type fractal_utils = {
  type float

  type point
  val point_bytes: i32

  val scale: float -> point -> point

  type rotate_t
  val rotate: rotate_t -> point -> point

  type translate_t
  val translate: translate_t -> point -> point

  val start_point: float -> point
  val xypos: point -> float -> (float, float)
  val set_pos_x: point -> float -> point
  val unit_eval: (point -> point) -> (float, float)
}

module fractal_utils_extended (float: float_extended)
                              (utils: fractal_utils with float = float.t)
                              (manual: manual) = {
  module float = float
  open manual
  type manual = manual_base float.t
  module vec2 = mk_vspace_2d float
  type vec2_vector = vec2.vector
  module vec3 = mk_vspace_3d float
  open utils

  let fractal (n_trans: i32) (total_scale: float) (rot_square_radius: float)
              (pick_trans: i32 -> i32 -> point -> point)
              (all_trans: point -> vec2.vector -> float -> float -> (float, float) -> []point)
              (height: i32) (width: i32) (iterations: i32)
              (vp_zoom: float) (vp_center: vec2.vector)
              (render_approach: render_approach): float.render_result [height][width] =
    let xy_factor = float.from_i32 (i32.min height width)
    let x_offset_base = float.from_i32 (i32.max 0 (width - height)) float./ xy_factor
    let y_offset_base = float.from_i32 (i32.max 0 (height - width)) float./ xy_factor
    let x_offset = float.(x_offset_base / f64 2)
    let y_offset = float.(y_offset_base / f64 2)

    let make_pixels points =
      let is = map (\p ->
                      let (x0, y0) = xypos p vp_zoom
                      let x = float.(to_i32 (xy_factor * ((x0 + float.f64 0.5 + x_offset) -
                                                          vp_center.x * vp_zoom)))
                      let y = float.(to_i32 (xy_factor * ((y0 + f64 0.5 + y_offset) -
                                                          vp_center.y * vp_zoom)))
                      in if x < 0 || x >= width || y < 0 || y >= height
                         then -1
                         else y * width + x) points
      let frame = replicate (height * width) 0
      let vs = map (const 1) points
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
          while i64.i32 (length points) * i64.i32 n_trans * i64.i32 point_bytes
                < settings.cullbranches_bytes
          do let points = flatten (map (\p -> all_trans p vp_center cur_scale vp_zoom
                                                        (x_offset, y_offset)) points)
             let points = filter (\p -> !(float.isinf (xypos p vp_zoom).0)) points
             in (points, float.(cur_scale * total_scale), step + 1)
        in (length points, iterations', make_pixels points)

    in {n_trans=n_trans, n_points=n_points,
        n_iterations=iterations',
        rot_square_radius=rot_square_radius,
        render=unflatten height width frame}

  -- Pretty conservative.
  let branch_visible (c: vec2.vector) (cur_scale: float) (vp_zoom: float)
                     ((x_offset, y_offset): (float, float))
                     (rot_square_radius: float) (q: point): bool =
    let qp = xypos q vp_zoom
    let r = {x=qp.0, y=qp.1}
    let k_base = float.(f64 0.5 + (rot_square_radius / f64 2) * cur_scale)
    let kx = float.(k_base + x_offset)
    let ky = float.(k_base + y_offset)
    in float.(r.x >= negate kx + c.x * vp_zoom && r.x < kx + c.x * vp_zoom
              && r.y >= negate ky + c.y * vp_zoom && r.y < ky + c.y * vp_zoom)

  let branch_visible' c cur_scale vp_zoom off rr q: point =
    if branch_visible c cur_scale vp_zoom off rr q
    then q
    else set_pos_x q float.inf

  let rot_square_radius (max_translation: float) (total_scale: float): float =
    if float.(total_scale < f64 1)
    then let square_radius = float.(max_translation / (f64 1 - total_scale)) -- geometric series
         in float.(sqrt (f64 2 * (square_radius * f64 2)**f64 2) / f64 2)
    else float.inf -- really should not happen

  let fractal2 trans1 trans2 =
    let ((mt1, ts1), (mt2, ts2)) =
      (unit_eval trans1, unit_eval trans2)
    let (max_translation, total_scale) =
      (float.max mt1 mt2, float.max ts1 ts2)
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
      (float.(max (max mt1 mt2) mt3), float.(max (max ts1 ts2) ts3))
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
      (float.(max (max (max mt1 mt2) mt3) mt4),
       float.(max (max (max ts1 ts2) ts3) ts4))
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

module fractal_utils_2d_base (float: float_extended) = {
  module vec2 = mk_vspace_2d float
  module vec3 = mk_vspace_3d float
  type float = float.t

  type point = {pos: vec2.vector, scale: float, rotate: float}

  let point_bytes = 4i32 * 4

  let scale (s: float) (p: point): point =
    p with scale = float.(p.scale * s)

  type rotate_t = float
  let rotate (rotd: rotate_t) (p: point): point =
    p with rotate = float.(p.rotate + rotd)

  type translate_t = (float, float)
  let translate ((xd, yd): translate_t) (p: point): point =
    let rot {x, y} = float.({x=x * cos p.rotate - y * sin p.rotate,
                             y=y * cos p.rotate + x * sin p.rotate})
    in p with pos = vec2.(p.pos + rot (scale p.scale {x=xd, y=yd}))

  let start_point (s: float): point =
    float.({pos={x=f64 0, y=f64 0}, scale=s, rotate=f64 0})

  let xypos (p: point) _: (float, float) = (p.pos.x, p.pos.y)

  let set_pos_x (p: point) (x: float): point = p with pos.x = x

  let unit_eval (trans: point -> point): (float, float) =
    let p = trans (start_point (float.f64 1))
    let (x, y) = (p.pos.x, p.pos.y)
    in (float.max (float.abs x) (float.abs y), p.scale)
}

module fractal_utils_2d (float: float_extended) =
  fractal_utils_extended float (fractal_utils_2d_base float) manual_2d

module fractal_utils_3d_base (float: float_extended) = {
  module vec2 = mk_vspace_2d float
  module vec3 = mk_vspace_3d float
  type float = float.t

  type point = {pos: vec3.vector, scale: float, rotate: vec3.vector}

  let point_bytes = 7i32 * 4

  let scale (s: float) (p: point): point =
    p with scale = float.(p.scale * s)

  type rotate_t = (float, float, float)
  let rotate ((xd, yd, zd): rotate_t) (p: point): point =
    p with rotate = p.rotate vec3.+ {x=xd, y=yd, z=zd}

  type translate_t = (float, float, float)
  let translate ((xd, yd, zd): translate_t) (p: point): point =
    let {x=ax, y=ay, z=az} = p.rotate
    let rot {x=x0, y=y0, z=z0} =
      let (sin_x, cos_x) = (float.sin ax, float.cos ax)
      let (sin_y, cos_y) = (float.sin ay, float.cos ay)
      let (sin_z, cos_z) = (float.sin az, float.cos az)

      -- X axis.
      let (x1, y1, z1) = (x0,
                          float.(y0 * cos_x - z0 * sin_x),
                          float.(y0 * sin_x + z0 * cos_x))
      -- Y axis.
      let (x2, y2, z2) = (float.(z1 * sin_y + x1 * cos_y),
                          y1,
                          float.(z1 * cos_y - x1 * sin_y))
      -- Z axis.
      let (x3, y3, z3) = (float.(x2 * cos_z - y2 * sin_z),
                          float.(x2 * sin_z + y2 * cos_z),
                          z2)

      in {x=x3, y=y3, z=z3}
    in p with pos = vec3.(p.pos + rot (scale p.scale {x=xd, y=yd, z=zd}))

  let start_point (s: float): point = float.({pos={x=f64 0, y=f64 0, z=f64 0.5}, scale=s,
                                              rotate={x=f64 0, y=f64 0, z=f64 0}})

  let xypos (p: point) (vp_zoom: float): (float, float) =
    let {x, y, z} = p.pos
    let z' = float.(z / vp_zoom)
    let z_ratio = float.(if z' >= f64 0
                         then (f64 1 + z') / f64 1
                         else f64 1 / ((f64 1 - z') / f64 1))
    let x_projected = float.(x / z_ratio)
    let y_projected = float.(y / z_ratio)
    in (x_projected, y_projected)

  let set_pos_x (p: point) (x: float): point = p with pos.x = x

  let unit_eval (trans: point -> point): (float, float) =
    let p = trans (start_point (float.f64 1))
    let (x, y, z) = (p.pos.x, p.pos.y, p.pos.z)
    in (float.(max (max (abs x) (abs y)) (abs z)), p.scale)
}

module fractal_utils_3d (float: float_extended) =
  fractal_utils_extended float (fractal_utils_3d_base float) manual_3d
