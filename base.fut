import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/lys/lys"

module vec2 = mk_vspace_2d f32
module rng = xorshift128plus
module dist = uniform_real_distribution f32 rng
module norm_dist = normal_distribution f32 rng

type point = {pos: vec2.vector, scale: f32, rotate: f32}

-- | Inputs to the `manual` fractal.
type manual = {
    rotate0: f32, tfac0: f32, translatex0: f32, translatey0: f32, scale0: f32,
    rotate1: f32, tfac1: f32, translatex1: f32, translatey1: f32, scale1: f32,
    rotate2: f32, tfac2: f32, translatex2: f32, translatey2: f32, scale2: f32
}

let scale (s: f32) (p: point): point =
  p with scale = p.scale * s

let rotate (rad: f32) (p: point): point =
  p with rotate = p.rotate + rad

let translate' (posd: vec2.vector) (p: point): point =
  let rot {x, y} = {x=x * f32.cos p.rotate - y * f32.sin p.rotate,
                    y=y * f32.cos p.rotate + x * f32.sin p.rotate}
  in p with pos = vec2.(p.pos + rot (scale p.scale posd))

let translate (xd: f32, yd: f32) = translate' {x=xd, y=yd}
let translate_x (xd: f32) = translate (xd, 0)
let translate_y (yd: f32) = translate (0, yd)

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
