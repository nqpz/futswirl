import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/lys/lys"

module vec2 = mk_vspace_2d f32
module rng = xorshift128plus
module dist = uniform_real_distribution f32 rng

type point = {pos: vec2.vector, scale: f32, rotate: f32}

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

let fractal (trans: []point -> i32 -> i32 -> []point)
            (height: i32) (width: i32) (steps: i32):
            [height][width]argb.colour =
  let n_trans = 3
  let array_length = n_trans**steps
  let array_initial = replicate array_length {pos={x= -1, y= -1},
                                              scale=0.0f32, rotate=0.0f32}
  let array_initial[0] = {pos={x=0, y=0}, scale=1, rotate=0}
  let array =
    loop array = array_initial for i < steps do
    let array1 = trans array i (steps - i)
    in array1
  let xy_factor = r32 (i32.min height width)
  let y_offset = 0.5 + r32 (i32.max 0 (height - width)) / xy_factor / 2
  let x_offset = 0.5 + r32 (i32.max 0 (width - height)) / xy_factor / 2
  let is = map (\p ->
                  let y = t32 ((p.pos.y + y_offset) * xy_factor)
                  let x = t32 ((p.pos.x + x_offset) * xy_factor)
                  in if x < 0 || x >= width || y < 0 || y >= height
                     then -1
                     else y * width + x) array
  let frame = replicate (height * width) 0
  let vs = replicate (length array) 1
  let frame' = reduce_by_index frame (+) 0 is vs
  let max_depth = reduce i32.max 0 frame'
  let frame'' = map (\depth ->
                       let (r, g, b) = hsl_to_rgb
                                       (0.5 + r32 depth / r32 max_depth) 0.5 0.5
                       in i32.sgn depth * argb_colour.from_rgba r g b 1.0) frame'
  in unflatten height width frame''

let fractal_run_transform (array_dest: *[]point) (array: []point)
                          (i: i32) (iinv: i32) (n_trans: i32)
                          (trans: point -> point) (k: i32): *[]point =
  -- Not necessarily the best way to do it, but could potentially be done
  -- entirely in-place.
  let is_dest =
    map (\j -> j * n_trans**iinv + k * n_trans**(iinv - 1))
        (0..<n_trans**i)
  in scatter array_dest is_dest
             (map (\j -> trans (unsafe array[j - j % n_trans**iinv]))
                  is_dest)

let fractal2 (trans0: point -> point) (trans1: point -> point)
             (height: i32) (width: i32) (steps: i32):
             [height][width]argb.colour =
  let n = 2
  let trans (array: []point) (i: i32) (iinv: i32): []point =
    let array0 = copy array
    let array1 = fractal_run_transform array0 array i iinv n trans0 0
    let array2 = fractal_run_transform array1 array i iinv n trans1 1
    in array2
  in fractal trans height width steps

let fractal3 (trans0: point -> point) (trans1: point -> point)
             (trans2: point -> point)
             (height: i32) (width: i32) (steps: i32):
             [height][width]argb.colour =
  let n = 3
  let trans (array: []point) (i: i32) (iinv: i32): []point =
    let array0 = copy array
    let array1 = fractal_run_transform array0 array i iinv n trans0 0
    let array2 = fractal_run_transform array1 array i iinv n trans1 1
    let array3 = fractal_run_transform array2 array i iinv n trans2 2
    in array3
  in fractal trans height width steps

let fractal4 (trans0: point -> point) (trans1: point -> point)
             (trans2: point -> point) (trans3: point -> point)
             (height: i32) (width: i32) (steps: i32):
             [height][width]argb.colour =
  let n = 4
  let trans (array: []point) (i: i32) (iinv: i32): []point =
    let array0 = copy array
    let array1 = fractal_run_transform array0 array i iinv n trans0 0
    let array2 = fractal_run_transform array1 array i iinv n trans1 1
    let array3 = fractal_run_transform array2 array i iinv n trans2 2
    let array4 = fractal_run_transform array3 array i iinv n trans3 3
    in array4
  in fractal trans height width steps

-- XXX: Use this for fun along with rand
let var (start: f32) (end: f32) (source: f32) (source_period: f32): f32 =
  let diff = end - start
  let pos_source = source - f32.floor (source / source_period) * source_period
  let pos_source' = if pos_source >= source_period / 2
                    then source_period - pos_source
                    else pos_source
  in pos_source' * (diff / source_period) - start
