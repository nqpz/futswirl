def hsl_to_rgb (h: f32) (s: f32) (l: f32): (f32, f32, f32) =
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
  in if s == 0.0
     then (l, l, l)
     else let m2 = if l <= 0.5
                   then l * (1.0 + s)
                   else l + s - l * s
          let m1 = 2.0 * l - m2
          let r = hsl_value m1 m2 (h * 6.0 + 2.0)
          let g = hsl_value m1 m2 (h * 6.0)
          let b = hsl_value m1 m2 (h * 6.0 - 2.0)
          in (r, g, b)
