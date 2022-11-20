import "base"

module type manual = {
  type manual_base 'f
  val gen_manual: rng -> gen_manual_constraint -> (rng, manual_base f64)
  val manual_from_f64: manual_base f64 -> manual_base f32
}

def gen_manual_trans (rng: rng): (rng, i32) =
  let (rng, t) = f32dist.rand (0, 1) rng
  in (rng, if t < 0.1 then 2
           else if t < 0.6 then 3
           else 4)

module manual_2d = {
  -- | Inputs to the 2D `manual` fractal.
  type manual_base 'f = {
      rotate0: f, tfac0: f, translate0: (f, f), scale0: f,
      rotate1: f, tfac1: f, translate1: (f, f), scale1: f,
      rotate2: f, tfac2: f, translate2: (f, f), scale2: f,
      rotate3: f, tfac3: f, translate3: (f, f), scale3: f,
      n_trans: #trans2 | #trans3 | #trans4
  }

  def gen_manual (rng: rng) (c: gen_manual_constraint): (rng, manual_base f64) =
    let gen_one rng =
      let (rng, rotate) = f64e.gen_float rng
      let (rng, tfac) = f64e.gen_float rng
      let (rng, translatex) = f64e.gen_float rng
      let (rng, translatey) = f64e.gen_float rng
      let (rng, scale) = f64e.gen_float rng
      let scale = f64e.(scale + f64 0.5)
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

  def manual_from_f64 (m: manual_base f64): manual_base f32 =
    let c = f32.f64
    in {rotate0=c m.rotate0, tfac0=c m.tfac0,
        translate0=(c m.translate0.0, c m.translate0.1),
        scale0=c m.scale0,
        rotate1=c m.rotate1, tfac1=c m.tfac1,
        translate1=(c m.translate1.0, c m.translate1.1),
        scale1=c m.scale1,
        rotate2=c m.rotate2, tfac2=c m.tfac2,
        translate2=(c m.translate2.0, c m.translate2.1),
        scale2=c m.scale2,
        rotate3=c m.rotate3, tfac3=c m.tfac3,
        translate3=(c m.translate3.0, c m.translate3.1),
        scale3=c m.scale3,
        n_trans=m.n_trans}
}

module manual_3d = {
  -- | Inputs to the 3D `manual` fractal.
  type manual_base 'f = {
      rotate0: (f, f, f), tfac0: (f, f, f), translate0: (f, f, f), scale0: f,
      rotate1: (f, f, f), tfac1: (f, f, f), translate1: (f, f, f), scale1: f,
      rotate2: (f, f, f), tfac2: (f, f, f), translate2: (f, f, f), scale2: f,
      rotate3: (f, f, f), tfac3: (f, f, f), translate3: (f, f, f), scale3: f,
      n_trans: #trans2 | #trans3 | #trans4
  }

  def gen_manual (rng: rng) (c: gen_manual_constraint): (rng, manual_base f64) =
    let gen_one rng =
      let (rng, rotatex) = f64e.gen_float rng
      let (rng, rotatey) = f64e.gen_float rng
      let (rng, rotatez) = f64e.gen_float rng
      let (rng, tfacx) = f64e.gen_float rng
      let (rng, tfacy) = f64e.gen_float rng
      let (rng, tfacz) = f64e.gen_float rng
      let (rng, translatex) = f64e.gen_float rng
      let (rng, translatey) = f64e.gen_float rng
      let (rng, translatez) = f64e.gen_float rng
      let (rng, scale) = f64e.gen_float rng
      let scale = f64e.(scale + f64 0.5)
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

  def manual_from_f64 (m: manual_base f64): manual_base f32 =
    let c = f32.f64
    in {rotate0=(c m.rotate0.0, c m.rotate0.1, c m.rotate0.2),
        tfac0=(c m.tfac0.0, c m.tfac0.1, c m.tfac0.2),
        translate0=(c m.translate0.0, c m.translate0.1, c m.translate0.2),
        scale0=c m.scale0,
        rotate1=(c m.rotate1.0, c m.rotate1.1, c m.rotate1.2),
        tfac1=(c m.tfac1.0, c m.tfac1.1, c m.tfac1.2),
        translate1=(c m.translate1.0, c m.translate1.1, c m.translate1.2),
        scale1=c m.scale1,
        rotate2=(c m.rotate2.0, c m.rotate2.1, c m.rotate2.2),
        tfac2=(c m.tfac2.0, c m.tfac2.1, c m.tfac2.2),
        translate2=(c m.translate2.0, c m.translate2.1, c m.translate2.2),
        scale2=c m.scale2,
        rotate3=(c m.rotate3.0, c m.rotate3.1, c m.rotate3.2),
        tfac3=(c m.tfac3.0, c m.tfac3.1, c m.tfac3.2),
        translate3=(c m.translate3.0, c m.translate3.1, c m.translate3.2),
        scale3=c m.scale3,
        n_trans=m.n_trans}
}
