import "../lib/github.com/athas/vector/vspace"
import "../lib/github.com/diku-dk/cpprandom/random"
import "../lib/github.com/diku-dk/lys/lys"

import "settings"

module rnge = xorshift128plus
module f32dist = uniform_real_distribution f32 rnge
type rng = rnge.rng

type render_approach = #scalarloop | #cullbranches

module settings_module = import "../settings"
module settings: settings = settings_module.settings

type render_result_base [h][w] 't = {n_trans: i32,
                                     n_points: i32,
                                     n_iterations: i32,
                                     rot_square_radius: t,
                                     render: [h][w]argb.colour}

module type float_extended = {
  include float

  type render_result [h][w] = render_result_base [h][w] t

  val from_i32: i32 -> t

  val gen_float: rnge.rng -> (rng, t)
}

module mk_float_extended (float: float): float_extended with t = float.t = {
  open float

  module norm_dist = normal_distribution float rnge

  type render_result [h][w] = render_result_base [h][w] t

  def from_i32 = f64 <-< r64

  -- | Generate a f32 between -0.5 and +0.5, with most values close to 0.0.
  def gen_float (rng: rng): (rng, t) =
    let (rng, x) = norm_dist.rand {mean=float.f64 0, stddev=float.f64 0.1} rng
    let k = float.f64 0.5
    let x = float.(if x < neg k then neg k
                   else if x > k then k
                   else x)
    in (rng, x)
}

module f32e = mk_float_extended f32
module f64e = mk_float_extended f64

type float_bits = #f32 | #f64

type gen_manual_constraint = #none | #trans i32

module vec2_f32 = mk_vspace_2d f32
module vec2_f64 = mk_vspace_2d f64
