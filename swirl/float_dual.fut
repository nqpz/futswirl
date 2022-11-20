import "../lib/github.com/athas/vector/vspace"

import "base"

-- After dead code elimination, this should use either f32 or f64 depending on
-- the enable_f64 setting.

type float_dual = {f32: f32, f64: f64}
type uint_dual = {u32: u32, u64: u64}

module type float_with_to_conv = {
  include float

  val to_f32: t -> f32
}

module float_dual = {
  type t = float_dual
  type int_t = uint_dual

  module i32m = i32
  module i64m = i64
  module u32m = u32
  module u64m = u64
  module f32m = f32
  module f64m = f64

  def to_f32 (x: float_dual): f32 =
    if settings.enable_f64
    then f32.f64 x.f64
    else x.f32

  def op0 (f32op: f32)
          (f64op: f64): float_dual =
    {f32=f32op, f64=f64op}

  def op1 (f32op: f32 -> f32)
          (f64op: f64 -> f64)
          (x: float_dual): float_dual =
    {f32=f32op x.f32, f64=f64op x.f64}

  def op1l (f32op: []f32 -> f32)
           (f64op: []f64 -> f64)
           (x: []float_dual): float_dual =
    {f32=f32op (map (.f32) x), f64=f64op (map (.f64) x)}

  def op1' 'a (f32op: a -> f32)
              (f64op: a -> f64)
              (x: a): float_dual =
    {f32=f32op x, f64=f64op x}

  def op1'' 'a (f32op: f32 -> a)
               (f64op: f64 -> a)
               (x: float_dual): a =
    if settings.enable_f64
    then f64op x.f64
    else f32op x.f32

  def op2 (f32op: f32 -> f32 -> f32)
          (f64op: f64 -> f64 -> f64)
          (x: float_dual) (y: float_dual): float_dual =
    {f32=f32op x.f32 y.f32, f64=f64op x.f64 y.f64}

  def op2' 'a 'b (f32op: a -> b -> f32)
                 (f64op: a -> b -> f64)
                 (x: a) (y: b): float_dual =
    {f32=f32op x y, f64=f64op x y}

  def op2'' 'a (f32op: f32 -> f32 -> a)
               (f64op: f64 -> f64 -> a)
               (x: float_dual) (y: float_dual): a =
    if settings.enable_f64
    then f64op x.f64 y.f64
    else f32op x.f32 y.f32

  def (+) = op2 (f32m.+) (f64m.+)
  def (-) = op2 (f32m.-) (f64m.-)
  def (*) = op2 (f32m.*) (f64m.*)
  def (/) = op2 (f32m./) (f64m./)
  def (%) = op2 (f32m.%) (f64m.%)
  def (**) = op2 (f32m.**) (f64m.**)

  def u8  (x: u8)  = op1' f32m.u8 f64m.u8 x
  def u16 (x: u16) = op1' f32m.u16 f64m.u16 x
  def u32 (x: u32) = op1' f32m.u32 f64m.u32 x
  def u64 (x: u64) = op1' f32m.u64 f64m.u64 x

  def i8 (x: i8) = op1' f32m.i8 f64m.i8 x
  def i16 (x: i16) = op1' f32m.i16 f64m.i16 x
  def i32 (x: i32) = op1' f32m.i32 f64m.i32 x
  def i64 (x: i64) = op1' f32m.i64 f64m.i64 x

  def f32 (x: f32) = op1' f32m.f32 f64m.f32 x
  def f64 (x: f64) = op1' f32m.f64 f64m.f64 x

  def bool (x: bool) = op1' f32m.bool f64m.bool x

  def from_fraction (x: i64) (y: i64) = op2' f32m.from_fraction f64m.from_fraction x y
  def to_i64 (x: t) = op1'' f32m.to_i64 f64m.to_i64 x
  def to_f64 (x: t) = op1'' f32m.to_f64 f64m.to_f64 x

  def (==) = op2'' (f32m.==) (f64m.==)
  def (<) = op2'' (f32m.<) (f64m.<)
  def (>) = op2'' (f32m.>) (f64m.>)
  def (<=) = op2'' (f32m.<=) (f64m.<=)
  def (>=) = op2'' (f32m.>=) (f64m.>=)
  def (!=) = op2'' (f32m.!=) (f64m.!=)

  def neg = op1 f32m.neg f64m.neg
  def max = op2 f32m.max f64m.max
  def min = op2 f32m.min f64m.min

  def sgn = op1 f32m.sgn f64m.sgn
  def abs = op1 f32m.abs f64m.abs

  def sqrt = op1 f32m.sqrt f64m.sqrt

  def log = op1 f32m.log f64m.log
  def log2 = op1 f32m.log2 f64m.log2
  def log10 = op1 f32m.log10 f64m.log10
  def exp = op1 f32m.exp f64m.exp
  def cos = op1 f32m.cos f64m.cos
  def sin = op1 f32m.sin f64m.sin
  def tan = op1 f32m.tan f64m.tan
  def acos = op1 f32m.acos f64m.acos
  def asin = op1 f32m.asin f64m.asin
  def atan = op1 f32m.atan f64m.atan
  def atan2 = op2 f32m.atan2 f64m.atan2
  def gamma = op1 f32m.gamma f64m.gamma
  def lgamma = op1 f32m.lgamma f64m.lgamma
  def lerp (v0: t) (v1: t) (t: t): t = {f32=f32m.lerp v0.f32 v1.f32 t.f32,
                                        f64=f64m.lerp v0.f64 v1.f64 t.f64}

  def ceil = op1 f32m.ceil f64m.ceil
  def floor = op1 f32m.floor f64m.floor
  def trunc = op1 f32m.trunc f64m.trunc

  def round = op1 f32m.round f64m.round

  def to_bits (x: t): int_t = {u32=f32m.to_bits x.f32, u64=f64m.to_bits x.f64}
  def from_bits (x: int_t): t = {f32=f32m.from_bits x.u32, f64=f64m.from_bits x.u64}

  def num_bits: i32 = if settings.enable_f64 then 64 else 32
  def get_bit (bit: i32) = op1'' (f32m.get_bit bit) (f64m.get_bit bit)
  def set_bit (bit: i32) (x: t) (b: i32) = {f32=f32m.set_bit bit x.f32 b, f64=f64m.set_bit bit x.f64 b}

  def isinf = op1'' f32m.isinf f64m.isinf
  def isnan = op1'' f32m.isnan f64m.isnan

  def inf = op0 f32m.inf f64m.inf
  def nan = op0 f32m.nan f64m.nan

  def highest = inf
  def lowest = neg inf

  def pi = op0 f32m.pi f64m.pi
  def e = op0 f32m.e f64m.e

  def sum = op1l f32m.sum f64m.sum
  def product = op1l f32m.product f64m.product
  def maximum = op1l f32m.maximum f64m.maximum
  def minimum = op1l f32m.minimum f64m.minimum
}

-- | Float dual constant.
def fdc (x: f64): float_dual = {f32=f32.f64 x, f64=x}

module vec2_float_dual = mk_vspace_2d float_dual
