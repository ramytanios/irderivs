package lib

class PiecewiseConstant(val knots: IndexedSeq[Double], val values: IndexedSeq[Double])
    extends (Double => Double):

  require(
    knots.size == values.size,
    s"arrays's sizes do not match, got ${knots.size} knots and ${values.size} values"
  )
  require(knots.nonEmpty, "expected at least one piece")
  require(knots.isStrictlyIncreasing, "knots must be stricly increasing")

  // right continuous
  def apply(x: Double): Double =
    val i = knots.indexWhere(_ > x)
    if i < 0 then values.last else values(i)

  def *(other: PiecewiseConstant): PiecewiseConstant =
    val newKnots = (knots ++ other.knots).sorted.distinct
    val newValues = newKnots.indices.map(i =>
      if i == 0 then values(0) * other.values(0)
      else
        val knot = newKnots(i - 1)
        this(knot) * other(knot)
    )
    new PiecewiseConstant(newKnots, newValues)

  def foldPieces[A](from: Double, to: Double, z: A)(op: (A, Double, Double, Double) => A): A =
    require(to >= from, "to must be >= from")
    var acc = z
    var lo = from
    while lo < to do
      val i = knots.indexWhere(_ > lo)
      val hi = if i < 0 then to else math.min(knots(i), to)
      val value = if i < 0 then values.last else values(i)
      acc = op(acc, lo, hi, value)
      lo = hi
    acc

  // ∫_from^to this(u) * kernel(u) du, where kernel(lo, hi) = ∫_lo^hi kernel(u) du
  def integrate(from: Double, to: Double)(kernel: (Double, Double) => Double): Double =
    foldPieces(from, to, 0.0): (acc, lo, hi, v) =>
      acc + v * kernel(lo, hi)

object PiecewiseConstant:

  def flat(value: Double): PiecewiseConstant =
    new PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(value))
