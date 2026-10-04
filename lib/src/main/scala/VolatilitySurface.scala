package lib

import lib.syntax.{ *, given }
import lib.utils.BinarySearch

import math.sqrt
import math.pow

trait VolatilitySurface[T]:

  def apply(maturity: T): VolatilitySmile

object VolatilitySurface:

  def apply[T: DateLike](
      tRef: T,
      forward: Forward[T],
      smiles: IndexedSeq[(T, Lazy[VolatilitySmile])]
  ): VolatilitySurface[T] =
    val ts = smiles.map(_(0))
    require(
      ts.isStrictlyIncreasing,
      s"pillar maturities must be strictly increasing, got ${ts.mkString(",")}"
    )

    val tMin = smiles.head(0)
    val tMax = smiles.last(0)

    given DayCounter = DayCounter.Act365

    t =>

      val fwd = forward(t)

      def blend(
          fn0: Double => Double,
          gn0: Double => Double,
          fn1: Double => Double,
          gn1: Double => Double,
          f0: Double,
          f1: Double,
          w: Double,
          dt: Double,
          dt0: Double,
          dt1: Double,
          m: Double
      ) =
        val k0 = f0 + m
        val k1 = f1 + m
        1.0 / dt * ((1.0 - w) * dt0 * fn0(k0) * gn0(k0) + w * dt1 * fn1(k1) * gn1(k1))

      def mkFn(
          f: VolatilitySmile => Double => Double,
          interp: Int => Double => Double
      ): Double => Double =
        if t < tMin || smiles.size == 1 then
          val g = f(smiles.head(1).value)
          val fMin = forward(tMin)
          (m: Double) => g(fMin + m)
        else if t > tMax then
          val g = f(smiles.last(1).value)
          val fMax = forward(tMax)
          (m: Double) => g(fMax + m)
        else
          smiles.searchBy(_(0))(t) match
            case BinarySearch.Found(i) =>
              val g = f(smiles(i)(1).value)
              (m: Double) => g(m + fwd)
            case BinarySearch.InsertionLoc(idx) => interp(idx)

      def interp(iloc: Int): Double => Double =
        val (t0, s0) = smiles(iloc - 1)
        val (t1, s1) = smiles(iloc)
        val v0: Double => Double = s0.value.apply
        val v1: Double => Double = s1.value.apply
        val w = t0.yearFractionTo(t) / t0.yearFractionTo(t1)
        val dt = tRef.yearFractionTo(t).value
        val dt0 = tRef.yearFractionTo(t0).value
        val dt1 = tRef.yearFractionTo(t1).value
        val f0 = forward(t0)
        val f1 = forward(t1)
        (m: Double) => sqrt(blend(v0, v0, v1, v1, f0, f1, w, dt, dt0, dt1, m))

      def fstDerivativeInterp(iloc: Int): Double => Double =
        val (t0, s0) = smiles(iloc - 1)
        val (t1, s1) = smiles(iloc)
        val sk0 = s0.value
        val sk1 = s1.value
        val v0 = sk0.apply
        val v1 = sk1.apply
        val d0 = sk0.fstDerivative
        val d1 = sk1.fstDerivative
        val w = t0.yearFractionTo(t) / t0.yearFractionTo(t1)
        val dt = tRef.yearFractionTo(t).value
        val dt0 = tRef.yearFractionTo(t0).value
        val dt1 = tRef.yearFractionTo(t1).value
        val f0 = forward(t0)
        val f1 = forward(t1)
        (m: Double) =>
          val c = blend(v0, v0, v1, v1, f0, f1, w, dt, dt0, dt1, m)
          blend(v0, d0, v1, d1, f0, f1, w, dt, dt0, dt1, m) / sqrt(c)

      def sndDerivativeInterp(iloc: Int): Double => Double =
        val (t0, s0) = smiles(iloc - 1)
        val (t1, s1) = smiles(iloc)
        val sk0 = s0.value
        val sk1 = s1.value
        val v0 = sk0.apply
        val v1 = sk1.apply
        val d0 = sk0.fstDerivative
        val d1 = sk1.fstDerivative
        val e0 = sk0.sndDerivative
        val e1 = sk1.sndDerivative
        val w = t0.yearFractionTo(t) / t0.yearFractionTo(t1)
        val dt = tRef.yearFractionTo(t).value
        val dt0 = tRef.yearFractionTo(t0).value
        val dt1 = tRef.yearFractionTo(t1).value
        val f0 = forward(t0)
        val f1 = forward(t1)
        (m: Double) =>
          val c = blend(v0, v0, v1, v1, f0, f1, w, dt, dt0, dt1, m)
          val cx = blend(v0, d0, v1, d1, f0, f1, w, dt, dt0, dt1, m)
          val a = blend(d0, d0, d1, d1, f0, f1, w, dt, dt0, dt1, m)
          val b = blend(v0, e0, v1, e1, f0, f1, w, dt, dt0, dt1, m)
          (a + b) / sqrt(c) - pow(cx, 2) / c / sqrt(c)

      val applyFn = mkFn(_.apply, interp)
      val fstDerivativeFn = mkFn(_.fstDerivative, fstDerivativeInterp)
      val sndDerivativeFn = mkFn(_.sndDerivative, sndDerivativeInterp)

      new VolatilitySmile:
        def apply(k: Double): Double = applyFn(k - fwd)
        def fstDerivative(k: Double): Double = fstDerivativeFn(k - fwd)
        def sndDerivative(k: Double): Double = sndDerivativeFn(k - fwd)

  def flat[T](vol: Double): VolatilitySurface[T] = _ => VolatilitySmile.flat(vol)

  def fromMoneynessSmile[T](
      forward: Forward[T],
      moneynesses: Seq[Double],
      vols: Seq[Double]
  ): VolatilitySurface[T] = new VolatilitySurface[T]:
    def apply(maturity: T): VolatilitySmile =
      val f = forward(maturity)
      val strikes = moneynesses.map(_ + f)
      VolatilitySmile(strikes.toIndexedSeq, vols.toIndexedSeq)
