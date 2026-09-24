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
      new VolatilitySmile:

        def L(left: (T, Double => Double), right: (T, Double => Double), m: Double) =
          val (tL, fL) = left
          val (tR, fR) = right
          val w = tL.yearFractionTo(t) / tL.yearFractionTo(tR)
          val dt = tRef.yearFractionTo(t).value
          val dtL = tRef.yearFractionTo(tL).value
          val dtR = tRef.yearFractionTo(tR).value
          1.0 / dt * (
            (1.0 - w) * dtL * pow(fL(forward(tL) + m), 2) + w * dtR * pow(fR(forward(tR) + m), 2)
          )

        // bilinear counterpart of `L`: combines the product of two smile functions per
        // pillar instead of the square, so it can express derivatives of `L`.
        def L2(
            left: (T, Double => Double, Double => Double),
            right: (T, Double => Double, Double => Double),
            m: Double
        ) =
          val (tL, fL, gL) = left
          val (tR, fR, gR) = right
          val w = tL.yearFractionTo(t) / tL.yearFractionTo(tR)
          val dt = tRef.yearFractionTo(t).value
          val dtL = tRef.yearFractionTo(tL).value
          val dtR = tRef.yearFractionTo(tR).value
          val kL = forward(tL) + m
          val kR = forward(tR) + m
          1.0 / dt * (
            (1.0 - w) * dtL * fL(kL) * gL(kL) + w * dtR * fR(kR) * gR(kR)
          )

        def apply(k: Double): Double =

          val m = k - forward(t)

          val I = (k: Int) =>
            val (t0, s0) = smiles(k - 1)
            val (t1, s1) = smiles(k)
            sqrt(L(t0 -> s0.value, t1 -> s1.value, m))

          // extrapolation is constant in vol to avoid the latter going negative
          if t < tMin || smiles.size == 1 then smiles.head(1).value(forward(tMin) + m)
          else if t > tMax then smiles.last(1).value(forward(tMax) + m)
          else
            smiles.searchBy(_(0))(t) match
              case BinarySearch.Found(i)        => smiles(i)(1).value(k)
              case BinarySearch.InsertionLoc(i) => I(i)

        def fstDerivative(k: Double): Double =

          val m = k - forward(t)

          val I = (k: Int) =>
            val (t0, s0) = smiles(k - 1)
            val (t1, s1) = smiles(k)
            val sk0 = s0.value
            val sk1 = s1.value
            val v0: Double => Double = sk0.apply
            val v1: Double => Double = sk1.apply
            val d0: Double => Double = sk0.fstDerivative
            val d1: Double => Double = sk1.fstDerivative
            // sigma = sqrt(c), c = L2(vol, vol); sigma' = c'/(2 sqrt c) = L2(vol, vol')/sqrt(c)
            val c = L2((t0, v0, v0), (t1, v1, v1), m)
            L2((t0, v0, d0), (t1, v1, d1), m) / sqrt(c)

          if t < tMin || smiles.size == 1 then smiles.head(1).value.fstDerivative(forward(tMin) + m)
          else if t > tMax then smiles.last(1).value.fstDerivative(forward(tMax) + m)
          else
            smiles.searchBy(_(0))(t) match
              case BinarySearch.Found(i)        => smiles(i)(1).value.fstDerivative(k)
              case BinarySearch.InsertionLoc(i) => I(i)

        def sndDerivative(k: Double): Double =

          val m = k - forward(t)

          val I = (k: Int) =>
            val (t0, s0) = smiles(k - 1)
            val (t1, s1) = smiles(k)
            val sk0 = s0.value
            val sk1 = s1.value
            val v0: Double => Double = sk0.apply
            val v1: Double => Double = sk1.apply
            val d0: Double => Double = sk0.fstDerivative
            val d1: Double => Double = sk1.fstDerivative
            val e0: Double => Double = sk0.sndDerivative
            val e1: Double => Double = sk1.sndDerivative
            // sigma'' = c''/(2 sqrt c) - (c')^2/(4 c^{3/2}), with
            // c = L2(vol,vol), c' = 2 L2(vol,vol'), c'' = 2 (L2(vol',vol') + L2(vol,vol''))
            val c = L2((t0, v0, v0), (t1, v1, v1), m)
            val cx = L2((t0, v0, d0), (t1, v1, d1), m)
            val a = L2((t0, d0, d0), (t1, d1, d1), m)
            val b = L2((t0, v0, e0), (t1, v1, e1), m)
            (a + b) / sqrt(c) - pow(cx, 2) / c / sqrt(c)

          if t < tMin || smiles.size == 1 then smiles.head(1).value.sndDerivative(forward(tMin) + m)
          else if t > tMax then smiles.last(1).value.sndDerivative(forward(tMax) + m)
          else
            smiles.searchBy(_(0))(t) match
              case BinarySearch.Found(i)        => smiles(i)(1).value.sndDerivative(k)
              case BinarySearch.InsertionLoc(i) => I(i)

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
