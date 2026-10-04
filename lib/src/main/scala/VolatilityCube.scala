package lib

import lib.quantities.Tenor
import lib.utils.BinarySearch

import scala.math.Ordering.Implicits.*

trait VolatilityCube[T]:

  def apply(tenor: Tenor): VolatilitySurface[T]

object VolatilityCube:

  def apply[T](
      surfaces: IndexedSeq[(Tenor, VolatilitySurface[T])],
      forward: Tenor => Forward[T]
  ): VolatilityCube[T] =
    val tenors = surfaces.map(_(0))
    require(
      tenors.map(_.toYf.value).isStrictlyIncreasing,
      s"surfaces must be order by tenor, got ${tenors.mkString(",")}"
    )

    val tenorMin = surfaces.head(0)
    val tenorMax = surfaces.last(0)

    tenor =>

      val mkFns: (T, Double) => (VolatilitySmile => Double => Double) => Double => Double =
        if tenor < tenorMin then
          val pillar = surfaces.head(1)
          val pillarFwd = forward(tenorMin)
          (t: T, _: Double) =>
            val s = pillar(t)
            val fMin = pillarFwd(t)
            f => (m: Double) => f(s)(fMin + m)
        else if tenor > tenorMax then
          val pillar = surfaces.last(1)
          val pillarFwd = forward(tenorMax)
          (t: T, _: Double) =>
            val s = pillar(t)
            val fMax = pillarFwd(t)
            f => (m: Double) => f(s)(fMax + m)
        else
          surfaces.searchBy(_(0))(tenor) match
            case BinarySearch.Found(i) =>
              val pillar = surfaces(i)(1)
              (t: T, fwd: Double) =>
                val s = pillar(t)
                f => (m: Double) => f(s)(fwd + m)
            case BinarySearch.InsertionLoc(i) =>
              val (tenorL, surfaceL) = surfaces(i - 1)
              val (tenorR, surfaceR) = surfaces(i)
              val w = (tenor.toYf - tenorL.toYf) / (tenorR.toYf - tenorL.toYf)
              (t: T, _: Double) =>
                val sL = surfaceL(t)
                val sR = surfaceR(t)
                val fL = forward(tenorL)(t)
                val fR = forward(tenorR)(t)
                f => (m: Double) => (1 - w) * f(sL)(fL + m) + w * f(sR)(fR + m)

      val forwardFn = forward(tenor)

      t =>

        val fwd = forwardFn(t)
        val mk = mkFns(t, fwd)

        val applyFnM = mk(_.apply)
        val fstDerivativeFnM = mk(_.fstDerivative)
        val sndDerivativeFnM = mk(_.sndDerivative)

        new VolatilitySmile:
          def apply(k: Double): Double = applyFnM(k - fwd)
          def fstDerivative(k: Double): Double = fstDerivativeFnM(k - fwd)
          def sndDerivative(k: Double): Double = sndDerivativeFnM(k - fwd)

  def flat[T](vol: Double): VolatilityCube[T] = _ => VolatilitySurface.flat(vol)
