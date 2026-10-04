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

      def mkFn(f: VolatilitySmile => Double => Double): (t: T, fwd: Double) => Double => Double =
        if tenor < tenorMin then
          (t: T, _: Double) =>
            val g = f(surfaces.head(1)(t))
            val fMin = forward(tenorMin)(t)
            (m: Double) => g(fMin + m)
        else if tenor > tenorMax then
          (t: T, _: Double) =>
            val g = f(surfaces.last(1)(t))
            val fMax = forward(tenorMax)(t)
            (m: Double) => g(fMax + m)
        else
          surfaces.searchBy(_(0))(tenor) match
            case BinarySearch.Found(i) =>
              (t: T, fwd: Double) =>
                val g = f(surfaces(i)(1)(t))
                (m: Double) => g(fwd + m)
            case BinarySearch.InsertionLoc(i) =>
              val (tenorL, surfaceL) = surfaces(i - 1)
              val (tenorR, surfaceR) = surfaces(i)
              val w = (tenor.toYf - tenorL.toYf) / (tenorR.toYf - tenorL.toYf)
              (t: T,_: Double) =>
                val gL = f(surfaceL(t))
                val gR = f(surfaceR(t))
                val fL = forward(tenorL)(t)
                val fR = forward(tenorR)(t)
                (m: Double) => (1 - w) * gL(fL + m) + w * gR(fR + m)

      val applyFn = mkFn(_.apply)
      val fstDerivativeFn = mkFn(_.fstDerivative)
      val sndDerivativeFn = mkFn(_.sndDerivative)

      val forwardFn = forward(tenor)

      t =>

        val fwd = forwardFn(t)
        val applyFnM = applyFn(t, fwd)
        val fstDerivativeFnM = fstDerivativeFn(t, fwd)
        val sndDerivativeFnM = sndDerivativeFn(t, fwd)

        new VolatilitySmile:
          def apply(k: Double): Double = applyFnM(k - fwd)
          def fstDerivative(k: Double): Double = fstDerivativeFnM(k - fwd)
          def sndDerivative(k: Double): Double = sndDerivativeFnM(k - fwd)

  def flat[T](vol: Double): VolatilityCube[T] = _ => VolatilitySurface.flat(vol)
