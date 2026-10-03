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

      def impl(f: VolatilitySmile => Double => Double): (t: T, m: Double) => Double =
        if tenor < tenorMin then (t: T, m: Double) => f(surfaces.head(1)(t))(forward(tenorMin)(t) + m)
        else if tenor > tenorMax then
          (t: T, m: Double) => f(surfaces.last(1)(t))(forward(tenorMax)(t) + m)
        else
          surfaces.searchBy(_(0))(tenor) match
            case BinarySearch.Found(i) =>
              (t: T, m: Double) => f(surfaces(i)(1)(t))(m + forward(tenor)(t))
            case BinarySearch.InsertionLoc(i) =>
              val (tenorL, surfaceL) = surfaces(i - 1)
              val (tenorR, surfaceR) = surfaces(i)
              val w = (tenor.toYf - tenorL.toYf) / (tenorR.toYf - tenorL.toYf)
              (t: T, m: Double) =>
                (1 - w) * f(surfaceL(t))(forward(tenorL)(t) + m) +
                  w * f(surfaceR(t))(forward(tenorR)(t) + m)

      val applyFn = impl(_.apply)
      val fstDerivativeFn = impl(_.fstDerivative)
      val sndDerivativeFn = impl(_.sndDerivative)

      t =>
        new VolatilitySmile:
          def apply(k: Double): Double = applyFn(t, k - forward(tenor)(t))
          def fstDerivative(k: Double): Double = fstDerivativeFn(t, k - forward(tenor)(t))
          def sndDerivative(k: Double): Double = sndDerivativeFn(t, k - forward(tenor)(t))

  def flat[T](vol: Double): VolatilityCube[T] = _ => VolatilitySurface.flat(vol)
