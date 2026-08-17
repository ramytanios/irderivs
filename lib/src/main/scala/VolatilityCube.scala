package lib

import lib.quantities.Tenor
import lib.utils.BinarySearch

import scala.math.Ordering.Implicits.*

trait VolatilityCube[T]:

  def apply(tenor: Tenor): VolatilitySurface[T]

object VolatilityCube:

  def apply[T](
      surfaces: IndexedSeq[(Tenor, VolatilitySurface[T])],
      forwards: Map[Tenor, Forward[T]]
  ): VolatilityCube[T] =
    val tenors = surfaces.map(_(0))
    require(
      tenors.map(_.toYearFraction.value).isStrictlyIncreasing,
      s"surfaces must be order by tenor, got ${tenors.mkString(",")}"
    )

    val tenorMin = surfaces.head(0)
    val tenorMax = surfaces.last(0)

    tenor =>
      t =>
        new VolatilitySkew:

          def impl(k: Double)(f: VolatilitySkew => Double => Double) =
            val m = k - forwards(tenor)(t)
            if tenor < tenorMin then f(surfaces.head(1)(t))(forwards(tenorMin)(t) + m)
            else if tenor > tenorMax then f(surfaces.last(1)(t))(forwards(tenorMax)(t) + m)
            else
              surfaces.searchBy(_(0))(tenor) match
                case BinarySearch.Found(i) => f(surfaces(i)(1)(t))(k)
                case BinarySearch.InsertionLoc(i) =>
                  val (tenorL, surfaceL) = surfaces(i - 1)
                  val (tenorR, surfaceR) = surfaces(i)
                  val w = (tenor.toYearFraction - tenorL.toYearFraction) /
                    (tenorR.toYearFraction - tenorL.toYearFraction)
                  (1 - w) * f(surfaceL(t))(forwards(tenorL)(t) + m) +
                    w * f(surfaceR(t))(forwards(tenorR)(t) + m)

          def apply(k: Double): Double = impl(k)(_.apply)

          def fstDerivative(k: Double): Double = impl(k)(_.fstDerivative)

          def sndDerivative(k: Double): Double = impl(k)(_.sndDerivative)

  def flat[T](vol: Double): VolatilityCube[T] = _ => VolatilitySurface.flat(vol)
