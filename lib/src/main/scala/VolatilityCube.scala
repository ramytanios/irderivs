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
      t =>
        new VolatilitySkew:

          def impl(k: Double)(f: VolatilitySkew => Double => Double) =
            val m = k - forward(tenor)(t)
            if tenor < tenorMin then f(surfaces.head(1)(t))(forward(tenorMin)(t) + m)
            else if tenor > tenorMax then f(surfaces.last(1)(t))(forward(tenorMax)(t) + m)
            else
              surfaces.searchBy(_(0))(tenor) match
                case BinarySearch.Found(i) => f(surfaces(i)(1)(t))(k)
                case BinarySearch.InsertionLoc(i) =>
                  val (tenorL, surfaceL) = surfaces(i - 1)
                  val (tenorR, surfaceR) = surfaces(i)
                  val w = (tenor.toYf - tenorL.toYf) / (tenorR.toYf - tenorL.toYf)
                  (1 - w) * f(surfaceL(t))(forward(tenorL)(t) + m) +
                    w * f(surfaceR(t))(forward(tenorR)(t) + m)

          def apply(k: Double): Double = impl(k)(_.apply)

          def fstDerivative(k: Double): Double = impl(k)(_.fstDerivative)

          def sndDerivative(k: Double): Double = impl(k)(_.sndDerivative)

  def flat[T](vol: Double): VolatilityCube[T] = _ => VolatilitySurface.flat(vol)
