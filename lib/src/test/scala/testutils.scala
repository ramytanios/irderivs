package lib

import cats.syntax.all.*

import scala.math.*

object testutils:

  opaque type Mean = Double
  object Mean:
    inline def apply(mean: Double): Mean = mean
    extension (mean: Mean)
      inline def value: Double = mean

  opaque type StdErr = Double
  object StdErr:
    inline def apply(stdErr: Double): StdErr = stdErr
    extension (stdErr: StdErr)
      inline def value: Double = stdErr

  def oneDimPseudoRandomMC(fn: Double => Double, n: Int, v: Double): (StdErr, Mean) =
    val (sum, sumSq) = Rand.normal.map(_ * sqrt(v)).map(fn)
      .view
      .take(n)
      .foldLeft((0.0, 0.0)) { case ((s, sq), y) => (s + y, sq + y * y) }
    val mean = sum / n
    val varY = (sumSq - sum * sum / n) / (n - 1)
    val stdErr = sqrt(varY / n)
    stdErr -> mean
