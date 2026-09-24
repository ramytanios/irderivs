package lib

import scala.math.pow
import scala.math.tanh

trait VolatilitySmile:

  def apply(strike: Double): Double

  def fstDerivative(strike: Double): Double

  def sndDerivative(strike: Double): Double

object VolatilitySmile:

  given Conversion[VolatilitySmile, Double => Double] = _.apply

  def apply(
      f: Double => Double,
      fstDeriv: Double => Double,
      sndDeriv: Double => Double
  ): VolatilitySmile =
    new VolatilitySmile:
      override def apply(strike: Double): Double = f(strike)
      override def fstDerivative(strike: Double): Double = fstDeriv(strike)
      override def sndDerivative(strike: Double): Double = sndDeriv(strike)

  def apply(ks: IndexedSeq[Double], vs: IndexedSeq[Double]): VolatilitySmile =

    val spline = CubicSpline(ks, vs)

    val kMin = ks.head
    val kMax = ks.last
    val vL = vs.head
    val vR = vs.last

    val w = 1.0 / 3.0

    new VolatilitySmile:

      def apply(k: Double): Double =
        if k <= kMin then
          val dL = spline.fstDerivative(kMin)
          if dL <= 0 then dL * k + (vL - dL * kMin)
          else vL * (1 + w * tanh(dL / vL / w * (k - kMin)))
        else if k >= kMax then
          val dR = spline.fstDerivative(kMax)
          if dR >= 0 then dR * k + (vR - dR * kMax)
          else vR * (1 + w * tanh(dR / vR / w * (k - kMax)))
        else spline(k)

      def fstDerivative(k: Double): Double =
        if k <= kMin then
          val dL = spline.fstDerivative(kMin)
          if dL <= 0 then dL
          else dL * (1.0 - pow(tanh(dL / vL / w * (k - kMin)), 2))
        else if k >= kMax then
          val dR = spline.fstDerivative(kMax)
          if dR >= 0 then dR
          else dR * (1.0 - pow(tanh(dR / vR / w * (k - kMax)), 2))
        else spline.fstDerivative(k)

      def sndDerivative(k: Double): Double =
        if k <= kMin then
          val dL = spline.fstDerivative(kMin)
          if dL <= 0 then 0.0
          else
            val t = tanh(dL / vL / w * (k - kMin))
            2 * pow(dL, 2) / vL / w * t * (t * t - 1)
        else if k >= kMax then
          val dR = spline.fstDerivative(kMax)
          if dR >= 0 then 0.0
          else
            val t = tanh(dR / vR / w * (k - kMax))
            2 * pow(dR, 2) / vR / w * t * (t * t - 1)
        else spline.sndDerivative(k)

  def flat(vol: Double): VolatilitySmile =

    new VolatilitySmile:

      def apply(strike: Double): Double = vol

      def fstDerivative(strike: Double): Double = 0.0

      def sndDerivative(strike: Double): Double = 0.0
