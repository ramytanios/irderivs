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

    val dL = spline.fstDerivative(kMin)
    val dR = spline.fstDerivative(kMax)

    val aL = dL / vL / w
    val aR = dR / vR / w

    val bL = vL - dL * kMin
    val bR = vR - dR * kMax

    val cL = 2 * pow(dL, 2) / vL / w
    val cR = 2 * pow(dR, 2) / vR / w

    new VolatilitySmile:

      def apply(k: Double): Double =
        if k <= kMin then
          if dL <= 0 then dL * k + bL
          else vL * (1 + w * tanh(aL * (k - kMin)))
        else if k >= kMax then
          if dR >= 0 then dR * k + bR
          else vR * (1 + w * tanh(aR * (k - kMax)))
        else spline(k)

      def fstDerivative(k: Double): Double =
        if k <= kMin then
          if dL <= 0 then dL
          else dL * (1.0 - pow(tanh(aL * (k - kMin)), 2))
        else if k >= kMax then
          if dR >= 0 then dR
          else dR * (1.0 - pow(tanh(aR * (k - kMax)), 2))
        else spline.fstDerivative(k)

      def sndDerivative(k: Double): Double =
        if k <= kMin then
          if dL <= 0 then 0.0
          else
            val t = tanh(aL * (k - kMin))
            cL * t * (t * t - 1)
        else if k >= kMax then
          if dR >= 0 then 0.0
          else
            val t = tanh(aR * (k - kMax))
            cR * t * (t * t - 1)
        else spline.sndDerivative(k)

  def flat(vol: Double): VolatilitySmile =

    new VolatilitySmile:

      def apply(strike: Double): Double = vol

      def fstDerivative(strike: Double): Double = 0.0

      def sndDerivative(strike: Double): Double = 0.0
