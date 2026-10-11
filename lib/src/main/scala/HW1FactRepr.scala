package lib

import lib.quantities.*
import lib.syntax.*

import math.exp

class HW1FactRepr[T: DateLike](
    val t0: T,
    val lambda: Double,
    val sigma: PiecewiseConstant,
    measure: Measure[T]
) extends FactRepr[T, Double](measure):

  private given DayCounter = DayCounter.Act365
  private def tau(t: T): Double = t0.yearFractionTo(t).value

  private[lib] def variance(t: T): Double =
    sigma.foldPieces(0.0, tau(t), 0.0): (acc, lo, hi, sig) =>
      acc + sig * sig * factorKernel(lo, hi)

  private[lib] def factorKernel(a: Double, b: Double): Double =
    if lambda == 0.0 then b - a
    else (exp(2.0 * lambda * b) - exp(2.0 * lambda * a)) / (2.0 * lambda)

  private[lib] def n(x: Double): Double =
    if lambda == 0.0 then x else (1.0 - exp(-lambda * x)) / lambda

  private[lib] def B(tStart: T, tEnd: T): Double =
    val ts = tau(tStart)
    val te = tau(tEnd)
    exp(-lambda * ts) * n(te - ts)

  private[lib] def A(tStart: T, tEnd: T, vx: Double, curve: YieldCurve[T]): Double =
    val bte = B(tStart, tEnd)
    val bts = B(tStart, measure.tStar)
    curve.discount(tStart, tEnd) * exp(-0.5 * bte * bte * vx + bte * bts * vx)

  private[lib] def forwardZeroCoupon(
      tStart: T,
      tEnd: T,
      curve: YieldCurve[T],
      vx: Double
  ): ExpAffine =
    val a = A(tStart, tEnd, vx, curve)
    val b = B(tStart, tEnd)
    ExpAffine(a, b)

  override def forwardZeroCoupon(
      t: T,
      tStart: T,
      tEnd: T,
      curve: YieldCurve[T]
  ): ExpAffine =
    forwardZeroCoupon(tStart, tEnd, curve, variance(t))

  override def zeroCoupon(
      tStart: T,
      tEnd: T,
      curve: YieldCurve[T]
  ): ExpAffine =
    forwardZeroCoupon(tStart, tStart, tEnd, curve)

  private[lib] def forwardLibor(
      tStart: T,
      tEnd: T,
      rate: Libor[T],
      vx: Double
  ): Double => Double =
    require(
      measure.currency == rate.currency,
      s"measure currency must match the currency of the underlying"
    )
    val zcFn = forwardZeroCoupon(tStart, tEnd, rate.resetCurve, vx)
    val dcf = DateLike[T].yearFraction(tStart, tEnd)(using rate.dayCounter).value
    x => (1.0 / zcFn(x) - 1.0) / dcf

  override def forwardLibor(
      t: T,
      tStart: T,
      tEnd: T,
      rate: Libor[T]
  ): Double => Double =
    forwardLibor(tStart, tEnd, rate, variance(t))

  private[lib] def forwardCompoundedRate(
      tStart: T,
      tEnd: T,
      rate: CompoundedRate[T],
      vx: Double
  ): Double => Double =
    require(
      measure.currency == rate.compoundingRate.currency,
      s"measure currency must match the currency of the underlying"
    )
    val zcFn = forwardZeroCoupon(tStart, tEnd, rate.compoundingRate.resetCurve, vx)
    val dcf = DateLike[T].yearFraction(tStart, tEnd)(using rate.compoundingRate.dayCounter).value
    x => (1.0 / zcFn(x) - 1.0) / dcf

  override def forwardCompoundedRate(
      t: T,
      tStart: T,
      tEnd: T,
      swapRate: CompoundedRate[T]
  ): Double => Double =
    forwardCompoundedRate(tStart, tEnd, swapRate, variance(t))

  private[lib] def forwardSwapRate(
      t: T,
      tStart: T,
      tEnd: T,
      rate: SwapLike[T],
      vx: Double
  ): Double => Double =
    require(
      measure.currency == rate.floatingRate.currency,
      s"measure currency must match the currency of the underlying"
    )
    val fixedFns = rate.fixedSchedule(tStart, tEnd).map:
      case FixedCoupon(from, to, paymentAt) =>
        val dcf = DateLike[T].yearFraction(from, to)(using rate.fixedDayCounter).value
        val zcFn = forwardZeroCoupon(t, paymentAt, rate.discountCurve, vx)
        (x: Double) => dcf * zcFn(x)

    val isDiscSameReset = rate.discountCurve eq rate.floatingRate.resetCurve

    // Fast path: the floating leg telescopes to P(t, tStart) - P(t, tEnd).
    // Besides the single-curve check below, this also assumes (not enforced here):
    //   - no payment delay, i.e. each coupon pays at its accrual end (paymentAt == to),
    //   - each coupon's reset/interest period coincides with its accrual period.
    val floatingFns = if isDiscSameReset then
      val curve = rate.discountCurve
      val zcFnStart = forwardZeroCoupon(t, tStart, curve, vx)
      val zcFnEnd = forwardZeroCoupon(t, tEnd, curve, vx)
      List((x: Double) => zcFnStart(x) - zcFnEnd(x))
    else
      val fn = rate match
        case sr: SwapRate[T] => (fc: FloatingCoupon[T]) =>
            val (itStart, itEnd) = sr.floatingRate.interestPeriod(fc.fixingAt)
            forwardLibor(itStart, itEnd, sr.floatingRate, vx)
        case csr: CompoundedSwapRate[T] => (fc: FloatingCoupon[T]) =>
            val compRate = CompoundedRate(fc.from, fc.to, csr.floatingRate, csr.stub, csr.direction)
            forwardCompoundedRate(fc.from, fc.to, compRate, vx)
      rate.floatingSchedule(tStart, tEnd).map:
        case c @ FloatingCoupon(fixingAt, from, to, paymentAt) =>
          val dcf = DateLike[T].yearFraction(from, to)(using rate.floatingRate.dayCounter).value
          val zcFn = forwardZeroCoupon(t, paymentAt, rate.discountCurve, vx)
          val uFn = fn(c)
          (x: Double) => dcf * zcFn(x) * uFn(x)

    (x: Double) =>
      floatingFns.foldLeft(0.0)((acc, fn) => acc + fn(x)) /
        fixedFns.foldLeft(0.0)((acc, fn) => acc + fn(x))

  override def forwardSwapRate(
      t: T,
      tStart: T,
      tEnd: T,
      rate: SwapLike[T]
  ): Double => Double =
    forwardSwapRate(t, tStart, tEnd, rate, variance(t))
