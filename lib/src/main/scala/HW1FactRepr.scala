package lib

import lib.quantities.*
import lib.syntax.*

import scala.math.*

import math.exp

class HW1FactRepr[T: DateLike](
    val t0: T,
    val lambda: Double,
    val sigma: PiecewiseConstant,
    measure: Measure[T]
) extends FactRepr[T, Double](measure):

  val factReprVar = new HW1VarFactRepr(t0, lambda, measure)
  export factReprVar.*

  private[lib] def variance(t: T): Double =
    sigma.foldPieces(0.0, tau(t), 0.0): (acc, lo, hi, sig) =>
      acc + sig * sig * factorKernel(lo, hi)

  override def forwardZeroCoupon(
      t: T,
      tStart: T,
      tEnd: T,
      curve: YieldCurve[T]
  ): ExpAffine =
    forwardZeroCoupon(tStart, tEnd, curve, variance(t))

  override def forwardLibor(
      t: T,
      tStart: T,
      tEnd: T,
      rate: Libor[T]
  ): Double => Double =
    forwardLibor(tStart, tEnd, rate, variance(t))

  override def forwardCompoundedRate(
      t: T,
      tStart: T,
      tEnd: T,
      swapRate: CompoundedRate[T]
  ): Double => Double =
    forwardCompoundedRate(tStart, tEnd, swapRate, variance(t))

  override def forwardSwapRate(
      t: T,
      tStart: T,
      tEnd: T,
      rate: SwapLike[T]
  ): Double => Double =
    forwardSwapRate(t, tStart, tEnd, rate, variance(t))

private[lib] class HW1VarFactRepr[T: DateLike](
    t0: T,
    lambda: Double,
    measure: Measure[T]
):

  private given DayCounter = DayCounter.Act365

  def tau(t: T): Double = t0.yearFractionTo(t).value

  def factorKernel(a: Double, b: Double): Double =
    if lambda == 0.0 then b - a
    else (exp(2.0 * lambda * b) - exp(2.0 * lambda * a)) / (2.0 * lambda)

  def n(x: Double): Double =
    if lambda == 0.0 then x else (1.0 - exp(-lambda * x)) / lambda

  def B(tStart: T, tEnd: T): Double =
    val ts = tau(tStart)
    val te = tau(tEnd)
    exp(-lambda * ts) * n(te - ts)

  def A(tStart: T, tEnd: T, vx: Double, curve: YieldCurve[T]): Double =
    val bte = B(tStart, tEnd)
    val bts = B(tStart, measure.tStar)
    curve.discount(tStart, tEnd) * exp(-0.5 * bte * bte * vx + bte * bts * vx)

  // log-normal forward E_t^{T_p} { 1 + dcf(Ts, Te) * L(Tf, Ts, Te) }
  inline def logNormalForward(
      rate: Libor[T],
      tFixing: T,
      vFixing: Double
  ): (Double, Double) =
    val (tStart, tEnd) = rate.interestPeriod(tFixing)
    val totalVar = pow(B(tStart, tEnd), 2) * vFixing
    totalVar -> 1 / A(tStart, tEnd, vFixing, rate.resetCurve) * exp(0.5 * totalVar)

  def forwardZeroCoupon(
      tStart: T,
      tEnd: T,
      curve: YieldCurve[T],
      vx: Double
  ): ExpAffine =
    val a = A(tStart, tEnd, vx, curve)
    val b = B(tStart, tEnd)
    ExpAffine(a, -b)

  def forwardLibor(
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

  // TODO might remove in favor of `forwardLibor`
  // with rate used as the compounding rate
  def forwardCompoundedRate(
      tStart: T,
      tEnd: T,
      rate: CompoundedRate[T],
      vx: Double
  ): Double => Double =
    require(
      DateLike[T].compare(t0, tStart) < 0,
      s"compounded rate is not forward looking with t0=$t0 and tStart=$tStart"
    )
    require(
      tStart == rate.from && tEnd == rate.to,
      "start and end dates don't match that of the compounded rate"
    )
    require(
      measure.currency == rate.compoundingRate.currency,
      s"measure currency must match the currency of the underlying"
    )
    val zcFn = forwardZeroCoupon(tStart, tEnd, rate.compoundingRate.resetCurve, vx)
    val dcf = DateLike[T].yearFraction(tStart, tEnd)(using rate.compoundingRate.dayCounter).value
    x => (1.0 / zcFn(x) - 1.0) / dcf

  def forwardSwapRate(
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
