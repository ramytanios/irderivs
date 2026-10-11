package lib

import lib.quantities.Tenor
import lib.syntax.*

sealed trait SwapLike[T] extends Underlying[T]:

  def floatingRate: Libor[T]

  def fixedDayCounter: DayCounter

  def discountCurve: YieldCurve[T]

  def fixedSchedule(from: T, to: T): IndexedSeq[FixedCoupon[T]]

  def floatingSchedule(from: T, to: T): IndexedSeq[FloatingCoupon[T]]

class SwapRate[T: DateLike](
    val tenor: Tenor,
    val spotLag: Int,
    val paymentDelay: Int,
    val fixedPeriod: Tenor,
    val floatingRate: Libor[T],
    val fixedDayCounter: DayCounter,
    val calendar: Calendar[T],
    val bdConvention: dtos.BusinessDayConvention,
    val stub: dtos.StubConvention,
    val direction: dtos.Direction,
    val discountCurve: YieldCurve[T]
) extends SwapLike[T]:

  def currency: dtos.Currency = floatingRate.currency

  def interestPeriod(fixingAt: T): (T, T) =
    val startAt = calendar.addBusinessDays(fixingAt, spotLag)
    val endAt = calendar.addBusinessPeriod(startAt, tenor)(using bdConvention)
    startAt -> endAt

  def fixedSchedule(from: T, to: T) =
    Leg.fixed(
      from,
      to,
      fixedPeriod,
      calendar,
      paymentDelay,
      bdConvention,
      stub,
      direction
    )

  def floatingSchedule(from: T, to: T) =
    Leg.floating(
      from,
      to,
      floatingRate.tenor,
      calendar,
      paymentDelay,
      bdConvention,
      floatingRate.settlementRule,
      stub,
      direction
    )

  def forward: Forward[T] =
    t =>
      val (from, to) = interestPeriod(t)

      val fixed = fixedSchedule(from, to)

      val floating = floatingSchedule(from, to)

      val fixedLegValue = fixed.foldMap:
        case FixedCoupon(startAt, endAt, paymentAt) =>
          val dcf = DateLike[T].yearFraction(startAt, endAt)(using fixedDayCounter)
          dcf * discountCurve.discount(paymentAt)

      val floatingLegValue = floating.foldMap:
        case FloatingCoupon(fixingAt, startAt, endAt, paymentAt) =>
          val dcf = DateLike[T].yearFraction(startAt, endAt)(using floatingRate.dayCounter)
          val discount = discountCurve.discount(paymentAt)
          val floatingForward = floatingRate.forward(fixingAt)
          dcf * discount * floatingForward

      floatingLegValue / fixedLegValue

class CompoundedSwapRate[T: DateLike](
    val tenor: Tenor,
    val spotLag: Int,
    val paymentDelay: Int,
    val fixedPeriod: Tenor,
    val floatingRate: Libor[T],
    val floatingPeriod: Tenor,
    val fixedDayCounter: DayCounter,
    val calendar: Calendar[T],
    val bdConvention: dtos.BusinessDayConvention,
    val stub: dtos.StubConvention,
    val direction: dtos.Direction,
    val discountCurve: YieldCurve[T]
) extends SwapLike[T]:

  def currency: dtos.Currency = floatingRate.currency

  def interestPeriod(fixingAt: T): (T, T) =
    val startAt = calendar.addBusinessDays(fixingAt, spotLag)
    val endAt = calendar.addBusinessPeriod(startAt, tenor)(using bdConvention)
    startAt -> endAt

  val resetCurve = floatingRate.resetCurve

  def fixedSchedule(from: T, to: T) =
    Leg.fixed(
      from,
      to,
      fixedPeriod,
      calendar,
      paymentDelay,
      bdConvention,
      stub,
      direction
    )

  def floatingSchedule(from: T, to: T) =
    Leg.fixed(
      from,
      to,
      floatingPeriod,
      calendar,
      paymentDelay,
      bdConvention,
      stub,
      direction
    ).map(c => FloatingCoupon(c.from, c.from, c.to, c.paymentAt))

  def forward: Forward[T] =
    t =>
      val (from, to) = interestPeriod(t)

      val fixed = fixedSchedule(from, to)

      // floating leg behaves like a fixed one's schedule
      val floating = floatingSchedule(from, to)

      val fixedLegValue = fixed.foldMap:
        case FixedCoupon(startAt, endAt, paymentAt) =>
          val dcf = DateLike[T].yearFraction(startAt, endAt)(using fixedDayCounter)
          dcf * discountCurve.discount(paymentAt)

      val floatingLegValue = floating.foldMap:
        case FloatingCoupon(_, startAt, endAt, paymentAt) =>
          discountCurve.discount(paymentAt) * (
            resetCurve.discount(startAt) / resetCurve.discount(endAt) - 1.0
          )

      floatingLegValue / fixedLegValue
