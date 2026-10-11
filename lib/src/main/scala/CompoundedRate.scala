package lib

import lib.quantities.*
import lib.syntax.{ *, given }
import lib.utils.BinarySearch.Found
import lib.utils.BinarySearch.InsertionLoc

case class CompoundingPeriod[T](
    fixingAt: T,
    startAt: T, // interest period start
    endAt: T // interest period end
)

class CompoundedRate[T: DateLike](
    val compoundingRate: Libor[T],
    val schedule: Vector[CompoundingPeriod[T]],
    val fixings: Map[T, Double]
):

  given DayCounter = compoundingRate.dayCounter

  val from = schedule.head.startAt
  val to = schedule.last.endAt

  val dcf: YearFraction = from.yearFractionTo(to)

  val firstFixingAt = schedule.head.fixingAt
  val lastFixingAt = schedule.last.fixingAt

  def compoundingFactor(toInclusive: T): Double =
    schedule.collect:
      case CompoundingPeriod(fixingAt, startAt, endAt) if fixingAt <= toInclusive =>
        val fixing = fixings.getOrElse(fixingAt, compoundingRate.forward(fixingAt))
        (1 + startAt.yearFractionTo(endAt) * fixing)
    .product

  def fullCompoundingFactor() = compoundingFactor(lastFixingAt)

  def findObservationIdx(t: T): Int =
    schedule.searchBy(_.fixingAt)(t) match
      case Found(i)        => i
      case InsertionLoc(i) => i - 1

  def forward(t: T): Either[Error, Double] =
    Either.raiseWhen(t > lastFixingAt)(
      Error.Generic(s"$t is after last fixing $lastFixingAt")
    ).map: _ =>
      if t < firstFixingAt then 1.0 / compoundingRate.resetCurve.discount(from, to)
      else if t == lastFixingAt then fullCompoundingFactor()
      else
        val obsIdx = findObservationIdx(t)
        val futIdx = obsIdx + 1
        compoundingFactor(schedule(obsIdx).fixingAt) /
          compoundingRate.resetCurve.discount(schedule(futIdx).startAt, to)
    .map(f => (f - 1.0) / dcf.value)

object CompoundedRate:

  def apply[T: DateLike](
      from: T,
      to: T,
      compoundingRate: Libor[T],
      stub: dtos.StubConvention,
      direction: dtos.Direction,
      fixings: Map[T, Double] = Map.empty[T, Double]
  ): CompoundedRate[T] =

    val schedule: Vector[CompoundingPeriod[T]] =
      Schedule(
        from,
        to,
        compoundingRate.tenor,
        compoundingRate.calendar,
        compoundingRate.bdConvention,
        stub,
        direction
      )
        .sliding(2)
        .collect:
          case Seq(t0, t1) =>
            val fixingDate = compoundingRate.settlementRule.fixingAt(t0)
            CompoundingPeriod(fixingDate, t0, t1)
        .toVector

    new CompoundedRate[T](compoundingRate, schedule, fixings)
