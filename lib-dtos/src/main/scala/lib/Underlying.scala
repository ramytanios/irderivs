package lib.dtos

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.Configuration

object Underlying:
  given Configuration = Configuration.default.withDiscriminator("type")
  given Codec[Underlying] = Codec.AsObject.derivedConfigured

enum Underlying:

  case Libor(
      currency: Currency,
      tenor: Tenor,
      spotLag: Int,
      dayCounter: DayCounter,
      calendar: CalendarId,
      resetCurve: CurveId,
      bdConvention: BusinessDayConvention
  ) extends Underlying

  case SwapRate(
      tenor: Tenor,
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: Tenor,
      floatingRate: RateId,
      fixedDayCounter: DayCounter,
      calendar: CalendarId,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction,
      discountCurve: CurveId
  ) extends Underlying

  case CompoundedSwapRate(
      tenor: Tenor,
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: Tenor,
      floatingRate: RateId,
      floatingPeriod: Tenor,
      fixedDayCounter: DayCounter,
      calendar: CalendarId,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction,
      discountCurve: CurveId
  ) extends Underlying
