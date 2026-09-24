package lib.dtos

import io.circe.*
import io.circe.derivation.Configuration
import io.circe.derivation.ConfiguredCodec
import io.circe.derivation.*

import VolatilityMarketConventions.*

case class VolatilityMarketConventions(
    boundaryTenor: Tenor,
    boundaryTenorKind: BoundaryTenorKind,
    liborRate: Libor,
    swapRate: SwapRate
) derives Codec

object VolatilityMarketConventions:

  object BoundaryTenorKind:
    given Configuration = Configuration.default

  enum BoundaryTenorKind derives ConfiguredEnumCodec:
    case Libor, Swap

  case class Libor(
      currency: Currency,
      spotLag: Int,
      dayCounter: DayCounter,
      calendar: CalendarId,
      resetCurve: Curve,
      bdConvention: BusinessDayConvention
  ) derives Codec

  case class SwapRate(
      spotLag: Int,
      paymentDelay: Int,
      fixedPeriod: Tenor,
      floatingRate: RateId,
      fixedDayCounter: DayCounter,
      calendar: CalendarId,
      bdConvention: BusinessDayConvention,
      stub: StubConvention,
      direction: Direction,
      discountCurve: Curve
  ) derives Codec

object Volatility:
  given Configuration = Configuration.default.withDiscriminator("type")
  given Codec[Volatility] = Codec.AsObject.derivedConfigured

enum Volatility:

  case Cube(
      cube: Map[Tenor, Map[Tenor, List[(Moneyness, Double)]]],
      conventions: VolatilityMarketConventions
  ) extends Volatility

  case Flat(vol: Double) extends Volatility
