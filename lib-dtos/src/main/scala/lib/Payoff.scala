package lib.dtos

import io.circe.Codec
import io.circe.derivation.*
import io.circe.derivation.ConfiguredCodec

object Payoff:
  given Configuration = Configuration.default.withDiscriminator("type")
  given [T: Codec]: Codec[Payoff[T]] = Codec.AsObject.derivedConfigured

enum Payoff[T]:

  case Caplet[T](
      rate: RateId,
      fixingAt: T,
      startAt: T,
      endAt: T,
      paymentAt: T,
      paymentCurrency: Currency,
      strike: Double,
      discountCurve: Curve,
      optionType: OptionType,
      fixings: Option[List[(T, Double)]]
  ) extends Payoff[T]

  case Swaption[T](
      rate: RateId,
      fixingAt: T,
      strike: Double,
      optionType: OptionType,
      annuity: Annuity,
      discountCurve: Curve,
      fixings: Option[List[(T, Double)]]
  ) extends Payoff[T]

  case BackwardLookingCaplet[T](
      startAt: T,
      endAt: T,
      rate: RateId,
      paymentCurrency: Currency,
      paymentAt: T,
      strike: Double,
      optionType: OptionType,
      discountCurve: Curve,
      stub: StubConvention,
      direction: Direction,
      fixings: Option[List[(T, Double)]]
  ) extends Payoff[T]
