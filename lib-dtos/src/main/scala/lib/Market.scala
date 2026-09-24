package lib.dtos

import io.circe.Codec

case class Market[T](
    rates: Map[RateId, Underlying],
    currencies: Map[Currency, CcyMarket[T]]
) derives Codec
