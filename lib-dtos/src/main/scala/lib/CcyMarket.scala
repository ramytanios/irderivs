package lib.dtos

import io.circe.Codec

case class CcyMarket[T](
    curves: Map[CurveId, YieldCurve[T]],
    volatility: Volatility
) derives Codec
