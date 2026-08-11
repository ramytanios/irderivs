package jsonrpc

import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import lib.api.Api
import lib.api.Market
import lib.dtos
import lib.dtos.given_Codec_LocalDate

import java.time.LocalDate

object Handler:

  case class PriceParams(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      payoff: dtos.Payoff[LocalDate],
      static: dtos.Static[LocalDate]
  ) derives Codec

  case class ArbitrageParams(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      static: dtos.Static[LocalDate],
      currency: dtos.Currency,
      tenor: dtos.Tenor,
      expiry: dtos.Tenor
  ) derives Codec

  case class ArbitrageMatrixParams(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      static: dtos.Static[LocalDate],
      currency: dtos.Currency
  ) derives Codec

  case class VolSamplingParams(
      tRef: LocalDate,
      market: Map[dtos.Currency, dtos.CcyMarket[LocalDate]],
      static: dtos.Static[LocalDate],
      currency: dtos.Currency,
      tenor: dtos.Tenor,
      expiry: dtos.Tenor,
      nSamplesMiddle: Int,
      nSamplesTail: Int,
      nStdvsTail: Int
  ) derives Codec

  private def impl[P: Decoder](
      r: JsonRpc.Request,
      f: P => Either[lib.Error, Json]
  ): JsonRpc.Response =
    r.params.fold(
      JsonRpc.error(JsonRpc.ErrorCode.InvalidParams, "missing params", r.id)
    )(_.as[P].fold(
      th => JsonRpc.error(JsonRpc.ErrorCode.InvalidParams, th.getMessage, r.id),
      f(_).fold(
        th => JsonRpc.error(JsonRpc.ErrorCode.InternalError, th.getMessage, r.id),
        js => JsonRpc.success(js, r.id)
      )
    ))

  private def arbitrageToJson(arb: Option[lib.Arbitrage]): Json =
    arb.map:
      case lib.Arbitrage.LeftAsymptoticCDF =>
        JsonObject("type" -> "LeftAsymptoticCDF".asJson).toJson
      case lib.Arbitrage.LeftAsymptoticPut =>
        JsonObject("type" -> "LeftAsymptoticPut".asJson).toJson
      case lib.Arbitrage.RightAsymptoticCDF =>
        JsonObject("type" -> "RightAsymptoticCDF".asJson).toJson
      case lib.Arbitrage.RightAsymptoticCall =>
        JsonObject("type" -> "RightAsymptoticCall".asJson).toJson
      case lib.Arbitrage.Density(leftStrike, rightStrike) =>
        JsonObject(
          "type" -> "Density".asJson,
          "between" -> List(leftStrike, rightStrike).asJson
        ).toJson
    .asJson

  def apply(request: JsonRpc.Request): JsonRpc.Response =
    request.method match
      case "price" => impl[PriceParams](
          request,
          params =>
            val market = Market[LocalDate](params.tRef, params.market, params.static)
            new Api(market).price(params.payoff).map(res => JsonObject("price" -> res.asJson).toJson)
        )

      case "arbitrage" => impl[ArbitrageParams](
          request,
          params =>
            val market = Market[LocalDate](params.tRef, params.market, params.static)
            new Api(market).arbitrageCheck(params.currency, params.tenor, params.expiry)
              .map(res => JsonObject("arbitrage" -> arbitrageToJson(res)).toJson)
        )

      case "arbitrage-matrix" => impl[ArbitrageMatrixParams](
          request,
          params =>
            val market = Market[LocalDate](params.tRef, params.market, params.static)
            market.volCube(params.currency).flatMap:
              case dtos.Volatility.Cube(cube, _) =>
                val tenors = cube.keysIterator.toList
                val expiries = cube.values.flatMap(_.keysIterator).toList.distinct
                val api = new Api(market)
                tenors.flatTraverse: tenor =>
                  expiries.traverse: expiry =>
                    api.arbitrageCheck(params.currency, tenor, expiry).map((tenor, expiry, _))
                .map: matrix =>
                  JsonObject("matrix" -> matrix.map((te, ex, ar) =>
                    (te, ex, arbitrageToJson(ar)).asJson
                  ).asJson).toJson
              case dtos.Volatility.Flat(_) =>
                lib.Error.Generic("arbitrage matrix does not support flat cube").asLeft
        )

      case "vol-sampling" => impl[VolSamplingParams](
          request,
          params =>
            val market = Market[LocalDate](params.tRef, params.market, params.static)
            new Api(market).sampleVolSkew(
              params.currency,
              params.tenor,
              params.expiry,
              params.nSamplesMiddle,
              params.nSamplesTail,
              params.nStdvsTail
            ).map(res =>
              JsonObject(
                "quotedStrikes" -> res.quotedStrikes.asJson,
                "quotedVols" -> res.quotedVols.asJson,
                "quotedPdf" -> res.quotedPdf.asJson,
                "strikes" -> res.strikes.asJson,
                "vols" -> res.vols.asJson,
                "pdf" -> res.pdf.asJson,
                "fwd" -> res.fwd.asJson
              ).toJson
            )
        )

      case other =>
        JsonRpc.error(JsonRpc.ErrorCode.MethodNotFound, s"method $other not found", request.id)
