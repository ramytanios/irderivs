package lib.api

import cats.syntax.all.*
import io.circe.Codec
import io.circe.parser.*
import lib.EitherSyntax
import lib.dtos
import lib.dtos.given_Codec_LocalDate
import lib.quantities.Tenor

import java.time.LocalDate
import scala.io.Source
import scala.util.Using

class JsonParsing extends munit.FunSuite with EitherSyntax:

  type T = LocalDate

  def readResource(name: String) =
    Using.resource(Source.fromResource(name))(_.mkString)

  test("parsing market json"):

    case class Js(
        tRef: T,
        market: dtos.Market[T],
        static: dtos.Static[T]
    ) derives Codec

    val js = readResource("market.json")
    decode[Js](js).leftMap(err => lib.Error.Generic(err.getMessage)).failOrAssert(_ => ())

  test("caplet price"):

    case class Js(
        tRef: T,
        payoff: dtos.Payoff[T],
        market: dtos.Market[T],
        static: dtos.Static[T]
    ) derives Codec

    val js = readResource("price.json")
    decode[Js](js)
      .leftMap: err =>
        lib.Error.Generic(err.getMessage)
      .flatMap: js =>
        val market = Market(js.tRef, js.market, js.static)
        val caplet = js.payoff
        new Api(market).price(caplet)
      .failOrAssert: price =>
        assertEqualsDouble(price, 0.0025670027485647476, 1e-10)

  test("arbitrage check"):

    case class Js(
        tRef: T,
        market: dtos.Market[T],
        static: dtos.Static[T]
    ) derives Codec

    val js = readResource("market.json")
    decode[Js](js)
      .leftMap: err =>
        lib.Error.Generic(err.getMessage)
      .flatMap: js =>
        val market = Market(js.tRef, js.market, js.static)
        new Api(market).arbitrageMatrix(dtos.Currency.USD, List(Tenor.`3M`), List(Tenor.`1Y`))
      .failOrAssert(arb => assert(!arb.headOption.exists(_(1).isEmpty)))

  test("vol sampling"):

    case class Js(
        tRef: T,
        market: dtos.Market[T],
        static: dtos.Static[T]
    ) derives Codec

    val js = readResource("market.json")
    decode[Js](js)
      .leftMap: err =>
        lib.Error.Generic(err.getMessage)
      .flatMap: js =>
        val market = Market(js.tRef, js.market, js.static)
        new Api(market).sampleVolSmile(dtos.Currency.USD, Tenor.`3M`, Tenor.`1Y`, 100, 20, 5)
      .failOrAssert(_ => ())
