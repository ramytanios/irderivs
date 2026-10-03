package lib.api

import lib.*
import lib.dtos
import lib.quantities.Tenor
import lib.syntax.*

class Api[T: lib.DateLike](val market: Market[T]):

  val l = new Lib(market)
  import l.*

  def price(payoff: dtos.Payoff[T]): Either[lib.Error, Double] =
    payoff match
      case p: dtos.Payoff.Caplet[T] =>
        for
          caplet <- buildCaplet(p)
          rate <- caplet.rate.asRight
          volSurface <- buildVolSurface(caplet.paymentCurrency, rate.tenor)
          price <- caplet.price(market.t, volSurface)
        yield price

      case p: dtos.Payoff.Cap[T] =>
        for
          cap <- buildCap(p)
          rate <- cap.rate.asRight
          volSurface <- buildVolSurface(cap.paymentCurrency, rate.tenor)
          price <- cap.price(market.t, volSurface)
        yield price

      case p: dtos.Payoff.Swaption[T] =>
        for
          swaption <- buildSwaption(p)
          rate <- swaption.rate.asRight
          volSurface <- buildVolSurface(rate.currency, rate.tenor)
          price <- swaption.price(market.t, volSurface)
        yield price

      case p: dtos.Payoff.BackwardLookingCaplet[T] =>
        for
          rfr <- buildBackwardLookingCaplet(p)
          volCube <- buildVolCube(rfr.rate.currency)
          price <- rfr.price(market.t, volCube)
        yield price

  private def readMarketQuotes(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor
  ): List[(dtos.Moneyness, Double)] =
    market.volSurface(currency, tenor).toOption.flatMap(_.get(expiry)).orEmpty

  def arbitrageMatrix(
      currency: dtos.Currency,
      tenor: List[Tenor],
      expiry: List[Tenor]
  ): Either[lib.Error, List[((Tenor, Tenor), Option[Arbitrage])]] =
    tenor.flatTraverse: tenor =>
      buildVolConventions(currency, tenor).flatMap: rate =>
        buildVolSurface(currency, tenor, rate).flatMap: surface =>
          expiry.traverse: expiry =>
            val t = rate.calendar.addBusinessPeriod(market.t, expiry)(using rate.bdConvention)
            val msQuoted = readMarketQuotes(currency, tenor, expiry).map((m, _) => m)
            val params = CDFInverter.Params()
            CDFInverter(
              market.t,
              t,
              msQuoted,
              surface(t),
              rate.forward,
              params
            ).map(result => (tenor -> expiry) -> result.swap.toOption)

  def sampleVolSmile(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor,
      nSamplesMiddle: Int,
      nSamplesTail: Int,
      nStdvsTail: Int
  ): Either[lib.Error, VolatilitySmileSampler.Result] =
    buildVolConventions(currency, tenor).flatMap: rate =>
      val t = rate.calendar.addBusinessPeriod(market.t, expiry)(using rate.bdConvention)
      buildVolCube(currency).map: volCube =>
        val volSmile = volCube(tenor)(t)
        val msQuoted = readMarketQuotes(currency, tenor, expiry).map((m, _) => m)
        val params = VolatilitySmileSampler.Params(nSamplesMiddle, nSamplesTail, nStdvsTail)
        VolatilitySmileSampler(market.t, t, msQuoted, volSmile, rate.forward, params)

  def sampleVolSmile(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor,
      moneynesses: List[dtos.Moneyness]
  ): Either[lib.Error, List[(dtos.Moneyness, Double)]] =
    buildVolConventions(currency, tenor).flatMap: rate =>
      val t = rate.calendar.addBusinessPeriod(market.t, expiry)(using rate.bdConvention)
      buildVolCube(currency).map: volCube =>
        val volSmile = volCube(tenor)(t)
        val forward = rate.forward(t)
        moneynesses.map(m => m -> volSmile(m.value + forward))
