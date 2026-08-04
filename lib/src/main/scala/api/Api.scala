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
          fixings <- buildFixings(p.rate)
          price <- caplet.price(market.t, volSurface, fixings)
        yield price

      case p: dtos.Payoff.Swaption[T] =>
        for
          swaption <- buildSwaption(p)
          rate <- swaption.rate.asRight
          volSurface <- buildVolSurface(rate.currency, rate.tenor)
          fixings <- buildFixings(p.rate)
          price <- swaption.price(market.t, volSurface, fixings)
        yield price

      case p: dtos.Payoff.BackwardLookingCaplet[T] =>
        for
          caplet <- buildBackwardLookingCaplet(p)
          volCube <- buildVolCube(caplet.rate.currency)
          fixings <- buildFixings(p.rate)
          price <- caplet.price(market.t, volCube, fixings)
        yield price

  def arbitrageCheck(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor
  ): Either[lib.Error, Option[Arbitrage]] =
    buildVolConventions(currency, tenor).flatMap: rate =>
      val t = rate.calendar.addBusinessPeriod(market.t, expiry)(using rate.bdConvention)
      buildVolSurface(currency, tenor)
        .map(_(t)).map(CDFInverter(market.t, t, _, rate.forward, CDFInverter.Params()).swap.toOption)

  def sampleVolSkew(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor,
      nSamplesMiddle: Int,
      nSamplesTail: Int,
      nStdvsTail: Int
  ): Either[lib.Error, VolSkewSampler.Result] =
    buildVolConventions(currency, tenor).flatMap: rate =>
      val t = rate.calendar.addBusinessPeriod(market.t, expiry)(using rate.bdConvention)
      buildVolCube(currency).map: volCube =>
        val volSkew = volCube(tenor)(t)
        val msQuoted = market.volSurface(currency, tenor)
          .toOption.flatMap(_.get(expiry)).map(_.unzip._1).orEmpty.toList
        VolSkewSampler(
          market.t,
          t,
          msQuoted,
          volSkew,
          rate.forward,
          VolSkewSampler.Params(nSamplesMiddle, nSamplesTail, nStdvsTail)
        )
