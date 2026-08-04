package lib.api

import lib.*
import lib.dtos
import lib.quantities.Tenor
import lib.syntax.*
import org.apache.commons.math3.distribution.NormalDistribution

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
      val expiryT = rate.calendar.addBusinessPeriod(market.t, expiry)(using rate.bdConvention)
      buildVolSurface(currency, tenor).map(_(expiryT)).map: volSkew =>
        CDFInverter(market.t, expiryT, volSkew, rate.forward, Params()).swap.toOption

  def sampleVolSkew(
      currency: dtos.Currency,
      tenor: Tenor,
      expiry: Tenor,
      nSamplesMiddle: Int,
      nSamplesTail: Int,
      nStdvsTail: Int
  ): Either[lib.Error, Api.SamplingResult] =
    market.volCube(currency).map(_.unit).flatMap: volUnit =>
      val volInUnit = volUnit match
        case dtos.VolUnit.BpPerYear => (v: Double) => v * 10000
      buildVolConventions(currency, tenor).flatMap: rate =>
        given dtos.BusinessDayConvention = rate.bdConvention
        val expiryT = rate.calendar.addBusinessPeriod(market.t, expiry)
        val fwd = rate.forward(expiryT)
        buildVolCube(currency).map: volCube =>
          val dt = market.t.yearFractionTo(expiryT)(using lib.DateLike[T], DayCounter.Act365)
          val volSkew = volCube(tenor)(expiryT)
          val ksQuoted =
            market.volSurface(currency, tenor).toOption.flatMap(_.get(expiry))
              .map(_.unzip._1.map(_.value + fwd)).orEmpty.toList
          val vsQuoted = ksQuoted.map(volSkew andThen volInUnit)
          val impliedPdf = bachelier.impliedDensity(
            fwd,
            dt.value,
            volSkew,
            volSkew.fstDerivative,
            volSkew.sndDerivative
          )
          val pdfQuoted = ksQuoted.map(impliedPdf)
          val atmStdv = volSkew(fwd) * math.sqrt(dt.value)
          val cdfInvN = NormalDistribution(fwd, atmStdv).inverseCumulativeProbability
          val ksMiddle = (1 to nSamplesMiddle).map(i => cdfInvN(i / (nSamplesMiddle + 1.0)))
          val ksRight = ksMiddle.lastOption.flatMap: kmMax =>
            ksQuoted.lastOption.map: kqMax =>
              val kMax0 = Iterator.iterate(kmMax)(_ + atmStdv).find(_ >= kqMax).get
              val kMax = kMax0 + nStdvsTail * atmStdv
              uniform(kmMax, kMax, nSamplesTail).toList
          .orEmpty
          val ksLeft = ksMiddle.headOption.flatMap: kmMin =>
            ksQuoted.headOption.map: kqMin =>
              val kMin0 = Iterator.iterate(kmMin)(_ - atmStdv).find(_ <= kqMin).get
              val kMin = kMin0 - nStdvsTail * atmStdv
              uniform(kMin, kmMin, nSamplesTail).toList
          .orEmpty
          val ks = ksLeft ++ ksMiddle ++ ksRight
          val vs = ks.map(volSkew andThen volInUnit)
          val pdf = ks.map(impliedPdf)
          Api.SamplingResult(ksQuoted, vsQuoted, pdfQuoted, ks, vs, pdf, fwd)

object Api:

  case class SamplingResult(
      quotedStrikes: List[Double],
      quotedVols: List[Double],
      quotedPdf: List[Double],
      strikes: List[Double],
      vols: List[Double],
      pdf: List[Double],
      fwd: Double
  )
