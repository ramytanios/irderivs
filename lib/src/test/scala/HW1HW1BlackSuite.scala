package lib

import lib.dtos.OptionType
import lib.literals.*
import lib.quantities.Tenor
import lib.syntax.*
import lib.testsyntax.*
import lib.testutils.oneDimPseudoRandomMC

import java.time.LocalDate
import scala.math.*

class HW1HW1BlackSuite extends munit.FunSuite with lib.EitherSyntax:

  val t0 = d"2026-10-22"

  case class Case(
      caplet: Caplet[LocalDate],
      lambdaD: Double,
      lambdaC: Double,
      hwVolD: PiecewiseConstant,
      hwVolC: PiecewiseConstant,
      blackFXVol: PiecewiseConstant,
      corrDC: PiecewiseConstant,
      corrDFX: PiecewiseConstant,
      corrCFX: PiecewiseConstant
  ):
    val isQuanto: Boolean = caplet.rate.currency != caplet.paymentCurrency

    val model: HW1HW1Black[LocalDate] = new HW1HW1Black(
      t0,
      caplet.paymentCurrency,
      caplet.rate.currency,
      lambdaD -> hwVolD,
      lambdaC -> hwVolC,
      blackFXVol,
      corrDFX,
      corrCFX,
      corrDC
    )

    val factRepr = new HW1HW1BlackFactRepr(
      t0,
      caplet.paymentCurrency,
      caplet.rate.currency,
      lambdaD -> hwVolD,
      lambdaC -> hwVolC,
      blackFXVol,
      corrDFX,
      corrCFX,
      corrDC,
      Measure(caplet.paymentAt, caplet.paymentCurrency)
    )

  object Case:

    def randomize: Rand[Case] =
      for
        caplet <- rand.randCaplet(t0)
        lambdaD <- rand.randHWLambda
        lambdaC <- rand.randHWLambda
        blackFxVol <- Rand.between(0.001, 0.01).map(PiecewiseConstant.flat)
        hwVolD <- Rand.between(0.001, 0.01).map(PiecewiseConstant.flat)
        hwVolC <- Rand.between(0.001, 0.01).map(PiecewiseConstant.flat)
        corrMat <- rand.randCorrMatrix(3)
      yield Case(
        caplet,
        lambdaD,
        lambdaC,
        hwVolD,
        hwVolC,
        blackFxVol,
        PiecewiseConstant.flat(corrMat(0)(1)),
        PiecewiseConstant.flat(corrMat(0)(2)),
        PiecewiseConstant.flat(corrMat(1)(2))
      )

  test("analytical quanto caplets should satisfy put-call parity"):
    Case
      .randomize
      .view
      .filter(_.isQuanto)
      .take(10)
      .foreach: cs =>
        val mo = cs.model
        val caplet = cs.caplet
        val cp = caplet.optionType match
          case OptionType.Call =>
            val c = mo.price(caplet)
            val p = mo.price(caplet.ofKind(OptionType.Put))
            (c, p).tupled
          case OptionType.Put =>
            val p = mo.price(caplet)
            val c = mo.price(caplet.ofKind(OptionType.Call))
            (c, p).tupled
        cp.failOrAssert: (c, p) =>
          val (tStart, tEnd, tPay) = (caplet.startAt, caplet.endAt, caplet.paymentAt)
          val discPay = caplet.discountCurve.discount(tPay)
          val dcf = DateLike[LocalDate].yearFraction(tStart, tEnd)(using caplet.rate.dayCounter)
          val fwd = cs.factRepr.logNormalForward(caplet.rate, caplet.fixingAt)
          assertEqualsDouble(
            (c - p) - discPay * (fwd - (1 + dcf * caplet.strike)),
            0.0,
            1e-10
          )

  test("analytical quanto caplets should match MC prices"):
    val n = 100_000
    Case
      .randomize
      .view
      .filter(_.isQuanto)
      .take(10)
      .foreach: cs =>
        val mo = cs.model
        val factRepr = cs.factRepr
        val tv = mo.price(cs.caplet)
        val fn = factRepr.forwardLibor(
          cs.caplet.fixingAt,
          cs.caplet.startAt,
          cs.caplet.endAt,
          cs.caplet.rate
        )
        val dcf = DateLike[LocalDate].yearFraction(cs.caplet.startAt, cs.caplet.endAt)(
          using cs.caplet.rate.dayCounter
        )
        val discPay = cs.caplet.discountCurve.discount(cs.caplet.paymentAt)
        val (stdErr, mean) = oneDimPseudoRandomMC(
          x =>
            dcf * max(cs.caplet.optionType.sign * (fn(x) - cs.caplet.strike), 0.0),
          n,
          factRepr.varianceForeign(cs.caplet.fixingAt)
        )
        val tvMC = discPay * mean.value
        tv.failOrAssert: tv =>
          assertEqualsDouble(tv, tvMC, 3 * stdErr.value)

  test("price should be exactly the hw single currency price if non quanto"):
    val zeroFXVol = PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(0.0))
    val unitCorr = PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(1.0))
    Case
      .randomize
      .view
      .filterNot(_.isQuanto)
      .take(10)
      .foreach: cs =>
        val priceQuanto = new HW1HW1Black(
          t0,
          cs.caplet.paymentCurrency,
          cs.caplet.rate.currency,
          cs.lambdaD -> cs.hwVolD,
          cs.lambdaD -> cs.hwVolD,
          zeroFXVol,
          unitCorr,
          unitCorr,
          unitCorr
        ).price(cs.caplet)
        val price = new HW1(t0, cs.lambdaD, cs.hwVolD).price(cs.caplet)
        (priceQuanto, price).tupled.failOrAssert: (pQ, p) =>
          assertEqualsDouble(pQ, p, 1e-10)

  test("calibrated fx vol should reproduce the market fx black variances"):

    val fxExpiries = Vector(Tenor.`1Y`, Tenor.`2Y`, Tenor.`10Y`, Tenor.`20Y`)

    case class CalibCase(
        lambdaD: Double,
        lambdaC: Double,
        hwVolD: PiecewiseConstant,
        hwVolC: PiecewiseConstant,
        corrDC: PiecewiseConstant,
        corrDFX: PiecewiseConstant,
        corrCFX: PiecewiseConstant,
        quotes: Vector[HW1HW1Black.FXVanilla[LocalDate]]
    )

    object CalibCase:
      def randomize: Rand[CalibCase] =
        for
          lambdaD <- rand.randHWLambda
          lambdaC <- rand.randHWLambda
          hwVolD <- Rand.between(0.001, 0.002).map(PiecewiseConstant.flat)
          hwVolC <- Rand.between(0.001, 0.002).map(PiecewiseConstant.flat)
          corrMat <- rand.randCorrMatrix(3)
          blackVols <- Rand.between(0.15, 0.2).listOfN(fxExpiries.size)
        yield CalibCase(
          lambdaD,
          lambdaC,
          hwVolD,
          hwVolC,
          PiecewiseConstant.flat(corrMat(0)(1)),
          PiecewiseConstant.flat(corrMat(0)(2)),
          PiecewiseConstant.flat(corrMat(1)(2)),
          fxExpiries.zip(blackVols).map((tenor, vol) => HW1HW1Black.FXVanilla(t0 + tenor, vol))
        )

    def fxBlackVariance(cs: CalibCase, fxVol: PiecewiseConstant, tau: Double): Double =
      HW1HW1Black.fxBlackVariance(
        0.0,
        tau,
        tau,
        cs.lambdaD -> cs.hwVolD,
        cs.lambdaC -> cs.hwVolC,
        fxVol,
        cs.corrDFX,
        cs.corrCFX,
        cs.corrDC
      )

    CalibCase
      .randomize
      .view
      .take(10)
      .foreach: cs =>
        val fxVol = HW1HW1Black.calibrate(
          t0,
          new HW1(t0, cs.lambdaD, cs.hwVolD),
          new HW1(t0, cs.lambdaC, cs.hwVolC),
          cs.corrDFX,
          cs.corrCFX,
          cs.corrDC,
          cs.quotes
        )
        fxVol.failOrAssert: vol =>
          cs.quotes.foreach: q =>
            val tau = q.fixingYf(t0).value
            assertEqualsDouble(
              fxBlackVariance(cs, vol, tau),
              q.blackVol * q.blackVol * tau,
              1e-10
            )
