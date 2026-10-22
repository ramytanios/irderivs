package lib

import lib.literals.*
import lib.quantities.*
import lib.syntax.*

import java.time.LocalDate
import lib.dtos.Currency
import lib.testutils.oneDimPseudoRandomMC

class HW1HW1BlackFactReprSuite extends munit.FunSuite:

  test("Factorial representation degenarates if foreign and domestic currencies identical"):

    val t0 = d"2026-10-21"

    case class Case(
        libor: Libor[LocalDate],
        lambda: Double,
        t: LocalDate,
        tFixing: LocalDate
    )

    val randCase: Rand[Case] =
      for
        libor <- rand.randLibor(t0)
        lambda <- rand.randHWLambda
        dt = Rand.choose(Tenor.`2M`, Tenor.`3M`, Tenor.`6M`, Tenor.`1Y`)
        t <- dt.map(t0 + _)
        tFixing <- dt.map(t + _)
      yield Case(libor, lambda, t, tFixing)

    randCase
      .view
      .take(10)
      .foreach:
        case Case(libor, lambda, t, tFixing) =>
          val domesticCcy = libor.currency
          val foreignCcy = domesticCcy

          val (tStart, tEnd) = libor.interestPeriod(tFixing)
          val tPay = tEnd + Tenor.`1D`

          val hwParams = lambda -> PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(0.001))
          val zeroFXVol = PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(0.0))
          val unitCorr = PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(1.0))

          val measure = Measure(tPay, domesticCcy)

          val factReprHybrid = new HW1HW1BlackFactRepr[LocalDate](
            t0,
            domesticCcy,
            foreignCcy,
            hwParams,
            hwParams,
            zeroFXVol,
            unitCorr,
            unitCorr,
            unitCorr,
            measure
          )
          val factReprSingle = new HW1FactRepr[LocalDate](t0, hwParams(0), hwParams(1), measure)
          val pHybrid = factReprHybrid.forwardLibor(t, tStart, tEnd, libor)
          val pSingle = factReprSingle.forwardLibor(t, tStart, tEnd, libor)

          // factorial representation degeneration
          val vt = factReprSingle.variance(t)
          Rand.normal.map(_ * math.sqrt(vt))
            .view.take(100)
            .foreach: x =>
              assertEqualsDouble(pHybrid(x), pSingle(x), 1e-10)

          // log-normal forward degeneration
          val forwardQuanto = factReprHybrid.LNF(libor, tFixing)
          val forwardDegen = factReprSingle.factReprVar.LNF(
            libor,
            tFixing,
            factReprSingle.variance(tFixing)
          )(1)
          assertEqualsDouble(forwardQuanto, forwardDegen, 1e-10)

  test("Log-normal forward analytical formula matches MC"):
    val n = 100_000
    val t0 = d"2026-10-21"

    case class Case(
        t: LocalDate,
        tFixing: LocalDate,
        libor: Libor[LocalDate],
        ccyPay: Currency,
        lambdaD: Double,
        lambdaC: Double,
        hwVolD: PiecewiseConstant,
        hwVolC: PiecewiseConstant,
        blackFXVol: PiecewiseConstant,
        corrDC: PiecewiseConstant,
        corrDFX: PiecewiseConstant,
        corrCFX: PiecewiseConstant
    ):

      val isQuanto: Boolean = libor.currency != ccyPay

    val randCase: Rand[Case] =
      val dt = Rand.choose(Tenor.`2M`, Tenor.`3M`, Tenor.`6M`, Tenor.`1Y`)
      for
        t <- dt.map(t0 + _)
        tFixing <- dt.map(t + _)
        libor <- rand.randLibor(t0)
        ccyPay <- rand.randCcy
        lambdaD <- rand.randHWLambda
        lambdaC <- rand.randHWLambda
        blackFxVol <- Rand.between(0.001, 0.01).map(PiecewiseConstant.flat)
        hwVolD <- Rand.between(0.001, 0.01).map(PiecewiseConstant.flat)
        hwVolC <- Rand.between(0.001, 0.01).map(PiecewiseConstant.flat)
        corrMat <- rand.randCorrMatrix(3) // d -> c -> FX
      yield Case(
        t,
        tFixing,
        libor,
        ccyPay,
        lambdaD,
        lambdaC,
        hwVolD,
        hwVolC,
        blackFxVol,
        PiecewiseConstant.flat(corrMat(0)(1)),
        PiecewiseConstant.flat(corrMat(0)(2)),
        PiecewiseConstant.flat(corrMat(1)(2))
      )

    randCase
      .view
      .filter(_.isQuanto)
      .take(10)
      .foreach: c =>
        val t = c.t
        val tFixing = c.tFixing
        val (tStart, tEnd) = c.libor.interestPeriod(tFixing)
        val tPay = tEnd + Tenor.`1D`
        val factReprQuanto =
          new HW1HW1BlackFactRepr(
            t0,
            c.ccyPay,
            c.libor.currency,
            c.lambdaD -> c.hwVolD,
            c.lambdaC -> c.hwVolC,
            c.blackFXVol,
            c.corrDFX,
            c.corrCFX,
            c.corrDC,
            Measure(tPay, c.ccyPay)
          )
        val forwardAnalytical = factReprQuanto.LNF(c.libor, tFixing)
        val dcf = DateLike[LocalDate].yearFraction(tStart, tEnd)(using c.libor.dayCounter)
        val fn =
          val fn0 = factReprQuanto.forwardLibor(tFixing, tStart, tEnd, c.libor)
          (x: Double) => 1 + dcf * fn0(x)
        val vT = factReprQuanto.factReprFor.variance(t)
        val (stdErr, forwardMC) = oneDimPseudoRandomMC(fn, n, vT)
        assertEqualsDouble(forwardAnalytical, forwardMC.value, 3 * stdErr.value)
