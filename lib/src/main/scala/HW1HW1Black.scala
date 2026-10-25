package lib

import lib.dtos.Currency
import lib.quantities.YearFraction
import lib.syntax.*

import scala.math.*

class HW1HW1Black[T: DateLike](
    val t0: T,
    val ccyDomestic: Currency,
    val ccyForeign: Currency,
    val hwDomestic: (Double, PiecewiseConstant),
    val hwForeign: (Double, PiecewiseConstant),
    val blackFXVol: PiecewiseConstant,
    val corrDomesticFX: PiecewiseConstant,
    val corrForeignFX: PiecewiseConstant,
    val corrDomesticForeign: PiecewiseConstant
):

  def price(caplet: Caplet[T]): Either[lib.Error, Double] =
    val rate = caplet.rate
    val (tStart, tEnd, tPay) = (caplet.startAt, caplet.endAt, caplet.paymentAt)
    val domCurrency = caplet.paymentCurrency
    val dcf = DateLike[T].yearFraction(tStart, tEnd)(using rate.dayCounter)
    val discPay = caplet.discountCurve.discount(tPay)
    val factRepr = new HW1HW1BlackFactRepr[T](
      t0,
      ccyDomestic,
      ccyForeign,
      hwDomestic,
      hwForeign,
      blackFXVol,
      corrDomesticFX,
      corrForeignFX,
      corrDomesticForeign,
      Measure(tPay, domCurrency)
    )
    val forward = factRepr.logNormalForward(rate, caplet.fixingAt)
    val strike = 1 + dcf * caplet.strike
    val b = factRepr.factReprFor.B(tStart, tEnd)
    val variance = b * b * factRepr.varianceForeign(caplet.fixingAt)
    Either.catchNonFatal(
      black.price(caplet.optionType, forward, strike, variance, discPay)
    ).leftMap(th => lib.Error(th.getMessage))

object HW1HW1Black:

  case class FXVanilla[T](tFixing: T, blackVol: Double):
    private given DayCounter = DayCounter.Act365
    def fixingYf(t0: T)(using d: DateLike[T]): YearFraction = t0.yearFractionTo(tFixing)

  // ∫_lo^hi ‖[σd n_d(tau-·), -σc n_c(tau-·), σFX(·)]·√Corr‖² du
  def fxBlackVariance(
      lo: Double,
      hi: Double,
      tau: Double,
      hwDomestic: (Double, PiecewiseConstant),
      hwForeign: (Double, PiecewiseConstant),
      blackFXVol: PiecewiseConstant,
      corrDomesticFX: PiecewiseConstant,
      corrForeignFX: PiecewiseConstant,
      corrDomesticForeign: PiecewiseConstant
  ): Double =
    if hi <= lo then 0.0
    else
      val (lambdaD, sigmaD) = hwDomestic
      val (lambdaC, sigmaC) = hwForeign
      val bondD = (sigmaD * sigmaD)
        .integrate(lo, hi)(nTimesN(lambdaD, lambdaD, tau, _, _))
      val bondC = (sigmaC * sigmaC)
        .integrate(lo, hi)(nTimesN(lambdaC, lambdaC, tau, _, _))
      val bondDC = (sigmaC * sigmaD * corrDomesticForeign)
        .integrate(lo, hi)(nTimesN(lambdaC, lambdaD, tau, _, _))
      val crossD = (blackFXVol * sigmaD * corrDomesticFX)
        .integrate(lo, hi)(nInt(lambdaD, tau, _, _))
      val crossC = (blackFXVol * sigmaC * corrForeignFX)
        .integrate(lo, hi)(nInt(lambdaC, tau, _, _))
      val fxSq = (blackFXVol * blackFXVol)
        .integrate(lo, hi)((l, h) => h - l)
      bondD + bondC - 2 * bondDC + 2 * (crossD - crossC) + fxSq

  def calibrate[T: DateLike](
      t0: T,
      hwDomestic: HW1[T],
      hwForeign: HW1[T],
      corrDomesticFX: PiecewiseConstant,
      corrForeignFX: PiecewiseConstant,
      corrDomesticForeign: PiecewiseConstant,
      quotes: Vector[FXVanilla[T]]
  ): Either[lib.Error, PiecewiseConstant] =

    val (lambdaD, sigmaD) = hwDomestic.lambda -> hwDomestic.sigma
    val (lambdaC, sigmaC) = hwForeign.lambda -> hwForeign.sigma

    def matchVol(
        tauPrev: Double,
        tau: Double,
        mktTotalVar: Double,
        fxVolKnown: PiecewiseConstant
    ): RootSolver.QuadSol =

      val fxVarKnown = fxBlackVariance(
        0.0,
        tauPrev,
        tau,
        hwDomestic.lambda -> hwDomestic.sigma,
        hwForeign.lambda -> hwForeign.sigma,
        fxVolKnown,
        corrDomesticFX,
        corrForeignFX,
        corrDomesticForeign
      )

      val a = tau - tauPrev
      val b =
        val crossD = (sigmaD * corrDomesticFX)
          .integrate(tauPrev, tau)(nInt(lambdaD, tau, _, _))
        val crossC = (sigmaC * corrForeignFX)
          .integrate(tauPrev, tau)(nInt(lambdaC, tau, _, _))
        2 * (crossD - crossC)
      val c =
        val bondD = (sigmaD * sigmaD)
          .integrate(tauPrev, tau)(nTimesN(lambdaD, lambdaD, tau, _, _))
        val bondC = (sigmaC * sigmaC)
          .integrate(tauPrev, tau)(nTimesN(lambdaC, lambdaC, tau, _, _))
        val bondDC = (sigmaC * sigmaD * corrDomesticForeign)
          .integrate(tauPrev, tau)(nTimesN(lambdaC, lambdaD, tau, _, _))
        bondD + bondC - 2 * bondDC + fxVarKnown - mktTotalVar
      RootSolver.quadratic(c, b, a, 1e-12)

    Either.raiseWhen(quotes.isEmpty)(lib.Error("need at least one calibration instrument"))
      .flatMap: _ =>
        val sorted = quotes.sortBy(_.fixingYf(t0).value)
        val expiries = sorted.map(fx => fx.fixingYf(t0).value)
        Either.raiseUnless(expiries.forall(_ > 0.0))(
          lib.Error("calibration instruments should expire in the future")
        )
          .map: _ =>
            sorted.zipWithIndex.foldLeft(Vector.empty[Double]):
              case (sigFn, (fxv @ FXVanilla(_, blackVol), i)) =>
                val marketTotalVar = blackVol * blackVol * fxv.fixingYf(t0).value
                val tauPrev = if i == 0 then 0.0 else expiries(i - 1)
                val tau = expiries(i)
                val fxVolKnown =
                  if i == 0 then PiecewiseConstant.flat(0.0)
                  else PiecewiseConstant(expiries.take(i), sigFn)
                val sig = matchVol(tauPrev, tau, marketTotalVar, fxVolKnown) match
                  case RootSolver.QuadSol.OneRoot(sig) => sig
                  case RootSolver.QuadSol.TwoRoots(sig0, sig1) =>
                    if sig0 > 0.0 && sig1 > 0.0 then min(sig0, sig1)
                    else if sig0 < 0.0 && sig1 < 0.0 then sigFn(i - 1) // TODO clamp to 0 instead?
                    else max(sig0, sig1)
                  case RootSolver.QuadSol.NoRealRoots() => sigFn(i - 1) // TODO clamp to 0 instead?
                sigFn :+ sig
          .map(fxVols => PiecewiseConstant(expiries, fxVols))
