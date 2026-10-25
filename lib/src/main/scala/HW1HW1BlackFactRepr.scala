package lib

import lib.dtos.Currency

class HW1HW1BlackFactRepr[T: DateLike](
    val t0: T,
    val ccyDomestic: Currency,
    val ccyForeign: Currency,
    val hwDomestic: (Double, PiecewiseConstant),
    val hwForeign: (Double, PiecewiseConstant),
    val blackFXVol: PiecewiseConstant,
    val corrDomesticFX: PiecewiseConstant,
    val corrForeignFX: PiecewiseConstant,
    val corrDomesticForeign: PiecewiseConstant,
    measure: Measure[T]
) extends FactRepr[T, Double](measure):

  require(
    ccyDomestic == measure.currency,
    s"measure currency must be the domestic one, got measure currency ${measure.currency} and domestic $ccyDomestic"
  )

  val factReprFor =
    new HW1FactRepr[T](t0, hwForeign(0), hwForeign(1), Measure(measure.tStar, ccyForeign))

  def varianceForeign(t: T): Double = factReprFor.variance(t)

  // ∫_{t0}^{to} [0, σc(u) e^{λc u}, 0] · Corr · [Γ^d(u), -Γ^c(u), σFX(u)]^T du
  // where Γ^y(u) = σy(u) n_y(tStar - u) and Corr is ordered d -> c -> FX.
  def quantoAdj(to: T): Double =
    val (lambdaD, sigmaD) = hwDomestic
    val (lambdaC, sigmaC) = hwForeign
    val tStar = factReprFor.tau(measure.tStar)
    val tauTo = factReprFor.tau(to)

    // ρ_dc · Γ^d(u) : σc·ρdc·σd weighting the kernel e^{λc u} n_d(tStar - u)
    val termD = (sigmaC * corrDomesticForeign * sigmaD)
      .integrate(0.0, tauTo)(expTimesN(lambdaC, lambdaD, tStar, _, _))

    // -Γ^c(u) : σc·σc weighting the kernel e^{λc u} n_c(tStar - u)
    val termC = (sigmaC * sigmaC)
      .integrate(0.0, tauTo)(expTimesN(lambdaC, lambdaC, tStar, _, _))

    // ρ_cFX · σFX(u) : σc·ρcFX·σFX weighting the kernel e^{λc u}
    val termFX = (sigmaC * corrForeignFX * blackFXVol)
      .integrate(0.0, tauTo)(expInt(lambdaC, _, _))

    termD - termC + termFX

  // log-normal forward E_t^{d, T_p} { 1 + dcf(Ts, Te) * L^c(Tf, Ts, Te) }
  inline def logNormalForward(rate: Libor[T], tFixing: T): Double =
    val (tStart, tEnd) = rate.interestPeriod(tFixing)
    val dcf = DateLike[T].yearFraction(tStart, tEnd)(using rate.dayCounter)
    val natForward = 1 + dcf * rate.forward(tFixing)
    val (lambdaD, sigmaD) = hwDomestic
    val (lambdaC, sigmaC) = hwForeign
    val tauFixing = factReprFor.tau(tFixing) // TODO should be t?
    val tauStart = factReprFor.tau(tStart)
    val tauEnd = factReprFor.tau(tEnd)
    val term = factReprFor.n(tauEnd - tauStart) * math.exp(-lambdaC * tauFixing)
    val termD = (sigmaC * corrDomesticForeign * sigmaD)
      .integrate(0.0, tauFixing)(expTimesN(lambdaC, lambdaD, factReprFor.tau(measure.tStar), _, _))
    val termC = (sigmaC * sigmaC)
      .integrate(0.0, tauFixing)(expTimesN(lambdaC, lambdaC, factReprFor.tau(tEnd), _, _))
    val termFX = (sigmaC * corrForeignFX * blackFXVol)
      .integrate(0.0, tauFixing)(expInt(lambdaC, _, _))
    natForward * math.exp(-1 * term * (termD - termC + termFX))

  override def forwardZeroCoupon(
      t: T,
      tStart: T,
      tEnd: T,
      curve: YieldCurve[T]
  ): Double => Double =
    val fn = factReprFor.forwardZeroCoupon(t, tStart, tEnd, curve)
    (x: Double) => fn(x - quantoAdj(t))

  override def forwardLibor(
      t: T,
      tStart: T,
      tEnd: T,
      rate: Libor[T]
  ): Double => Double =
    require(
      rate.currency == ccyForeign,
      s"rate currency ${rate.currency} must be the foreign currency $ccyForeign"
    )
    val fn = factReprFor.forwardLibor(t, tStart, tEnd, rate)
    (x: Double) => fn(x - quantoAdj(t))

  override def forwardCompoundedRate(
      t: T,
      tStart: T,
      tEnd: T,
      rate: CompoundedRate[T]
  ): Double => Double =
    require(
      rate.compoundingRate.currency == ccyForeign,
      s"rate currency ${rate.compoundingRate.currency} must be the foreign currency $ccyForeign"
    )
    val fn = factReprFor.forwardCompoundedRate(t, tStart, tEnd, rate)
    (x: Double) => fn(x - quantoAdj(t))

  override def forwardSwapRate(
      t: T,
      tStart: T,
      tEnd: T,
      rate: SwapLike[T]
  ): Double => Double =
    require(
      rate.currency == ccyForeign,
      s"rate currency ${rate.currency} must be the foreign currnecy $ccyForeign"
    )
    val fn = factReprFor.forwardSwapRate(t, tStart, tEnd, rate)
    (x: Double) => fn(x - quantoAdj(t))
