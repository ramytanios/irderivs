package lib

import lib.quantities.YearFraction
import lib.syntax.*

import scala.math.*

class HW1[T: DateLike](
    val t0: T,
    val lambda: Double,
    val sigma: PiecewiseConstant
):

  def price(caplet: Caplet[T]): Either[lib.Error, Double] =
    val measure = Measure(caplet.paymentAt, caplet.paymentCurrency)
    val factRepr = new HW1FactRepr[T](t0, lambda, sigma, measure)
    HW1.priceCapletFromVar(caplet, factRepr.variance(caplet.fixingAt), factRepr.factReprVar)

  def price(swaption: Swaption[T]): Either[lib.Error, Double] =
    val measure = Measure(swaption.fixingAt, swaption.rate.currency)
    val factRepr = new HW1FactRepr[T](t0, lambda, sigma, measure)
    HW1.priceSwaptionFromVar(swaption, factRepr.variance(swaption.fixingAt), factRepr.factReprVar)

  def price(payoff: Caplet[T] | Swaption[T]): Either[lib.Error, Double] =
    payoff match
      case c: Caplet[T]   => price(c)
      case s: Swaption[T] => price(s)

object HW1:

  private[lib] def priceCapletFromVar[T: DateLike](
      caplet: Caplet[T],
      vAtExpiry: Double,
      factRepr: HW1VarFactRepr[T]
  ): Either[lib.Error, Double] =
    val rate = caplet.rate
    val dcf = DateLike[T].yearFraction(caplet.startAt, caplet.endAt)(using rate.dayCounter)
    val discTp = caplet.discountCurve.discount(caplet.paymentAt)
    val (totalVar, forward) = factRepr.LNF(rate, caplet.fixingAt, vAtExpiry)
    val strike = 1 + dcf * caplet.strike
    Either.catchNonFatal(
      black.price(caplet.optionType, forward, strike, totalVar, discTp)
    ).leftMap(th => lib.Error(th.getMessage))

  // E^T{ a * e^{b * x_T} * I{sign * (x_T - xStar) >= 0} } where x_T ~ N(0, V_T)
  private inline def E(p: ExpAffine, xStar: Double, vT: Double, sign: Int): Double =
    val a = p.a
    val b = p.b
    if vT == 0.0 then if sign * (-xStar) >= 0 then a else 0.0
    else
      val sT = sqrt(vT)
      a * exp(0.5 * vT * b * b) * normal.cdf(sign * (b * sT - xStar / sT))

  private[lib] def priceSwaptionFromVar[T: DateLike](
      swaption: Swaption[T],
      vAtExpiry: Double,
      factRepr: HW1VarFactRepr[T]
  ): Either[lib.Error, Double] =
    val rate = swaption.rate
    val tExpiry = swaption.fixingAt
    val sign = swaption.optionType.sign
    val exSign = -sign
    val (sipStart, sipEnd) = rate.interestPeriod(tExpiry)
    val fn = factRepr.forwardSwapRate(tExpiry, sipStart, sipEnd, rate, vAtExpiry)
    RootSolver.brent(x => fn(x) - swaption.strike, -10, 10).map: xStar =>
      val discFixing = swaption.discountCurve.discount(tExpiry)
      val valueFixed = rate.fixedSchedule(sipStart, sipEnd).foldLeft(0.0):
        case (acc, FixedCoupon[T](tStart, tEnd, tPay)) =>
          val dcfStartEnd = DateLike[T].yearFraction(tStart, tEnd)(using rate.fixedDayCounter)
          val p = factRepr.forwardZeroCoupon(tExpiry, tPay, rate.discountCurve, vAtExpiry)
          acc + dcfStartEnd * E(p, xStar, vAtExpiry, exSign)
      val isDiscSameReset = rate.discountCurve eq rate.floatingRate.resetCurve
      val valueFloating = if isDiscSameReset then
        val curve = rate.discountCurve
        val pStart = factRepr.forwardZeroCoupon(tExpiry, sipStart, curve, vAtExpiry)
        val pEnd = factRepr.forwardZeroCoupon(tExpiry, sipEnd, curve, vAtExpiry)
        E(pStart, xStar, vAtExpiry, exSign) - E(pEnd, xStar, vAtExpiry, exSign)
      else
        val ip = rate match
          case sr: SwapRate[T] =>
            (fc: FloatingCoupon[T]) => rate.floatingRate.interestPeriod(fc.fixingAt)
          case csr: CompoundedSwapRate[T] => (fc: FloatingCoupon[T]) => fc.from -> fc.to
        rate.floatingSchedule(sipStart, sipEnd).foldLeft(0.0):
          case (acc, fc @ FloatingCoupon[T](tFixing, tStart, tEnd, tPay)) =>
            val floatingDayCounter = rate.floatingRate.dayCounter
            val dcfStartEnd = DateLike[T].yearFraction(tStart, tEnd)(using floatingDayCounter)
            val (ipStart, ipEnd) = ip(fc)
            val dcfIp = DateLike[T].yearFraction(ipStart, ipEnd)(using floatingDayCounter)
            val pDiscTPay =
              factRepr.forwardZeroCoupon(tExpiry, tPay, rate.discountCurve, vAtExpiry)
            val pResTStart =
              factRepr.forwardZeroCoupon(tExpiry, ipStart, rate.floatingRate.resetCurve, vAtExpiry)
            val pResTEnd =
              factRepr.forwardZeroCoupon(tExpiry, ipEnd, rate.floatingRate.resetCurve, vAtExpiry)
            acc +
              dcfStartEnd / dcfIp * (
                E(pDiscTPay.mul(pResTStart).div(pResTEnd), xStar, vAtExpiry, exSign) -
                  E(pDiscTPay, xStar, vAtExpiry, exSign)
              )
      sign * discFixing * (valueFloating - swaption.strike * valueFixed)

  opaque type CalibrationInstrument[T] = Caplet[T] | Swaption[T]

  object CalibrationInstrument:
    given DayCounter = DayCounter.Act365
    inline def apply[T](v: Caplet[T] | Swaption[T]): CalibrationInstrument[T] = v
    extension [T](calib: CalibrationInstrument[T])
      def yfExpiry(t0: T)(using DateLike[T]): YearFraction = calib match
        case c: Caplet[T]   => t0.yearFractionTo(c.fixingAt)
        case s: Swaption[T] => t0.yearFractionTo(s.fixingAt)
      def measure: Measure[T] = calib match
        case c: Caplet[T]   => Measure(c.paymentAt, c.paymentCurrency)
        case s: Swaption[T] => Measure(s.fixingAt, s.rate.currency)

  import CalibrationInstrument.*

  def calibrate[T: DateLike](
      t0: T,
      lambda: Double,
      quotes: Vector[(CalibrationInstrument[T], Double)]
  ): Either[lib.Error, HW1[T]] =
    Either.raiseWhen(quotes.isEmpty)(lib.Error("need at least one calibration instrument"))
      .flatMap: _ =>
        val sorted = quotes.sortBy((ins, _) => ins.yfExpiry(t0).value)
        val expiries = sorted.map((ins, _) => ins.yfExpiry(t0)).map(_.value)
        Either.raiseUnless(expiries.forall(_ > 0.0))(
          lib.Error("calibration instruments should expire in the future")
        )
          .flatMap: _ =>
            sorted.zipWithIndex.foldLeftM((0.0, Vector.empty[Double])):
              case ((accVar, sigFn), ((ins, mktPrice), i)) =>
                val factRepr = new HW1VarFactRepr(t0, lambda, ins.measure)
                val tauPrev = if i == 0 then 0 else expiries(i - 1)
                val factorKernelIdx = factRepr.factorKernel(tauPrev, expiries(i))
                val fn = ins match
                  case c: Caplet[T] => (sig: Double) =>
                      priceCapletFromVar(c, accVar + sig * sig * factorKernelIdx, factRepr)
                        .valueOr(throw _) - mktPrice
                  case s: Swaption[T] => (sig: Double) =>
                      priceSwaptionFromVar(s, accVar + sig * sig * factorKernelIdx, factRepr)
                        .valueOr(throw _) - mktPrice
                val (sigLo, sigHi) = (0.0, 1.0)
                RootSolver.brent(fn, sigLo, sigHi, RootSolver.Settings(100, 1e-14)) match
                  case Left(_: RootSolver.NotBracketed) =>
                    val sig = sigFn(i - 1)
                    Right(
                      (accVar + sig * sig * factorKernelIdx) -> (sigFn :+ sig)
                    )
                  case Right(sig) => Right(
                      (accVar + sig * sig * factorKernelIdx) -> (sigFn :+ sig)
                    )
                  case Left(err) => Left[lib.Error, (Double, Vector[Double])](err)
            .map((_, sigma) => new HW1(t0, lambda, PiecewiseConstant(expiries, sigma)))
