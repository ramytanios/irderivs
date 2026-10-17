package lib

import cats.syntax.all.*
import lib.dtos.OptionType
import lib.literals.*
import lib.testsyntax.*
import lib.testutils.oneDimPseudoRandomMC

import java.time.LocalDate
import scala.math.*

class HW1Suite extends munit.FunSuite with lib.EitherSyntax:

  val t0 = d"2026-10-15"

  val sigma = PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(0.1))

  test("pricing caplets with zero vol should be intrinsic"):

    (rand.randCaplet(t0), rand.randHWLambda)
      .tupled
      .view
      .take(10)
      .foreach: (caplet, lambda) =>
        val zeroVol = PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(0.0))
        val hw1 = new HW1(t0, lambda, zeroVol)
        val tvHW = hw1.price(caplet)
        val dcf =
          DateLike[LocalDate].yearFraction(caplet.startAt, caplet.endAt)(using caplet.rate.dayCounter)
        val discPay = caplet.discountCurve.discount(caplet.paymentAt)
        val fixing = caplet.rate.forward(caplet.fixingAt)
        val tvIntrinsic = dcf * discPay * max(caplet.optionType.sign * (fixing - caplet.strike), 0.0)
        tvHW.failOrAssert(tvHW => assertEqualsDouble(tvHW, tvIntrinsic, 1e-10))

  test("analytical caplet prices should satisify put-call parity"):
    (rand.randCaplet(t0), rand.randHWLambda)
      .tupled
      .view
      .take(10)
      .foreach: (caplet, lambda) =>
        val hw1 = new HW1(t0, lambda, sigma)
        val cp = caplet.optionType match
          case OptionType.Call =>
            val c = hw1.price(caplet)
            val p = hw1.price(caplet.ofKind(OptionType.Put))
            (c, p).tupled
          case OptionType.Put =>
            val p = hw1.price(caplet)
            val c = hw1.price(caplet.ofKind(OptionType.Call))
            (c, p).tupled
        cp.failOrAssert: (c, p) =>
          val (tStart, tEnd, tPay) = (caplet.startAt, caplet.endAt, caplet.paymentAt)
          val discPay = caplet.discountCurve.discount(tPay)
          val dcf = DateLike[LocalDate].yearFraction(tStart, tEnd)(using caplet.rate.dayCounter)
          val factRepr = new HW1FactRepr[LocalDate](
            t0,
            lambda,
            sigma,
            Measure(caplet.paymentAt, caplet.paymentCurrency)
          )
          val fwd = hw1.LNF(
            caplet.rate,
            caplet.fixingAt,
            factRepr.variance(caplet.fixingAt),
            factRepr
          )(1)
          assertEqualsDouble(
            (c - p) - discPay * (fwd - (1 + dcf * caplet.strike)),
            0.0,
            1e-10
          )

  test("swaption analytical price should match MC price"):
    val n = 100_00
    (rand.randSwaption(t0), rand.randHWLambda)
      .tupled
      .view
      .take(10)
      .foreach: (swaption, lambda) =>
        val hw1 = new HW1(t0, lambda, sigma)
        val fixingAt = swaption.fixingAt
        val rate = swaption.rate
        val measure = Measure(swaption.fixingAt, rate.currency)
        val factRepr = new HW1FactRepr(t0, lambda, sigma, measure)
        val (ipStart, ipEnd) = swaption.rate.interestPeriod(fixingAt)
        val vT = factRepr.variance(fixingAt)
        val fn = factRepr.forwardSwapRate(fixingAt, ipStart, ipEnd, rate, vT)
        val discFixing = swaption.discountCurve.discount(fixingAt)
        val sign = swaption.optionType.sign
        val fixedFns = rate.fixedSchedule(ipStart, ipEnd).map:
          case FixedCoupon(from, to, paymentAt) =>
            val dcf =
              DateLike[LocalDate].yearFraction(from, to)(using rate.fixedDayCounter).value
            val zcFn = factRepr.forwardZeroCoupon(fixingAt, paymentAt, rate.discountCurve, vT)
            (x: Double) => dcf * zcFn(x)
        val (stdErr0, mean) = oneDimPseudoRandomMC(
          x =>
            max(sign * (fn(x) - swaption.strike), 0.0) *
              fixedFns.foldLeft(0.0)((acc, fn) => acc + fn(x)),
          n,
          vT
        )
        val stdErr = discFixing * stdErr0.value
        val tvMC = discFixing * mean.value
        hw1.price(swaption).failOrAssert: tvHW =>
          assertEqualsDouble(tvHW, tvMC, 3 * stdErr)

  test("caplet analytical price should match MC price"):
    val n = 100_000
    (rand.randCaplet(t0), rand.randHWLambda)
      .tupled
      .view
      .take(10)
      .foreach: (caplet, lambda) =>
        val hw1 = new HW1(t0, lambda, sigma)
        val measure = Measure(caplet.paymentAt, caplet.paymentCurrency)
        val factRepr = new HW1FactRepr(t0, lambda, sigma, measure)
        val (ipStart, ipEnd) = caplet.rate.interestPeriod(caplet.fixingAt)
        val vT = factRepr.variance(caplet.fixingAt)
        val fn = factRepr.forwardLibor(ipStart, ipEnd, caplet.rate, vT)
        val dcf = DateLike[LocalDate].yearFraction(caplet.startAt, caplet.endAt)(
          using caplet.rate.dayCounter
        )
        val discPay = caplet.discountCurve.discount(caplet.paymentAt)
        val sign = caplet.optionType.sign
        val (stdErr0, mean) =
          oneDimPseudoRandomMC(x => dcf * max(sign * (fn(x) - caplet.strike), 0.0), n, vT)
        val stdErr = discPay * stdErr0.value
        val tvMC = discPay * mean.value
        hw1.price(caplet).failOrAssert: tvHW =>
          assertEqualsDouble(tvHW, tvMC, 3 * stdErr)

  test("analytical swaption prices should satisify put-call parity"):
    // Zero payment delay: with a payment delay the model prices a payment
    // convexity adjustment that the convexity-free curve forward below ignores, so
    // parity only holds exactly against rate.forward when there is no delay.
    (rand.randSwaption(t0, useZeroPayDelay = true), rand.randHWLambda)
      .tupled
      .view
      .take(10)
      .foreach: (swaption, lambda) =>
        val hw1 = new HW1(t0, lambda, sigma)
        val cp = swaption.optionType match
          case OptionType.Call =>
            val c = hw1.price(swaption)
            val p = hw1.price(swaption.ofKind(OptionType.Put))
            (c, p).tupled
          case OptionType.Put =>
            val p = hw1.price(swaption)
            val c = hw1.price(swaption.ofKind(OptionType.Call))
            (c, p).tupled
        cp.failOrAssert: (c, p) =>
          val (sipStart, sipEnd) = swaption.rate.interestPeriod(swaption.fixingAt)
          val a = swaption.rate.fixedSchedule(sipStart, sipEnd).foldLeft(0.0):
            case (acc, FixedCoupon(tStart, tEnd, tPay)) =>
              val dcf = DateLike[LocalDate].yearFraction(tStart, tEnd)(
                using swaption.rate.fixedDayCounter
              )
              acc + dcf * swaption.rate.discountCurve.discount(tPay)
          val fwd = swaption.rate.forward(swaption.fixingAt)
          val adj = swaption.discountCurve.discount(swaption.fixingAt) /
            swaption.rate.discountCurve.discount(swaption.fixingAt)
          assertEqualsDouble(
            (c - p) - adj * a * (fwd - swaption.strike),
            0.0,
            1e-10
          )
