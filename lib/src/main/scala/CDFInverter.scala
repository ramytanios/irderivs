package lib

import lib.dtos.Moneyness
import lib.syntax.*
import org.apache.commons.math3.distribution.NormalDistribution

enum Arbitrage:
  case LeftAsymptotic
  case RightAsymptotic
  case Density(leftStrike: Double, rightStrike: Double)

object CDFInverter:

  case class Params(
      nMiddle: Int = 500, // number of interior strikes
      nTail: Int = 50, // number strikes per tail
      nTailMax: Int = 15, // tail strikes max iterations
      cdfThreshold: Double = 0.001, // tail cdf theshold
      relPriceThreshold: Double = 0.05 // call put relative price threshold
  )

  def apply[T: DateLike](
      t: T,
      expiry: T,
      msQuoted: List[Moneyness],
      vol: VolatilitySkew,
      forward: Forward[T],
      params: Params = Params()
  ): Either[Arbitrage, Double => Double] =

    val dt = t.yearFractionTo(expiry)(using DateLike[T], DayCounter.Act365).value

    val fwd = forward(expiry)

    val atmStdv = vol(fwd) * math.sqrt(dt)

    // φ⁻¹ of N(F,σ²T)
    val cdfInvN = NormalDistribution(fwd, atmStdv).inverseCumulativeProbability

    // cdf implied from vol skew
    val cdfImplied = bachelier.impliedCumulative(fwd, dt, vol.apply, vol.fstDerivative)

    val ksQuoted = msQuoted.map(_.value + fwd)

    def middleStrikes =
      val dk = 1.0 / (params.nMiddle + 1)
      val strikes = (1 to params.nMiddle).map(i => cdfInvN(i * dk))
      strikes.asRight[Arbitrage]

    def leftStrikes(kL: Double) =
      (if cdfImplied(kL) <= params.cdfThreshold then kL.asRight[Arbitrage]
       else
         val points = LazyList.range(1, params.nTailMax + 1)
           .map(kL - _ * atmStdv)
           .map(k => k -> cdfImplied(k))
         val cut = points.indexWhere((_, cdf) => cdf <= params.cdfThreshold)
         Either.raiseWhen(cut < 0)(Arbitrage.LeftAsymptotic).as(points(cut)(0))
      ) .flatMap: kMin0 =>
        val kMin = math.min(kMin0, ksQuoted.headOption.getOrElse(kMin0))
        val put = bachelier.price(dtos.OptionType.Put, fwd, kMin, dt, vol(kMin), 1.0)
        val putAtm = bachelier.price(dtos.OptionType.Put, fwd, fwd, dt, vol(fwd), 1.0)
        Either.raiseUnless(put <= params.relPriceThreshold * putAtm)(Arbitrage.LeftAsymptotic)
          .as(uniform(kMin, kL, params.nTail).dropRight(1))

    def rightStrikes(kR: Double) =
      (if cdfImplied(kR) >= (1 - params.cdfThreshold) then kR.asRight[Arbitrage]
       else
         val points = LazyList.range(1, params.nTailMax + 1)
           .map(kR + _ * atmStdv)
           .map(k => k -> cdfImplied(k))
         val cut = points.indexWhere((_, cdf) => cdf >= (1 - params.cdfThreshold))
         Either.raiseWhen(cut < 0)(Arbitrage.RightAsymptotic).as(points(cut)(0))
      ) .flatMap: kMax0 =>
        val kMax = math.max(kMax0, ksQuoted.lastOption.getOrElse(kR))
        val call = bachelier.price(dtos.OptionType.Call, fwd, kMax, dt, vol(kMax), 1.0)
        val callAtm = bachelier.price(dtos.OptionType.Call, fwd, fwd, dt, vol(fwd), 1.0)
        Either.raiseUnless(call <= params.relPriceThreshold * callAtm)(Arbitrage.RightAsymptotic)
          .as(uniform(kR, kMax, params.nTail).drop(1))

    for
      mks <- middleStrikes
      lks <- leftStrikes(mks.head)
      rks <- rightStrikes(mks.last)
      // add quoted strikes to make sure density check hits exactly the quotes
      ks = (lks ++ mks ++ rks ++ ksQuoted.toIndexedSeq).distinct.sorted
      vs = ks.map(cdfImplied)
      _ <- vs.indices.init.find(i => vs(i) >= vs(i + 1)).toLeft(())
        .leftMap(i => Arbitrage.Density(ks(i), ks(i + 1)))
    yield LinearInterpolation.withLinearExtrapolation(vs, ks)
