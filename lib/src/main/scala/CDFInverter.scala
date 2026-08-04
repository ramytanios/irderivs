package lib

import lib.syntax.*
import org.apache.commons.math3.distribution.NormalDistribution

enum Arbitrage:
  case LeftAsymptotic
  case RightAsymptotic
  case Density(leftStrike: Double, rightStrike: Double)

case class Params(
    nMiddle: Int = 500, // number of interior strikes
    nTail: Int = 50, // number strikes per tail
    nTailMax: Int = 15, // tail strikes max iterations
    cdfThreshold: Double = 0.001, // tail cdf theshold
    relPriceThreshold: Double = 0.05 // call put relative price threshold
)

object CDFInverter:

  def apply[T: DateLike](
      t: T,
      expiry: T,
      vol: VolatilitySkew,
      forward: Forward[T],
      params: Params = Params()
  ): Either[Arbitrage, Double => Double] =

    val dt = t.yearFractionTo(expiry)(using DateLike[T], DayCounter.Act365).value

    val fwd = forward(expiry)

    val atmStdv = vol(fwd) * math.sqrt(dt)

    // φ⁻¹ of N(F,σ²T)
    val cdfInvN = NormalDistribution(fwd, atmStdv).inverseCumulativeProbability

    // cdf implied from vol
    val cdfImplied = bachelier.impliedCumulative(fwd, dt, vol.apply, vol.fstDerivative)

    def middleStrikes =
      val dk = 1.0 / (params.nMiddle + 1)
      val strikes = (1 to params.nMiddle).map(i => cdfInvN(i * dk))
      val cdfs = strikes.map(cdfImplied)
      (strikes -> cdfs).asRight[Arbitrage]

    def uniform(kMin: Double, kMax: Double): IndexedSeq[Double] =
      val step = (kMax - kMin) / params.nTail
      if step == 0.0 then IndexedSeq.empty[Double] else (0 to params.nTail).map(i => kMin + i * step)

    def leftStrikes(kL: Double) =
      (if cdfImplied(kL) <= params.cdfThreshold then LazyList.empty[Double].asRight[Arbitrage]
       else
         val points = LazyList.range(1, params.nTailMax + 1)
           .map(kL - _ * atmStdv)
           .map(k => k -> cdfImplied(k))
         val cut = points.indexWhere((_, cdf) => cdf <= params.cdfThreshold)
         Either.raiseWhen(cut < 0)(Arbitrage.LeftAsymptotic).as(points.take(cut + 1).map(_(0)))
      ) .flatMap: ks =>
        val kMin = ks.lastOption.getOrElse(kL)
        val put = bachelier.price(dtos.OptionType.Put, fwd, kMin, dt, vol(kMin), 1.0)
        val putAtm = bachelier.price(dtos.OptionType.Put, fwd, fwd, dt, vol(fwd), 1.0)
        Either.raiseUnless(put <= params.relPriceThreshold * putAtm)(Arbitrage.LeftAsymptotic)
          .as:
            val strikes = uniform(kMin, kL).dropRight(1)
            val cdfs = strikes.map(cdfImplied)
            strikes -> cdfs

    def rightStrikes(kR: Double) =
      (if cdfImplied(kR) >= (1 - params.cdfThreshold) then LazyList.empty[Double].asRight[Arbitrage]
       else
         val points = LazyList.range(1, params.nTailMax + 1)
           .map(kR + _ * atmStdv)
           .map(k => k -> cdfImplied(k))
         val cut = points.indexWhere((_, cdf) => cdf >= (1 - params.cdfThreshold))
         Either.raiseWhen(cut < 0)(Arbitrage.RightAsymptotic).as(points.take(cut + 1).map(_(0)))
      ) .flatMap: ks =>
        val kMax = ks.lastOption.getOrElse(kR)
        val call = bachelier.price(dtos.OptionType.Call, fwd, kMax, dt, vol(kMax), 1.0)
        val callAtm = bachelier.price(dtos.OptionType.Call, fwd, fwd, dt, vol(fwd), 1.0)
        Either.raiseUnless(call <= params.relPriceThreshold * callAtm)(Arbitrage.RightAsymptotic)
          .as:
            val strikes = uniform(kR, kMax).drop(1)
            val cdfs = strikes.map(cdfImplied)
            strikes -> cdfs

    for
      (mks, mvs) <- middleStrikes
      (lks, lvs) <- leftStrikes(mks.head)
      (rks, rvs) <- rightStrikes(mks.last)
      ks = lks ++ mks ++ rks
      vs = lvs ++ mvs ++ rvs
      _ <- vs.indices.init.find(i => vs(i) >= vs(i + 1)).toLeft(())
        .leftMap(i => Arbitrage.Density(ks(i), ks(i + 1)))
    yield LinearInterpolation.withLinearExtrapolation(vs, ks)
