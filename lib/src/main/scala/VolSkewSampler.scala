package lib

import lib.syntax.*
import org.apache.commons.math3.distribution.NormalDistribution

object VolSkewSampler:

  case class Params(
      nSamplesMiddle: Int,
      nSamplesTail: Int,
      nStdvsTail: Int
  )

  case class Result(
      quotedStrikes: List[Double],
      quotedVols: List[Double],
      quotedPdf: List[Double],
      strikes: List[Double],
      vols: List[Double],
      pdf: List[Double],
      fwd: Double
  )

  def apply[T: DateLike](
      t: T,
      expiry: T,
      ksQuoted: List[Double],
      volSkew: VolatilitySkew,
      forward: Forward[T],
      params: VolSkewSampler.Params
  ) =

    val dt = t.yearFractionTo(expiry)(using lib.DateLike[T], DayCounter.Act365)
    val fwd = forward(expiry)
    val vsQuoted = ksQuoted.map(volSkew)
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
    val ksMiddle = (1 to params.nSamplesMiddle).map(i => cdfInvN(i / (params.nSamplesMiddle + 1.0)))
    val ksRight = ksMiddle.lastOption.flatMap: kmMax =>
      ksQuoted.lastOption.map: kqMax =>
        val kMax0 = Iterator.iterate(kmMax)(_ + atmStdv).find(_ >= kqMax).get
        val kMax = kMax0 + params.nStdvsTail * atmStdv
        uniform(kmMax, kMax, params.nSamplesTail).toList
    .orEmpty
    val ksLeft = ksMiddle.headOption.flatMap: kmMin =>
      ksQuoted.headOption.map: kqMin =>
        val kMin0 = Iterator.iterate(kmMin)(_ - atmStdv).find(_ <= kqMin).get
        val kMin = kMin0 - params.nStdvsTail * atmStdv
        uniform(kMin, kmMin, params.nSamplesTail).toList
    .orEmpty
    val ks = ksLeft ++ ksMiddle ++ ksRight
    val vs = ks.map(volSkew)
    val pdf = ks.map(impliedPdf)
    VolSkewSampler.Result(ksQuoted, vsQuoted, pdfQuoted, ks, vs, pdf, fwd)
