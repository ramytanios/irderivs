package lib

import scala.math.*

object bachelier:

  import normal.*

  def price(
      optionType: dtos.OptionType,
      forward: Double,
      strike: Double,
      timeToExpiry: Double,
      vol: Double,
      discountFactor: Double
  ): Double =

    require(timeToExpiry >= 0.0 && vol >= 0.0, "tte and vol must be positive")

    val call =
      if timeToExpiry == 0 || vol == 0.0 then
        discountFactor * max(forward - strike, 0.0)
      else
        val stdv = vol * sqrt(timeToExpiry)
        val d = (forward - strike) / stdv
        discountFactor * ((forward - strike) * cdf(d) + stdv * pdf(d))

    optionType match
      case dtos.OptionType.Call => call
      case dtos.OptionType.Put  => call - discountFactor * (forward - strike)

  def impliedCumulative(
      forward: Double,
      dt: Double,
      vol: Double => Double,
      fstDerivVol: Double => Double
  ): Double => Double =
    val sdt = sqrt(dt)
    k =>
      val stdv = vol(k) * sdt
      val d = (forward - k) / stdv
      1 - cdf(d) + sdt * fstDerivVol(k) * pdf(d)

  def impliedDensity(
      forward: Double,
      dt: Double,
      vol: Double => Double,
      fstDerivVol: Double => Double,
      sndDerivVol: Double => Double
  ): Double => Double =
    k =>
      val stdv = vol(k) * sqrt(dt)
      val d = (forward - k) / stdv
      pdf(d) / stdv * (
        dt * vol(k) * sndDerivVol(k) + pow((1 + (forward - k) * fstDerivVol(k) / vol(k)), 2)
      )
