package lib

import scala.math.*

object black:

  import normal.*

  def price(
      optionType: dtos.OptionType,
      forward: Double,
      strike: Double,
      variance: Double,
      discountFactor: Double
  ): Double =

    require(variance >= 0.0, "variance must be positive")

    val call =
      if variance == 0.0 then discountFactor * max(forward - strike, 0.0)
      else
        val stdv = sqrt(variance)
        val d1 = log(forward / strike) / stdv + 0.5 * stdv
        val d2 = d1 - stdv
        discountFactor * (forward * cdf(d1) - strike * cdf(d2))

    optionType match
      case dtos.OptionType.Call => call
      case dtos.OptionType.Put  => call - discountFactor * (forward - strike)
