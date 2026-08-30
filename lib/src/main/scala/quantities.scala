package lib

import java.time.Period

object quantities:

  // Year fraction
  opaque type YearFraction = Double

  object YearFraction:
    inline def apply(yf: Double): YearFraction = yf

    val zero: YearFraction = 0.0
    val oneDay: YearFraction = 1.0 / 365.0
    val oneYear: YearFraction = 1.0

    extension (yf: YearFraction)
      inline def value: Double = yf
      inline def +(other: YearFraction) = yf + other
      inline def -(other: YearFraction) = yf - other
      inline def *(other: Double) = yf * other
      inline def /(other: YearFraction) = yf / other

    given Conversion[Double, YearFraction] = YearFraction.apply

  // Annualized interest rate
  opaque type Rate = Double

  object Rate:
    inline def apply(r: Double): Rate = r

    extension (r: Rate)
      inline def value: Double = r
      inline def *(yf: YearFraction): Double = r * yf
      inline def unary_- : Rate = -r

    given Conversion[Double, Rate] = Rate.apply

  // Strike
  opaque type Strike = Double

  object Strike:
    inline def apply(r: Double): Rate = r

    extension (r: Strike) inline def value: Double = r

    given Conversion[Double, Strike] = Strike.apply

  // Tenor
  case class Tenor(length: Int, unit: Tenor.Unit):

    def days: Int = unit match
      case Tenor.Unit.Day   => length
      case Tenor.Unit.Week  => length * 7
      case Tenor.Unit.Month => length * 30
      case Tenor.Unit.Year  => length * 365

  object Tenor:

    enum Unit:
      case Day, Week, Month, Year

    val `1D`: Tenor = Tenor(1, Unit.Day)
    val `3M`: Tenor = Tenor(3, Unit.Month)
    val `6M`: Tenor = Tenor(6, Unit.Month)
    val `1Y`: Tenor = Tenor(1, Unit.Year)
    val `2Y`: Tenor = Tenor(2, Unit.Year)
    val `10Y`: Tenor = Tenor(10, Unit.Year)

    def fromDays(n: Int): Tenor = Tenor(n, Unit.Day)
    def fromMonths(n: Int): Tenor = Tenor(n, Unit.Month)
    def fromYears(n: Int): Tenor = Tenor(n, Unit.Year)

    extension (t: Tenor)
      def toPeriod = t.unit match
        case Unit.Day   => Period.ofDays(t.length)
        case Unit.Week  => Period.ofWeeks(t.length)
        case Unit.Month => Period.ofMonths(t.length)
        case Unit.Year  => Period.ofYears(t.length)

      def unary_- : Tenor = t.copy(length = -1 * t.length)
      def *(factor: Int): Tenor = t.copy(length = factor * t.length)
      def toYf: YearFraction = t.unit match
        case Unit.Day   => t.length / 365.0
        case Unit.Week  => t.length * 7 / 365.0
        case Unit.Month => t.length * 1 / 12.0
        case Unit.Year  => t.length

    given Ordering[Tenor] = Ordering.by(_.toYf)

    given fromDto: Conversion[dtos.Tenor, Tenor] = t =>
      t.unit match
        case dtos.Tenor.Unit.Day   => Tenor(t.length, Unit.Day)
        case dtos.Tenor.Unit.Week  => Tenor(t.length, Unit.Week)
        case dtos.Tenor.Unit.Month => Tenor(t.length, Unit.Month)
        case dtos.Tenor.Unit.Year  => Tenor(t.length, Unit.Year)

    given toDto: Conversion[Tenor, dtos.Tenor] = t =>
      t.unit match
        case Unit.Day   => dtos.Tenor(t.length, dtos.Tenor.Unit.Day)
        case Unit.Week  => dtos.Tenor(t.length, dtos.Tenor.Unit.Week)
        case Unit.Month => dtos.Tenor(t.length, dtos.Tenor.Unit.Month)
        case Unit.Year  => dtos.Tenor(t.length, dtos.Tenor.Unit.Year)
