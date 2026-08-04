package lib.api

import lib.dtos

enum MarketError(msg: String) extends lib.Error(msg):

  case MissingRate(name: dtos.RateId)
      extends MarketError(s"missing rate $name")

  case MissingYieldCurve(ccy: dtos.Currency, name: dtos.CurveId)
      extends MarketError(s"missing curve $name in ccy $ccy")

  case MissingFixingOf(underlying: dtos.RateId)
      extends MarketError(s"missing fixings of $underlying")

  case MissingFixingAt[T](underlying: dtos.RateId, at: T)
      extends MarketError(s"missing fixing of $underlying at $at")

  case MissingVolatility(currency: dtos.Currency)
      extends MarketError(s"missing vol cube of currency $currency")

  case MissingVolatilitySurface(currency: dtos.Currency, tenor: dtos.Tenor)
      extends MarketError(s"missing vol surface in currency $currency and tenor $tenor")

  case MissingCalendar(name: dtos.CalendarId) extends MarketError(s"missing calendar $name")

trait Market[T]:

  def t: T

  def rate(name: dtos.RateId): Either[MarketError, dtos.Underlying]

  def yieldCurve(curve: dtos.Curve): Either[MarketError, dtos.YieldCurve[T]]

  def fixings(rate: dtos.RateId): Either[MarketError, Seq[dtos.Fixing[T]]]

  def volCube(currency: dtos.Currency): Either[MarketError, dtos.Volatility]

  def volSurface(
      currency: dtos.Currency,
      tenor: dtos.Tenor
  ): Either[MarketError, Map[dtos.Tenor, List[(dtos.Moneyness, Double)]]]

  def calendar(name: dtos.CalendarId): Either[MarketError, dtos.Calendar[T]]

object Market:

  def apply[T](
      tRef: T,
      marketByCcy: Map[dtos.Currency, dtos.CcyMarket[T]],
      static: dtos.Static[T]
  ): Market[T] =
    this.apply(
      tRef,
      rates = marketByCcy.values.map(_.rates).reduce(_ ++ _),
      curves = marketByCcy.flatMap((ccy, market) =>
        market.curves.map((name, curve) => dtos.Curve(ccy, name) -> curve)
      ).toMap,
      fixingsByRate = marketByCcy.values.map(_.fixings).reduce(_ ++ _),
      volatilities = marketByCcy.view.mapValues(_.volatility).toMap,
      calendars = static.calendars
    )

  def apply[T](
      tRef: T,
      rates: Map[dtos.RateId, dtos.Underlying],
      curves: Map[dtos.Curve, dtos.YieldCurve[T]],
      fixingsByRate: Map[dtos.RateId, Seq[dtos.Fixing[T]]],
      volatilities: Map[dtos.Currency, dtos.Volatility],
      calendars: Map[dtos.CalendarId, dtos.Calendar[T]]
  ): Market[T] = new Market[T]:

    import MarketError.*

    def t: T = tRef

    def rate(name: dtos.RateId): Either[MarketError, dtos.Underlying] =
      rates.get(name).toRight(MissingRate(name))

    def yieldCurve(curve: dtos.Curve): Either[MarketError, dtos.YieldCurve[T]] =
      curves.get(curve).toRight(MissingYieldCurve(curve.currency, curve.name))

    def fixings(rate: dtos.RateId): Either[MarketError, Seq[dtos.Fixing[T]]] =
      fixingsByRate.get(rate).toRight(MissingFixingOf(rate))

    def volCube(currency: dtos.Currency): Either[MarketError, dtos.Volatility] =
      volatilities.get(currency).toRight(MissingVolatility(currency))

    def volSurface(
        currency: dtos.Currency,
        tenor: dtos.Tenor
    ): Either[MarketError, Map[dtos.Tenor, List[(dtos.Moneyness, Double)]]] =
      volatilities.get(currency).flatMap:
        case dtos.Volatility.Cube(cube, _) => cube.get(tenor)
        case dtos.Volatility.Flat(_)       => None
      .toRight(MissingVolatilitySurface(currency, tenor))

    def calendar(name: dtos.CalendarId): Either[MarketError, dtos.Calendar[T]] =
      calendars.get(name).toRight(MissingCalendar(name))
