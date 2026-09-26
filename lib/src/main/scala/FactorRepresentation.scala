package lib

trait FactorRepresentation[T]:

  // P(t,T,T+θ)
  def forwardZeroCoupon(
      t: T,
      zcStart: T,
      zcEnd: T,
      curve: YieldCurve[T],
      measure: Measure[T]
  ): Double => Double

  // L(t,T,T+θ)
  def forwardLibor(
      t: T,
      zcStart: T,
      zcEnd: T,
      libor: Libor[T],
      measure: Measure[T]
  ): Double => Double

  // S(t,T,T+θ)
  def forwardSwapRate(
      t: T,
      zcStart: T,
      zcEnd: T,
      swapRate: SwapLike[T],
      measure: Measure[T]
  ): Double => Double
