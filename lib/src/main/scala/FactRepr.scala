package lib

trait FactRepr[T, I](val measure: Measure[T]):

  // P(t,Ts,Te)
  def forwardZeroCoupon(
      t: T,
      tStart: T,
      tEnd: T,
      curve: YieldCurve[T]
  ): I => Double

  // L(t,Ts,Te)
  def forwardLibor(
      t: T,
      tStart: T,
      tEnd: T,
      rate: Libor[T]
  ): I => Double

  // R(t,Ts,Te)
  def forwardCompoundedRate(
      t: T,
      tStart: T,
      tEnd: T,
      rate: CompoundedRate[T]
  ): I => Double

  // S(t,Ts,Te)
  def forwardSwapRate(
      t: T,
      tStart: T,
      tEnd: T,
      rate: SwapLike[T]
  ): I => Double
