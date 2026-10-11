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

  // P(Ts,Te)
  def zeroCoupon(
      tStart: T,
      tEnd: T,
      curve: YieldCurve[T]
  ): I => Double =
    forwardZeroCoupon(tStart, tStart, tEnd, curve)

  // L(Ts,Te)
  def libor(
      tStart: T,
      tEnd: T,
      rate: Libor[T]
  ): I => Double =
    forwardLibor(tStart, tStart, tEnd, rate)

  // S(Ts,Te)
  def swapRate(
      tStart: T,
      tEnd: T,
      rate: SwapLike[T]
  ): I => Double =
    forwardSwapRate(tStart, tStart, tEnd, rate)
