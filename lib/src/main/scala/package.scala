package object lib:

  // ∫_lo^hi e^{a u} du
  private def expInt(a: Double, lo: Double, hi: Double): Double =
    if a == 0.0 then hi - lo
    else (math.exp(a * hi) - math.exp(a * lo)) / a

  // ∫_lo^hi u e^{a u} du
  private def uExpInt(a: Double, lo: Double, hi: Double): Double =
    if a == 0.0 then 0.5 * (hi * hi - lo * lo)
    else
      def prim(u: Double) = math.exp(a * u) * (a * u - 1.0) / (a * a)
      prim(hi) - prim(lo)

  // ∫_lo^hi e^{λc u} n_{λy}(tStar - u) du,  with n_λ(x) = (1 - e^{-λ x}) / λ  (= x if λ == 0)
  private def expTimesN(lc: Double, ly: Double, tStar: Double, lo: Double, hi: Double): Double =
    if ly == 0.0 then tStar * expInt(lc, lo, hi) - uExpInt(lc, lo, hi)
    else (expInt(lc, lo, hi) - math.exp(-ly * tStar) * expInt(lc + ly, lo, hi)) / ly

  // ∫_lo^hi n_{ly}(tStar - u) du  (the lc = 0 case of expTimesN)
  private def nInt(ly: Double, tStar: Double, lo: Double, hi: Double): Double =
    expTimesN(0.0, ly, tStar, lo, hi)

  // ∫_lo^hi n_{la}(tStar - u) n_{lb}(tStar - u) du
  // Using n_lb(x) = (1 - e^{-lb x}) / lb:
  //   ∫ n_la n_lb = (1/lb) [ ∫ n_la(tStar - u) du - e^{-lb tStar} ∫ e^{lb u} n_la(tStar - u) du ]
  private def nTimesN(la: Double, lb: Double, tStar: Double, lo: Double, hi: Double): Double =
    if la == 0.0 && lb == 0.0 then
      // ∫ (tStar - u)^2 du
      def prim(u: Double) = -math.pow(tStar - u, 3) / 3.0
      prim(hi) - prim(lo)
    else if lb != 0.0 then
      (nInt(la, tStar, lo, hi) - math.exp(-lb * tStar) * expTimesN(lb, la, tStar, lo, hi)) / lb
    else
      (nInt(lb, tStar, lo, hi) - math.exp(-la * tStar) * expTimesN(la, lb, tStar, lo, hi)) / la
