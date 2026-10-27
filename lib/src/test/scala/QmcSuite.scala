package lib

class QmcSuite extends munit.FunSuite:

  private val r = Array(
    Array(1.0, 0.7, 0.3),
    Array(0.7, 1.0, 0.5),
    Array(0.3, 0.5, 1.0)
  )

  test(s"E[x0 x1] = rho"):
    val e = qmc.integrate(x => x(0) * x(1), r, log2N = 14, replicates = 16)
    assertEqualsDouble(e.mean, 0.7, math.max(5 * e.stdErr, 1e-3))

  test("E[max(x0, 0)] = 1/sqrt(2pi)"):
    val e = qmc.integrate(x => math.max(x(0), 0.0), r, log2N = 14, replicates = 16)
    assertEqualsDouble(e.mean, 1.0 / math.sqrt(2 * math.Pi), math.max(5 * e.stdErr, 1e-3))
