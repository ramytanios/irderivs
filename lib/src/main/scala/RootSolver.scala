package lib

import org.apache.commons.math3.analysis.solvers.BrentSolver
import org.apache.commons.math3.analysis.UnivariateFunction
import cats.syntax.all.*

object RootSolver:

  case class Settings(maxIters: Int = 30, absAccuracy: Double = 1e-6)

  case class NotBracketed(min: Double, fMin: Double, max: Double, fMax: Double) extends lib.Error(
        s"root not bracketed on [$min, $max], f(min)=$fMin, f(max)=$fMax"
      )

  def brent(
      f: Double => Double,
      min: Double,
      max: Double,
      settings: Settings = Settings()
  ): Either[lib.Error, Double] =
    val fMin = f(min)
    val fMax = f(max)
    if fMin * fMax >= 0 then NotBracketed(min, max, fMin, fMax).asLeft[Double]
    else
      val solver = new BrentSolver(settings.absAccuracy)
      val uf = new UnivariateFunction:
        def value(x: Double): Double = f(x)
      Either.catchNonFatal(solver.solve(settings.maxIters, uf, min, max))
        .leftMap(t => lib.Error(t.getMessage))
        .flatTap(root => Either.raiseWhen(root.isNaN)(lib.Error("root solver returned NaN")))
