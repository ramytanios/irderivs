package lib

import cats.syntax.all.*
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.analysis.solvers.BrentSolver
import org.apache.commons.math3.exception.NoBracketingException
import scala.math.*

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
    val solver = new BrentSolver(settings.absAccuracy)
    val uf: UnivariateFunction = (x: Double) => f(x)
    Either.catchNonFatal(solver.solve(settings.maxIters, uf, min, max))
      .leftMap:
        case nb: NoBracketingException => NotBracketed(nb.getLo, nb.getFLo, nb.getHi, nb.getFHi)
        case t                         => lib.Error(t.getMessage)
      .flatTap(root => Either.raiseWhen(root.isNaN)(lib.Error("root solver returned NaN")))

  enum QuadSol:
    case OneRoot(root: Double)
    case TwoRoots(root0: Double, root1: Double)
    case NoRealRoots()

  // solves c + b·x + a·x·x = 0
  def quadratic(c: Double, b: Double, a: Double, tol: Double = 1e-12) =
    val disc = b * b - 4 * a * c
    if disc < -tol then QuadSol.NoRealRoots()
    else if abs(disc) <= tol then QuadSol.OneRoot(-b / 2.0 / a)
    else
      val s = sqrt(disc)
      val q = -0.5 * (b + signum(b) * s)
      QuadSol.TwoRoots(q / a, c / q)
