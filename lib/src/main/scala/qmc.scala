package lib

import org.apache.commons.math3.linear.CholeskyDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.SobolSequenceGenerator
import org.apache.commons.math3.random.Well19937c

import scala.math.*

object qmc:

  private val eps = 1e-16

  case class Estimate(mean: Double, stdErr: Double, replicates: Int, points: Int)

  private def cholesky(r: Array[Array[Double]]): Array[Array[Double]] =
    val m = MatrixUtils.createRealMatrix(r)
    CholeskyDecomposition(m).getL.getData

  // out ~ U(0, I)
  final class ShiftedSobol(val dim: Int, rng: RandomGenerator):
    private val sobol = SobolSequenceGenerator(dim)
    private val shift = Array.fill(dim)(rng.nextDouble)

    def next(out: Array[Double]): Unit =
      val x = sobol.nextVector
      var i = 0
      while i < dim do
        val v = (x(i) + shift(i)) % 1
        out(i) = min(max(v, eps), 1 - eps)
        i += 1

  // out ~ N(0, R)
  final class QmcNormals(ss: ShiftedSobol, a: Array[Array[Double]]):
    require(ss.dim == a.length, "dimensions mismatch")
    private val dim = ss.dim
    private val z = new Array[Double](dim)

    def next(out: Array[Double]): Unit =
      ss.next(z)
      var j = 0
      while j < dim do
        z(j) = normal.inverseCdf(z(j))
        j += 1

      var i = 0
      while i < dim do
        var s = 0.0
        j = 0
        val row = a(i)
        while j < dim do
          s += row(j) * z(j)
          j += 1
        out(i) = s
        i += 1

  def integrate(
      fn: Array[Double] => Double,
      r: Array[Array[Double]],
      log2N: Int,
      replicates: Int,
      seed: Long = 42L
  ): Estimate =
    val dim = r.length
    val n = 1 << log2N
    val a = cholesky(r)
    val rng = Well19937c(seed)
    val x = new Array[Double](dim)
    val means = Array.fill(replicates):
      val gen = QmcNormals(ShiftedSobol(dim, rng), a)
      var s = 0.0
      var i = 0
      while i < n do
        gen.next(x)
        s += fn(x)
        i += 1
      s / n

    val mean = means.sum / means.size
    val variance = means.foldLeft(0.0)((acc, m) => acc + (m - mean) * (m - mean)) / (replicates - 1)
    Estimate(mean, sqrt(variance / replicates), replicates, n)
