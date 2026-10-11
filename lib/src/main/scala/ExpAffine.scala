package lib

case class ExpAffine(a: Double, b: Double) extends (Double => Double):

  def apply(x: Double): Double = a * math.exp(b * x)

  def mul(other: ExpAffine): ExpAffine = ExpAffine(a * other.a, b + other.b)

  def div(other: ExpAffine): ExpAffine = ExpAffine(a / other.a, b - other.b)
