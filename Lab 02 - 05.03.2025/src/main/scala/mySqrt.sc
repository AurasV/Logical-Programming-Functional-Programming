import scala.annotation.tailrec

def mySqrt(a: Double): Double = {
  def improve(xn: Double, a: Double): Double =
  {
    0.5 * (xn + a / xn)
  }
  def acceptable(xn: Double, a: Double): Boolean =
  {
    (xn * xn - a).abs <= 0.001
    // (xn * xn / a).abs <= 1.0001 // <-- for big numbers like 2.0e50
  }

  @tailrec
  def tailSqrt(estimate: Double): Double =
  {
    if(acceptable(estimate, a)) estimate
    else tailSqrt(improve(estimate, a))
  }

  tailSqrt(1.0)
}

mySqrt(64)