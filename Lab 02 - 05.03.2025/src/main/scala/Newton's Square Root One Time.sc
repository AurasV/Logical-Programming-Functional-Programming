def improve(xn: Double, a: Double): Double =
{
  0.5 * (xn + a / xn)
}

improve(1, 16)