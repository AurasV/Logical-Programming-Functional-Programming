def acceptable(xn: Double, a: Double): Boolean =
{
  (xn * xn - a).abs <= 0.001
}

acceptable(4.000051, 16)