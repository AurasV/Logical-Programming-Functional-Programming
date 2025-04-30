import scala.annotation.tailrec

def nth_guess(n: Int, a: Double): Double =
{
  @tailrec
  def nth_guessAux(i: Int, acc: Double): Double =
  {
    if(i > n) acc
    else nth_guessAux(i + 1, 0.5 * (acc + a / acc))
  }
  nth_guessAux(0, 1)
}

nth_guess(5, 16)