import scala.annotation.tailrec

def sumSquares(n: Int): Int =
{
  @tailrec
  def sumSquaresAux(i: Int, acc: Int): Int =
    {
      if(i > n) acc
      else sumSquaresAux(i + 1, acc + i * i)
    }
  sumSquaresAux(0,0)
}
// sumSquares(5) = 55
// sumSquares(6) = 91
// sumSquares(7) = 140
// sumSquares(8) = 204
sumSquares(8)

