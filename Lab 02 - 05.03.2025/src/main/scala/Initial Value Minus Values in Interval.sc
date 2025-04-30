import scala.annotation.tailrec

def subtractRange(x: Int, start: Int, stop: Int): Int =
{
  @tailrec
  def subtractRangeAux(i: Int, acc: Int): Int =
  {
    if(i > stop) acc
    else subtractRangeAux(i + 1, acc - i)
  }
  subtractRangeAux(start, x)
}

// sum 1 to 10000 is 50005000
subtractRange(5000, 1, 10000)