import scala.annotation.tailrec

def fact(n: Int): Int =
{
  @tailrec
  def fact_aux(i: Int, acc: Int): Int =
    if(i > n) acc
    else fact_aux(i + 1, acc * i)
  fact_aux(1, 1)
}

fact(10)