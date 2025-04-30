import scala.annotation.tailrec

def sumNats(start: Int, stop: Int): Int =
{
  if(start > stop) 0
  else start + sumNats(start + 1, stop)
}
// sumNats(4,10) = 49
// sumNats(4,11) = 60
// sumNats(4,12) = 72
// sumNats(3,12) = 75
// sumNats(3, 1000000) = 705082701, overflows on normal
sumNats(3, 100000)

def tailSumNats(start: Int, stop: Int): Int =
{
  @tailrec
  def tailSumNatsAux(i: Int, acc: Int): Int =
  {
    if(i > stop) acc
    else tailSumNatsAux(i + 1, acc + i)
  }
  tailSumNatsAux(start, 0)
}

tailSumNats(3, 100000)