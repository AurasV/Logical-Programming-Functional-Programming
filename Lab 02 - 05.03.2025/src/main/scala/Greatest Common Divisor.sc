import scala.annotation.tailrec

@tailrec
def gcd(a: Int, b: Int): Int =
{
  if(b == 0) a
  else gcd(b, a % b)
}

gcd(30,1500051693)