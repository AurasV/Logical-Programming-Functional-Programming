import scala.annotation.tailrec

trait Nat // similar to Interface in Java

// use object instead of class to make Zero be unique
// instead of multiple classes that are the same ig
case object Zero extends Nat
case class Succ(n: Nat) extends Nat

val n1 = Zero
val n2 = Zero

n1 == n2 // false because we should compare 2 references

Succ(Zero) == Succ(Zero) // false
//class MyClass(val x: Int){}

/*
  In Java:
  public class Succ extends Nat
  {
    private Nat n;
    public Succ(nat n)
    {
      this.n = n;
    }
  }

*/

def isZero(n: Nat): Boolean =
{
  n match {
    case Zero => true
    case Succ(m) => false // m instead of n
  }
}

isZero(Zero)
isZero(Succ(Succ(Zero)))

def add(n: Nat, m: Nat): Nat =
{
  n match {
    case Zero => m
    case Succ(np) => Succ(add(np, m)) // add(m, np) <- bad idea, but correct
  }
}

add(Succ(Zero), Succ(Succ(Zero)))

def largerThanThree(n: Nat): Boolean =
{
  n match{
    case Succ(Succ(Succ(Succ(_)))) => true
    case _ => false // anything else
  }
}

val pair = (1, 2) // the "pair" type

@tailrec
def greater(n: Nat, m: Nat): Boolean =
{
  (n, m) match {
    case (Succ(np), Succ(mp)) => greater(np, mp)
    case (Succ(_), Zero) => true
    case (Zero, _) => false
  }
}

greater(Succ(Succ(Zero)), Succ(Zero)) // 2, 1

greater(Succ(Zero), Succ(Succ(Zero))) // 1, 2


trait IList // List with Int type only

case object Void extends IList

case class Cons(x: Int, xs: IList) extends IList

val l1 = Cons(1, Cons(2, Cons(3, Void)))

l1 == Cons(1, Cons(2, Cons(3, Void)))

def isEmpty(l: IList): Boolean =
{
  l match {
    case Void => true
    case _ => false
    // case Cons(_, _) => false
  }
}

def append(l1: IList, l2: IList): IList =
{
  l1 match {
    case Void => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }
}