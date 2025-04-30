import scala.annotation.tailrec

trait IList // List with Int type only

case object Void extends IList

case class Cons(x: Int, xs: IList) extends IList

val l1 = Cons(1, Cons(2, Cons(3, Void)))

l1 == Cons(1, Cons(2, Cons(3, Void)))

/*
4.1. Consider the following axioms for the operator isEmpty.

isEmpty : IList -> Boolean
isEmpty(Void) = true
isEmpty(Cons(h,t)) = false.

Implement isEmpty in Scala:
 */
def isEmpty(l: IList): Boolean =
{
  l match {
    case Void => true
    case _ => false
    // case Cons(_, _) => false
  }
}

//  4.2. Write down axioms for size : IList → Int
//  and implement the operator in Scala:
def size(l: IList): Int =
{
  l match {
    case Void => 0
    case Cons(x, xs) => 1 + size(xs)
  }
}

val l2 = Cons(1, Cons(2, Cons(3, Cons(4, Void))))
val l3 = Void

size(l2)
size(l3)

//  4.3. Implement contains which checks
//  if an element is a member of a list.
@tailrec
def contains(e: Int, l: IList): Boolean =
{
  (e, l) match {
    case (e, Void) => false
    case (e, Cons(x, xs)) => (e == x) || contains(e, xs)
  }
}

contains(1, l2)
contains(0, l2)

// 4.4. Implement max which returns
// the largest integer from a list:
def max(l: IList): Int =
{
  l match {
    case Cons(x, xs) =>
      val maxNum = max(xs)
      if (x > maxNum) x
      else maxNum
    case Void => 0
  }
}

val l4 = Cons(1, Cons(3, Cons(5, Cons(2, Void))))
max(l2)
max(l3)
max(l4)

//  4.5. Implement take which returns
//  a new list containing
//  the first n elements of the original list:
def take(n: Int)(l: IList): IList =
{
  (n, l) match {
    case (n, Void) => Void
    case (0, xs) => Void
    case (n, Cons(x, xs)) => Cons(x, take(n - 1)(xs))
  }
}

take(3)(l2)
take(2)(l2)
take(10)(l2)

//  4.6. Implement drop which returns
//  a new list containing the original list
//  without the first n elements:
@tailrec
def drop(n: Int)(l: IList): IList =
{
  (n, l) match {
    case (n, Void) => Void
    case (0, xs) => xs
    case (n, Cons(x, xs)) => drop(n - 1)(xs)
  }
}

l2
size(l2)
drop(2)(l2)

//  4.7. Implement append which
//  concatenates two lists:
def append(l1: IList, l2: IList): IList =
{
  l1 match {
    case Void => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }
}

//  4.8. (!) Implement last which returns
//  the last element from a list:
@tailrec
def last(l: IList): Int =
{
  l match {
    case Cons(x, Void) => x
    case Cons(x, xs) => last(xs)
    case Void => 0
  }
}

l2
size(l2)
l3
size(l3)

last(l2)
last(l3)

//  4.9. (!) Implement reverse.
//  There are two different ways
//  to implement reverse
//  (with direct and with tail-end recursion).
//  Try both implementations.
def reverse(l: IList): IList =
{
  l match {
    case Cons(x, xs) => append(reverse(xs), Cons(x, Void))
    case Void => Void
  }
}

l4
reverse(l4)

//  4.10. Implement isSorted which
//  checks if a list is sorted:

def isSorted(l: IList): Boolean = {
  l match {
    case Void => true

  }
}

