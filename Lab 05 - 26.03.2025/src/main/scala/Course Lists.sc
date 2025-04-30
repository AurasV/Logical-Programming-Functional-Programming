// Using Scala lists

// Cons(1, Cons(2, Cons(3, Cons(4, Void))))
val l: List[Int] = 1 :: 2 :: 3 :: 4 :: Nil

l.head
l.tail

def sum(l: List[Int]): Int = {
  l match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }
}

def prod(l: List[Int]): Int = {
  l match {
    case Nil => 1
    case x :: xs => x * prod(xs)
  }
}

sum(l)
prod(l)

// acest cod est repetitiv
// structura de cod est identica. Ce este diferit?
//  - valoarea intiala de la care incepem 0 / 1
//  - operatia in sine + / *

// fold right
def fold1(acc: Int)(op: (Int, Int) => Int)(l: List[Int]): Int = {
  def loop(l: List[Int]): Int =
    l match {
      case Nil => acc
      case x :: xs => op(x, loop(xs))
    }
  loop(l)
}

val sum1 = fold1(0)((a,b) => a + b)
// shorhand pentru operatii simple
fold1(0)(_+_)(List(1, 2, 3, 4))

val prod1 = fold1(1)((a,b) => a * b)

sum1(List(1, 2, 3, 4))
prod1(List(1, 2, 3, 4))

// fold left
def fold2(acc: Int)(op: (Int, Int) => Int)(l: List[Int]): Int = {
  def loop(acc: Int, l: List[Int]): Int =
    l match {
      case Nil => acc
      case x :: xs => loop(op(acc, x), xs)
    }
  loop(acc, l)
}

// fold left
def fold3[B](acc: B)(op: (B, Int) => B)(l: List[Int]): B = {
  def loop(acc: B, l: List[Int]): B =
    l match {
      case Nil => acc
      case x :: xs => loop(op(acc, x), xs)
    }
  loop(acc, l)
}

fold2(1)(_ + _)(List(2, 3, 4))

List(1, 2, 3, 4).foldRight(0)(_ + _)
List(1, 2, 3, 4).foldLeft(1)(_ * _)

val reverse = fold3(Nil)((x: List[Int], y: Int) => y :: x)

reverse(List(1, 2, 3))

