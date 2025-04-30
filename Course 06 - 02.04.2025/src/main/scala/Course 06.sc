import scala.annotation.tailrec
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
  @tailrec
  def loop(acc: Int, l: List[Int]): Int =
    l match {
      case Nil => acc
      case x :: xs => loop(op(acc, x), xs)
    }
  loop(acc, l)
}

// fold left
def fold3[T](acc: T)(op: (T, Int) => T)(l: List[Int]): T = {
  @tailrec
  def loop(acc: T, l: List[Int]): T =
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
val reverse2 = fold3(Nil :List[Int])((x, y) => y :: x)

reverse(List(1, 2, 3))
reverse2(List(1, 2, 3))

// fold right
def fold4(acc: Int, op: (Int, Int) => Int, l:List[Int]): Int =
  l match {
    case Nil => acc
    case x :: xs => op(x, fold4(acc, op, xs))
  } // better approach


// A extends T
/*
def fold5[T](acc: T)(op: (T, Int) => T)(l: List[Int]): T = {
  @tailrec
  def loop[A <: T](acc: A, l: List[Int]): A =
    l match {
      case Nil => acc
      case x :: xs => loop(op(acc, x), xs)
    }
  loop(acc, l)
}
*/

1 :: 2 :: 3 :: Nil
1 :: (2 :: (3 :: Nil))

List(1, 2, 3).foldRight(Nil:List[Int])(_ :: _)
List(1, 2, 3).foldLeft(Nil:List[Int])((x, y) => y :: x)

List(1, 2, 3).map(_ + 1)
List(1, 2, 3).map(_ :: Nil)
List(1, 2, 3).map((_, 0))


def mapRecursive[A, B](f: A => B)(l: List[A]): List[B] =
{
  l match {
    case Nil => Nil
    case x :: xs => f(x) :: mapRecursive(f)(xs)
  }
}

def mapUsingFold[A, B](f: A => B)(l: List[A]): List[B] =
{
  // f(x) :: xs practically
  l.foldRight(Nil: List[B])((x: A, acc: List[B]) => f(x) :: acc)
}

mapUsingFold((x: Int) => x + 1)(List(1,2,3))

List(1, 2, 3, 4).filter(_ % 2 == 0)

def filterWithFold[A](p: A => Boolean)(l: List[A]): List[A] =
{
  l.foldRight(Nil)((x: A, acc: List[A]) => if (p(x)) x :: acc else acc)
}

filterWithFold((x:Int) => x % 2 == 0)(List(1, 2, 3, 4))

List(1, 2, 3, 4).zip(List(5, 6, 7, 8))
List(1, 2, 3, 4, 9).zip(List(5, 6, 7, 8)) // extra ignored
List(1, 2, 3, 4).zip(List(5, 6, 7, 8, 9)) // extra ignored

/*
  Sa construim un mic parser
  o functie care citeste un string si il transforma
  in ceva ma complicat
*/

val text =
  """1 2 3
    |4 5 6
    |7 8 999
    |""".stripMargin

type Matriceal = List[List[Int]]
type Str = List[Char]

val text_after = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

text.toList

def split(sep: Char, l: Str): List[Str] =
{
  def op(c: Char, acc: List[Str]): List[Str] =
  {
    acc match {
      case Nil =>
        if (c == sep) Nil
        else List(List(c))
      // line last line found, xs the rest of acc
      case line :: xs =>
        // we're done with current line add nil to acc
        // so we can start doing next line
        if (c == sep) Nil :: acc
        // add char found to first pos in last line we found
        // and then add that to the big list
        else (c :: line) :: xs
    }
  }
  l.foldRight(Nil)(op)
}

split('\n', text.toList)