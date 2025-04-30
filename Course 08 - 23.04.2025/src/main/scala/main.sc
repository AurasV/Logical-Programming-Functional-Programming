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

// same as .sum
List(1, 2, 3, 4).foldRight(0)(_ + _)
// same as .product
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


// we start to implement the procedure to parse the matrix


def parse(s: String): List[List[Int]] =
{
  def split(sep: Char)(l: Str): List[Str] =
  {
    def op(c: Char, acc: List[Str]): List[Str] =
      acc match {
        case Nil => if (c == sep) Nil else List(List(c))
        case line :: xs =>
          if (c == sep) Nil :: acc
          else (c :: line) :: xs
      }
    l.foldRight(Nil)(op)
  }

  def makeInt(secv: List[Char]): Int =
    secv.foldRight("")(_ + _).toInt

  split('\n')(s.toList) // : List[List[Char]]
    .map(split(' '))    // : List[List[List[Char]]]
    .map(_.map(makeInt)) // : List[List[Int]]
  // .map(line => line.map(cell => makeInt(cell)))
  // go on each line, and apply to each cell makeInt
}

// _ + _  adds the char (first parameter) to
// the beginning of the string (2nd parameter)
List('1', '2', '3').foldRight("")(_ + _)

parse(text)

// este ceva ce putem generaliza cu un beneficiu imediat?
// putem generaliza separatoarele (not interesting)

// putem generaliza tipul continut de matrice

type Tabular[A] = List[List[A]]

def readTabular[A](read: List[Char] => A)(s: String): List[List[A]] =
{
  def split(sep: Char)(l: Str): List[Str] =
  {
    def op(c: Char, acc: List[Str]): List[Str] =
      acc match {
        case Nil => if (c == sep) Nil else List(List(c))
        case line :: xs =>
          if (c == sep) Nil :: acc
          else (c :: line) :: xs
      }
    l.foldRight(Nil)(op)
  }

  split('\n')(s.toList) // : List[List[Char]]
    .map(split(' '))    // : List[List[List[Char]]]
    .map(_.map(read))             // : List[List[A]]
  // .map(line => line.map(cell => makeInt(cell)))
  // go on each line, and apply to each cell makeInt
}

val parse1: String => Tabular[String] =
  readTabular(_.foldRight("")(_ + _))

parse1(text)


// o alta posibila generalizare.
// Noi am presupus ca tipul este unic. Nu putem tipuri diferite.
// Am putea acomoda valori cu tipuri diferite in matrice?

// Se poate, astfel:
// "Boxing types"
trait MVal
case class MString (s: String) extends MVal
case class MInt (s: Int) extends MVal

// daca toate caracterele sunt numerice, atunci e int, altfel e string
def mread(secv: List[Char]): MVal =
  // if we have at least one int
  if (secv.map(_.isDigit).foldRight(true)(_ && _)) // este Int
    MInt(secv.foldRight("")(_ + _).toInt)
  else MString(secv.foldRight("")(_ + _)) // este string


val parse2: String => Tabular[MVal] =
  readTabular(mread) // "fake" read function


val text2 =
  """1 2 3
    |4 5 6
    |7 888 9x9
    |""".stripMargin

parse2(text2)


// (B) vrem sa facem ceva cu matricile paraste pana acum.
/*    Vrem sa implementam operatii peste matrici
*     Ne ocupam intai de forma pe care o vor lua operatiile
* */

// plus(m1, m2)                           not like this !!!!
// plus(mult(m1,m2), mult(m3, m4))        <- It will lead to this, bad
// (m1 * m2) + (m3 * m4)                  <- LIKE THIS!!!!

trait Nat {
  def +(other: Nat): Nat
  // def plus(other: Nat): Nat
}
case object Zero extends Nat {
  override def +(other: Nat): Nat = other
  // override def plus(other: Nat): Nat = other
}
case class Succ(n: Nat) extends Nat {
  override def +(other: Nat): Nat = Succ(n + other)
  // override def plus (other: Nat): Nat = Succ(n.plus(other))
}


// In Scala exista o notatie alternativa pentru apelul unei metode
// (membru a unei clase)
// instead of o.f(p)
//            o f p

// Zero plus Zero
// Zero.plus(Zero)
// Succ(Zero) plus Succ(Zero)

//In Scala, putem folosi nume de functii care contin caractere speciale

1.+(2)
1 + 2

//class C1 {
//  def f
//}

//class C2 {
//  def f
//}

//List(1,2,3).zip(List(3,4,5)) = List((1,4), (2,4), (3,5))
/*
    List( List(1,2),     zip     List(List(5,6),
          List(3,4))                  List(7,8) )

    List( (List(1,2), List(5,6)),
          (List(3,4), List(7,8)) )

    aplicam functia zip, "pe fiecare pereche"

    List( List( (1,5), (2,6) ),
          List( (3,7), (4,8) ) )


*/

// pentru a introduce + si * ca operatii peste matrici, trebuie sa creem o clasa
class Matrix(val inner: Tabular[Int]) {
  // val => immutable => inner is immutable
  def + (other: Matrix): Matrix = {
    Matrix(
    this.inner
      .zip(other.inner)
      .map(p => p._1 zip p._2)
      .map(_.map(p => p._1 + p._2)))
  }

  def transpose: Matrix = {
    def trans(l: Tabular[Int]): Tabular[Int] =
      l match {
        case Nil :: _ => Nil
        case _ => l.map(_.head) :: trans(l.map(_.tail))
      }
    Matrix(trans(this.inner))
  }

/*
  def * (other: Matrix): Matrix = {
    Matrix(
      this.inner
        .map(li => other.transpose.inner
          .map(cj => li
            .zip(cj)
              .map(p => p._1 * p._2)
                .foldLeft(0)(_ + _)))
    )
  }
*/

  def * (other: Matrix): Matrix = {
    Matrix(
      for li <- this.inner
        yield
          for cj <- other.transpose.inner
            yield
              (for p <- li zip cj yield p._1 * p._2).foldLeft(0)(_ + _)
    )
    // this.inner.map(li => ...)
  }

  override def toString: String = {
    def reducer(delim: Char)(l: List [String]): String =
      l.foldRight("")((v, acc) => v + delim + acc)

    val v = inner
      .map(_.map(_.toString))
      .map(reducer(' '))

    "\n" ++ reducer('\n')(v)
  }
}

// companion object
object Matrix {
  // supraincarcare
  def apply(s: String): Matrix =
    new Matrix(parse(s))

  def apply(t: Tabular[Int]): Matrix =
    new Matrix(t)
}

// o =  new C
// o(p) este posibil, daca exista implementata o metoda "apply" in clasa C
// o.apply(p) este de fapt o(p)
// Matrix()

// sub-problem 1.

def fromString(s: String): Matrix =
  new Matrix(parse(s))

def fromTabular(t: Tabular[Int]): Matrix =
  new Matrix(t)

// si alte functii.... in functie de locurile de unde "vin" matricile
// pe masura ce "locurile" sunt tot mai multe, programatorul trebuie sa memoreze
// "artificial" nume de functii. Putem gasi o forma universala de nume,
// cu care sa putem construi orice?

// soluctie: companion objects

// f(10) //In Scala, TOTUL este un obiect.
// f trebuie sa fie si el obiect si apelul trebuie sa fie o metoda a obiectului


// functiile sunt obiecte, cu un anume tip, si care au
// implementata o metoda apply.
// Exemplu:

class F {
  def apply(i: Int): Int = i + 1
}

// F(2) doesn't work since F is the object
val f = new F
f(2)


object G {
  def apply(i: Int): Int = i + 1
}

// F(2) doesn't work since F is the object
G(2)


val m1 = Matrix("1 2\n3 4")
val m2 = Matrix(List(List(1,2), List(3,4)))
val m3 = Matrix("5 6\n7 8")

m1 == m2

m1 + m3

val m4 = Matrix("1 2 3\n4 5 6\n7 8 9")
val m5 = Matrix("10 11 12\n13 14 15\n16 17 18")
m4 + m5
m4.transpose

m4 * m5

List(1, 2, 3, 4).filter(_ % 2 == 0).map(_ + 1)

// for x in
// List(1, 2, 3, 4).map(_ + 1)
// for expression => map
// in Python:
// [x + 1 for x in [1, 2, 3, 4]
for x <- List(1, 2, 3, 4) yield x + 1

for x <- List(1, 2, 3, 4) if x % 2 == 0 yield x + 1

// for <list of generators> [with condition] yield <expression>

// produs cartezian
for x <- List(1, 2, 3); y <- List(4, 5, 6) yield (x, y)
// OR
for x <- List(1, 2, 3)
    y <- List(4, 5, 6)
      yield (x,y)
// INDENTATION DOESN'T MATTER HERE!!!!! ^^^

for x <- List(1, 2, 3)
    yield
      for y <- List(4, 5, 6)
        yield (x,y)