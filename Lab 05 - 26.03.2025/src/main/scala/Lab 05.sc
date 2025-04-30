trait Nat {
  def isZero: Boolean
  def add(other: Nat): Nat
  def subtract(other: Nat): Nat
  def greater(other: Nat): Boolean
  def toInt: Int
}


case object Zero extends Nat {
  override def isZero: Boolean = true
  override def add(other: Nat): Nat = other
  override def subtract(other: Nat): Nat = Zero
  override def greater(other: Nat): Boolean = false
  override def toInt: Int = 0
}

case class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  override def add(other: Nat): Nat = Succ(n.add(other))
  override def subtract(other: Nat): Nat =
    other match {
      case Zero => this
      case Succ(m) =>
        if (m.greater(n)) Zero
        // since we take Succ(m) it's basically orig-1
        // that's why we use Succ(other)
        else Succ(n.subtract(Succ(other)))
    }
  override def greater(other: Nat): Boolean =
    other match {
      case Succ(m) => n.greater(m)
      case _ => true
    }
  override def toInt: Int = 1 + n.toInt
}
// When implementing the following methods,
// think about whether they are local
// (are they best implemented using functional or OO decomposition?)

val zero: Nat = Zero
val one: Nat = Succ(Zero)
val two: Nat = Succ(Succ(Zero))
val three: Nat = Succ(Succ(Succ(Zero)))
val four: Nat = Succ(Succ(Succ(Succ(Zero))))

zero.isZero
three.isZero

zero.add(two)
two.add(three)

two.subtract(one)
two.subtract(three)
three.subtract(one)

four.subtract(two)
four.subtract(three)

four.toInt
three.toInt

trait OList{
  def head: Int
  def tail: OList
  def foldRight[B](acc: B)(op: (Int,B) => B): B
  def foldLeft[B](acc: B)(op: (B,Int) => B): B
  def indexOf(i: Int): Int
  def filter(p: Int => Boolean): OList
  def map(f: Int => Int): OList
  def partition(p: Int => Boolean): (OList, OList)
  def slice(start: Int, stop: Int): OList
  def forall(p: Int => Boolean): Boolean
}

case object Void extends OList {
  override def head: Int = 0
  override def tail: OList = Void
  override def foldRight[B](acc: B)(op: (Int, B) => B): B = acc
  override def foldLeft[B](acc: B)(op: (B, Int) => B): B = acc
  override def indexOf(i: Int): Int = 0
  override def filter(p: Int => Boolean): OList = Void
  override def map(f: Int => Int): OList = Void
  override def partition(p: Int => Boolean): (OList, OList) = (Void, Void)
  override def slice(start: Int, stop: Int): OList = Void
  override def forall(p: Int => Boolean): Boolean = false
}

case class Cons(x: Int, xs: OList) extends OList {

  override def head: Int = x
  override def tail: OList = xs
  override def foldRight[B](acc: B)(op: (Int, B) => B): B = {
    def loop(l: OList): B =
      l match {
        case Void => acc
        case Cons(x, xs) => op(x, loop(xs))
      }
    loop(this)
  }

  override def foldLeft[B](acc: B)(op: (B, Int) => B): B = {
    def loop(acc: B, l: OList): B =
      l match {
        case Void => acc
        case Cons(x, xs) => loop(op(acc, x), xs)
      }

    loop(acc, this)
  }
  override def indexOf(i: Int): Int = {
    foldLeft(0)((a,b) =>
    // found i (the number we want the index of)
    if (b == i) return a // return the index
    // didn't find, move on
    else a + 1)
    // found nothing => -1
    -1
  }
  override def filter(p: Int => Boolean): OList = ???
  override def map(f: Int => Int): OList = ???
  override def partition(p: Int => Boolean): (OList, OList) = ???
  override def slice(start: Int, stop: Int): OList = ???
  override def forall(p: Int => Boolean): Boolean = ???
}

val l1: OList = Cons(1, Cons(2, Cons(3, Cons(4, Void))))
val l2: OList = Cons(5, Void)
val le: OList = Void

l1.foldLeft(0)(_+_)
l1.foldLeft(1)(_*_)

l1.foldRight(0)(_+_)
l1.foldRight(1)(_*_)

l2.foldRight(0)(_+_)
l2.foldRight(1)(_*_)

le.foldRight(0)(_+_)
le.foldRight(1)(_*_)

l1.indexOf(4)
l1.indexOf(5)
