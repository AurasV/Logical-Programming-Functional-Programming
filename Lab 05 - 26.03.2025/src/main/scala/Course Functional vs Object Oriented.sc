
/*
import scala.annotation.tailrec

trait Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat
case object Infinity extends Nat

def isZero(x: Nat): Boolean =
  x match {
    case Zero => true
    case Succ(_) => false
    case Infinity => false
  }

def add(x: Nat, y: Nat): Nat =
  x match {
    case Zero => y
    case Succ(xp) => Succ(add(xp, y))
    case Infinity => false
  }

@tailrec
def equals(x: Nat, y: Nat): Boolean =
  (x, y) match {
    case (Succ(xp), Succ(yp)) => equals(xp, yp)
    case (Zero, Zero) => true
    case (Infinity, Infinity) => true
    case _ => false
  }

def greater(x: Nat, y: Nat): Boolean =
  (x, y) match {
    case (Succ(xp), Succ(yp)) => greater(xp, yp)
    case (Zero, _) => false
    case (Succ(_), Zero) => true
  }
*/



/*
trait Nat {
  def isZero: Boolean
  def add(other: Nat): Nat
  def equals(other: Nat): Boolean
  def greater(other: Nat): Boolean
}

case object Zero extends Nat {
  override def isZero: Boolean = true
  override def add(other: Nat): Nat = other
  override def equals(other: Nat): Boolean =
    other match {
      case Zero => true
      case _ => false
    }
  override def greater(other: Nat): Boolean = false
}

case class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  // Succ(n) + other
  override def add(other: Nat): Nat = Succ(n.add(other))
  override def equals(other: Nat): Nat =
    other match {
      case Zero => false
      case Succ(m) => n.equals(m)
    }

  override def greater(other: Nat): Boolean =
    other match {
      case Succ(m) => n.greater(m)
      case _ => true
    }
}

case object Infinity extends Nat {
  override def isZero: Boolean = ???
  override def add(other: Nat): Nat = ???
  override def equals(other: Nat): Boolean = ???
  override def greater(other: Nat): Boolean = ???
}

*/

/*
  Ipoteza 1: Vrem sa adaugam noi feluri de valori in tipul nostru.
    Mult mai simplu, in varianta object oriented.
  Ipoteza 2: Vrem sa adaugam o noua operatie
    Mult mai simplu, in varianta functionala.
*/


/*
trait IList
case object Void extends IList
case class Cons(x: Int, xs: IList) extends IList

def size(l: IList): Int = ???
def take(n: Int)(l: IList): IList = ???
def drop(n: Int)(l: IList): IList = ???

def mergeSort(l: IList): IList =
  def merge(l1: IList, l2: IList): IList =
    (l1, l2) match {
      case (Cons(x, xs), Cons(y, ys)) =>
        if (x < y) Cons(x, merge(xs, l2))
        else Cons(y, merge(l1, ys))
      case (Void, _) => l2
      case (_, Void) => l1
    }
  l match {
    case Void => l
    case Cons(_, Void) => l
    case _ => {
      val mid = size(l)
      merge(mergeSort(take(mid)(l)), mergeSort(drop(mid)(l)))
    }
  }
*/

trait IList {
  def size: Int
  def take(n: Int): IList
  def drop(n: Int): IList
  def merge(other: IList): IList
  def mergeSort: IList
}

case object Void extends IList {
  override def size: Int = ???
  override def take(n: Int): IList = ???
  override def drop(n: Int): IList = ???
  override def merge(other: IList): IList = other
  override def mergeSort: IList = this
}

case class Cons(x:Int, xs: IList) extends IList {
  override def size: Int = ???
  override def drop(n: Int): IList = ???
  override def take(n: Int): IList = ???
  override def merge(other: IList): IList =
    other match {
      case Void => this
      case Cons(y, ys) =>
        if (x < y) Cons(x, xs.merge(other))
        else Cons(y, this.merge(xs))
    }
  override def mergeSort: IList = {
    xs match {
      case Void => this
      case _ => {
        val mid = this.size / 2
        this.
          take(mid).                        // luam prima jumatate din lista
          mergeSort.                        // o sortam recursiv
          merge(this.drop(mid).mergeSort)   // o interclasam cu cealalta lista
      }
    }
  }
}