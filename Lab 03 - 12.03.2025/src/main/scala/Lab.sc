import java.awt.geom.Line2D
import scala.annotation.tailrec

/*
3.1. Define the function foldWith which uses an operation op,
and an initial value b to reduce a range of integers to a value.
For instance, given that op is addition (+), the result of folding
the range 1 to 3 with b = 0 will be (((0 + 1) + 2) + 3) = 6.
foldWith should be curried (it will take the operation and return
another function which expects the bounds).
*/

def foldWith (b: Int)(op: (Int,Int) => Int)(start: Int, stop: Int): Int =
{
  @tailrec
  def tail_fold(i: Int, acc: Int): Int  =
    {
      if (i > stop) acc
      else tail_fold(i + 1, op(i, acc))
    }

  tail_fold(start, b)
}

def add(x: Int, y: Int): Int = x + y

val foldAdd = foldWith(0)(add)

foldAdd(0, 3)

/*
3.2. Define the function foldConditional which extends foldWith
by also adding a predicate p: Int ⇒ Int. foldConditional will
reduce only those elements of a range which satisfy the predicate.
*/

def foldConditional(b: Int)(op: (Int,Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int =
{
  @tailrec
  def tail_fold_conditional(i: Int, acc: Int): Int =
    {
      if (i > stop) acc
      else if (p(i)) tail_fold_conditional(i + 1, op(i, acc))
      else tail_fold_conditional(i + 1, acc)
    }

  tail_fold_conditional(start, b)
}

def predicate(x: Int): Boolean = x % 2 == 0

def addConditional(x: Int, y: Int): Int = (x + y) * 2

val foldAddConditional = foldConditional(0)(addConditional, predicate)

foldAddConditional(0, 4) // only 0, 2 and 4 are even (0 + 2) * 2 + (2 + 4) * 2 = 16

/*
3.3. Implement the function foldRight which has
the same behaviour as foldWith, but the order in
which the operation is performed is now: 1+(2+(3+0))=6.
What is the simplest way to implement it?
*/

def foldRight(b: Int)(op: (Int,Int) => Int)(start: Int, stop: Int): Int =
{
  @tailrec
  def tail_fold(i: Int, acc: Int): Int =
    {
      if (i < start) acc
      else tail_fold(i - 1, op(i, acc))
    }

  tail_fold(stop, b)
}

def addRight(x: Int, y: Int): Int = x + y

val foldAddRight = foldRight(0)(addRight)

foldAddRight(0, 3)

/*
3.4. Write a function foldMap which takes values
a1,a2,…,ak from a range and computes f(a1) op f(a2) op …f(ak) .
*/

def foldMap(op: (Int,Int) => Int, f: Int => Int)(start: Int, stop: Int): Int =
{
  @tailrec
  def tail_foldMap(i: Int, acc: Int): Int =
    {
      if (i > stop) acc
      else tail_foldMap(i + 1, op(f(i), acc))
    }

  tail_foldMap(start, 0)
}

def addMap(x: Int, y: Int): Int = x + y

def sum(x: Int): Int = x + x

val foldAddMap = foldMap(addMap, sum)

foldAddMap(0, 3)

// 0 + 0 + 1 + 1 + 2 + 2 + 3 + 3 = 12

// 3.5. Write a function which computes 1+2^2+3^2+…+(n−1)^2+n^2 using foldMap.

def sumSquares(n: Int): Int =
{
  def square(x: Int): Int = x * x

  foldMap(addMap, square)(1, n)
}

sumSquares(4)
// 1^2 + 2^2 + 3^2 + 4^2 = 1 + 4 + 9 + 16 = 14 + 16 = 30

// 3.6. Write a function hasDivisor which checks
// if a range contains a multiple of k. Use foldMap and choose f carefully.

def hasDivisor(k: Int, start: Int, stop: Int): Boolean =
{
  def multiple(x: Int): Int =
  {
    if (x % k == 0 && x != k) 1
    else 0
  }

  foldMap(addMap, multiple)(start, stop) > 0
}

hasDivisor(19, 1, 30)
// are there any multiples on 19 in [1,30]? Well... besides 19, no

/*
 3.7. We can compute the sum of an area defined by a function within a range a,b
 (the integral of that function given the range), using the following recursive scheme:

    if the range is small enough, we treat f as a line (and the area as a trapeze).
    It's area is (f(a)+f(b))(b−a)/2

    otherwise, we compute the mid of the range, we recursively compute
    the integral from a to mid and from mid to b, and add-up the result.

Implement the function integrate which computes the integral of a function f given a range:
*/

def integrate(f: Double => Double)(start: Double, stop: Double): Double =
{
  @tailrec
  def tail_integrate(i: Double, j: Double, acc: Double): Double =
  {
    if (j - i < 0.0001) acc + ((f(start) + f(stop)) * (stop - start)) / 2
    else tail_integrate(i, (j - i) / 2, acc)
  }

  tail_integrate(start, stop, 0)
}

def f(x: Double): Double = x * x

integrate(f)(0, 80)


/*
3.8. We define Line2D to be lines in a 2-dimensional space,
and we represent them as functions. Write a function which
takes a Line2D and translates it up on the Ox axis by a given offset.
For instance, translateOx of 2 on y=x+1 will return y=x+3.
*/
type Line2D = Int => Int // ITS AN ALIAS!!!!!

def translateOx(offset: Int)(l: Int => Int): Int => Int =
{
  
}

def fLine(x:Int): Int = x + 1

val v = fLine

translateOx(2)(v)
