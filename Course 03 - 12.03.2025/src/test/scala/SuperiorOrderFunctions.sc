import scala.annotation.tailrec

def sumAll(start: Int, stop: Int): Int =
  @tailrec
  def loop(start: Int, acc: Int): Int =
    if (start > stop) acc
    else loop(start + 1, acc + start)
  loop(start, 0)


sumAll(0, 3)

/*
Reusing old code and just modifying a little BAD, VERY BAD
def sumSquares(start: Int, stop: Int): Int =
  @tailrec
  def loop(i: Int, acc: Int): Int =
    if(i > stop) acc
    else loop(i + 1, i * i + acc)
  loop(start, 0)

sumSquares(0, 3)
*/
// SCALA IS OOP, WE USE THAT
def sumWithf(f: Int => Int, start: Int, stop: Int): Int =
  @tailrec
  def loop(i: Int, acc: Int): Int =
    if (i > stop) acc
    else loop(i + 1, f(i) + acc)
  loop(start, 0)

// define the functions we want to pass
def id(x: Int): Int = x
def square(x: Int): Int = x * x

// pass the functions (object)
sumWithf(id, 0, 3)
sumWithf(square, 0, 3)

// or use lambda
sumWithf((x:Int) => x, 0, 3) // lambda x cu x de tip int
// or
sumWithf((x) => x, 0, 3)
// or
sumWithf(x => x * x, 0, 3)

// squares
sumWithf((x:Int) => x * x, 0, 3)
// or
sumWithf(x => x * x, 0, 3)

def alg1(x: Int): Int = x
def alg2(x: Int): Int = x * x
def alg3(x: Int): Int = x * x * x

// make it so user can just call
// applyAlg1(0, 10)
// applyAlg2(11, 99)

def currySumWithf(alg: Int => Int): (Int, Int) => Int =
{
  def sumWithF(start: Int, stop: Int): Int =
    @tailrec
    def loop(i: Int, acc: Int): Int =
      if (i > stop) acc
      else loop(i + 1, alg(i) + acc)
    loop(start, 0)
  sumWithF
}

// functions of superior order
val applyAlg1 = currySumWithf(x => x)
val applyAlg2 = currySumWithf(x => x * x)

applyAlg1(0, 10)
applyAlg2(11, 99)

// inchidere functionala = chem functia da nu dau toti parametrii
currySumWithf(x => x + 1)

// curry but cleaner/shorter (same thing)
def cleanSumWithF(f: Int => Int)(start: Int, stop: Int): Int =
{
  @tailrec
  def loop(i: Int, acc: Int): Int =
    if (i > stop) acc
    else loop(i + 1, f(i) + acc)
  loop(start, 0)
}

def fcurry(x: Int)(y: Int)(z: Int): Int = x + y +z
// Int => Int takes int, returns int
val v: Int => Int = fcurry(0)(1)

v(2)

// Int => Int => Int, takes int returns int, takes int returns int
val vp: Int => Int => Int = fcurry(1)

// Int => Int => Int => Int, takes int returns int, then takes int returns int, then takes int returns int
val vpp: Int => Int => Int => Int = fcurry

// uncurry
def funcurry (x: Int, y: Int, z: Int): Int = x + y + z

funcurry(1, 2, 3)

val vppp: (Int, Int, Int) => Int = funcurry

def compose (f: Int => Int, g: Int => Int): Int => Int =
  x => f(g(x))

compose(x => x + 1, x => 2 * x)(2) // g first then f of g

// same thing, just curry
def composep(f: Int => Int, g: Int => Int)(x: Int): Int =
  f(g(x))

cleanSumWithF(compose(x => x + 1, x => 2 * x))(0, 10)