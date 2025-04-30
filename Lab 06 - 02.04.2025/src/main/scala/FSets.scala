import scala.annotation.tailrec

object FSets {

  type Set = Int => Boolean

  def tokenID: Int = 861679

  def member(e: Int)(s: Set): Boolean =
  {
    // it returns a bool so we can just
    // test the set(var) directly
    s(e)
  }

  def singleton(x: Int): Set =
  {
    // just make a set
    // I'm not sure if we can use
    // (x: Int) instead, I never checked
    (y: Int) => y == x
  }

  def ins(x: Int)(s: Set): Set =
  {
    // I spent half an hour trying to figure out
    // why (y: Int) => y == x || s wouldn't work
    // then I realized s(y) is the boolean value...
    (y: Int) => y == x || s(y)
  }

  def fromBounds(start: Int, stop: Int): Set =
  {
    // just need to check if the number is within the range
    (y: Int) => (y >= start) && (y <= stop)
  }

  def union (s1: Set, s2: Set): Set =
  {
    // we just unite (or) the conditions for them
    (x: Int) => s1(x) || s2(x)
  }

  def complement(s1: Set): Set =
  {
    // literally just the opposite
    (x: Int) => !s1(x)
  }

  def sumSet(b: Int)(start: Int, stop: Int)(s: Set): Int = {
    @tailrec
    def aux(crt: Int, acc: Int): Int =
    {
      // tried if(crt > stop && s(crt)) acc for a bit |||| doesn't work (takes too long I guess)
      if(crt > stop) acc
      else if(s(crt)) aux(crt + 1, acc + crt)
      else aux(crt + 1, acc)
    }
    aux(start, b)
  }

  def foldLeftSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    @tailrec
    def aux(crt: Int, acc: Int): Int =
    {
      if(crt > stop) acc
      // else if(s(crt)) aux(crt + 1, op(crt, acc)) this would be wrong
      // got confused because of how aux is made (crt,acc), my bad
      else if(s(crt)) aux(crt + 1, op(acc, crt))
      else aux(crt + 1, acc)
    }

    // aux(0, b)
    // ^ wrong, we start from the start of the interval!!!! (AKA *start* in this case)
    aux(start, b)
  }

  def foldRightSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int =
  {
    def aux(crt: Int): Int =
    {
      // return initial val as the last one so at the end we do (x op b )
      if(crt > stop) b
      // the current element is part of the set we do right fold
      // by performing the operation using current and NEXT element
      // basically we do (a op (b op (c op b))) recursively
      else if(s(crt)) op(crt, aux(crt + 1))
      // if it's not part of the set we move on to next oneeeee
      else aux(crt + 1)
    }

    // beginning from the start as well
    aux(start)
  }

  def filter(p: Int => Boolean)(s: Set): Set =
  {
    // just check that both p and s hold
    (x: Int) => p(x) && s(x)
  }

  def partition(p: Int => Boolean)(s: Set): (Set,Set) =
  {
    // ((x: Int) => p(x) && s(x), (x: Int) => p(x) && !s(x))
    // huh..? oh I was negating the set instead of the requirement...
    ((x: Int) => p(x) && s(x), (x: Int) => !p(x) && s(x))
  }

  def forall(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean =
  {
    @tailrec
    def aux(crt: Int): Boolean =
    {
      if(crt > stop) true
      // else if (!cond(crt)) false |||| I need to also check if it's actually in the set lol
      // I looked in the tests to figure that out D:
      else if(!cond(crt) && s(crt)) false
      else aux(crt + 1)
    }

    aux(start)
  }

  def exists(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean =
  {
    // same thing as forall just reverse, if we find one good one => true
    // if we don't find no good one => false
    @tailrec
    def aux(crt: Int): Boolean =
    {
      if(crt > stop) false
      else if (cond(crt) && s(crt)) true
      else aux(crt + 1)
    }

    aux(start)
  }

  def setOfDivByK(k: Int): Set =
  {
    // (x: Int) => x % k == 0
    // I don't really see the point in using other functions to get the set of div by k,
    // but I'll implement it like that because it was in the problem text
    filter((x: Int) => x % k == 0)((y: Int) => true)
  }

  def moreDivs(k: Int)(start: Int, stop:Int)(s1: Set, s2: Set): Boolean =
  {
    // find all values divisible by k
    val divisible = setOfDivByK(k)
    // find all values *in range* divisible by k
    val divisibleInRange = filter((x: Int) => x < stop && x > start)(divisible)

    // find all values in range divisible by k *in s1!*
    val s1DivisibleInRange = filter((x: Int) => s1(x))(divisibleInRange)
    // find all values in range divisible by k *in s2!*
    val s2DivisibleInRange = filter((x: Int) => s2(x))(divisibleInRange)

    // find the sum of all the values in range divisible by k *in s1!*
    val s1Sum = sumSet(0)(start, stop)(s1DivisibleInRange)
    // find the sum of all the values in range divisible by k *in s2!*
    val s2Sum = sumSet(0)(start, stop)(s2DivisibleInRange)

    // compare the sums and return true if s1 has more false if not (automatically)
    s1Sum > s2Sum

    // just taking it step by step my brain is
    // too tired to be able to do complicated one-liners
    // and if I was capable of doing them right now
    // I'd probably get confused later
  }
}
