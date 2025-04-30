object Matrix extends App{
  // define your functions here

  // write your test code here


  type Matrix = List[List[Int]]

  val m2 = List(List(1, 2), List(3, 4))
  val m3 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  val m4 = List(List(10, 11, 12), List(13, 14, 15), List(16, 17, 18))

  def sumAll(m: Matrix): Int = {
    // add what's inside the inner lists
    // then add all the lists together
    m.map(_.sum).sum
  }

  println(sumAll(m2))
  println(sumAll(m3))

  def scalarMult(const: Int, m: Matrix): Matrix = {
    for x <- m
      yield
        for y <- x
          yield y * const
  }

  println(scalarMult(3, m2))
  println(scalarMult(10, m3))

  def add(m1: Matrix, m2: Matrix): Matrix = {
    m1.zip(m2)
      .map(p => p._1 zip p._2)
      .map(_.map(p => p._1 + p._2))
  }

  println(add(m3, m4))

  def singleLine(m: Matrix): List[Int] = {
    for x <- m
      yield x.head
  }

  println(singleLine(m3))

  def remCol(m: Matrix): Matrix = {
    for x <- m
      yield x.tail
  }

  println(remCol(m3))

  def transpose(m: Matrix): Matrix = {
    def trans(l: Matrix): Matrix = {
      l match
        case Nil :: _ => Nil
        case _ => l.map(_.head) :: trans(l.map(_.tail))
    }

    trans(m)
  }

  println(transpose(m3))

  type Img = List[List[Int]]

  val letterA = List(List(0, 0, 1, 0, 0), List(0, 1, 0, 1, 0), List(0, 1, 1, 1, 0), List(1, 0, 0, 0, 1), List(1, 0, 0, 0, 1))
  def show(m: Img): String = {
    
  }

  println(show(letterA))
}

