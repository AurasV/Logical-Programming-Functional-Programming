import scala.annotation.tailrec

abstract class WTree extends WTreeInterface {
  override def filter(pred: Token => Boolean): WTree = filterAux(pred, Empty)
  def filterAux(pred: Token => Boolean, acc: WTree): WTree

}

case object Empty extends WTree {
  override def balance: Int = 0
  override def height: Int = 0
  override def rotateLeft: WTree = this
  override def rotateRight: WTree = this
  override def rotateRightLeft: WTree = this
  override def rotateLeftRight: WTree = this
  override def rebalance: WTree = this

  override def isEmpty = true
  override def ins(w: Token): WTree = Node(w, Empty, Empty)
  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = acc
  override def size: Int = 0
  override def contains(s: String): Boolean = false

}

case class Node(word: Token, left: WTree, right: WTree) extends WTree {

  override def balance: Int = right.height - left.height
  override def height: Int = 1 + (left.height max right.height)
  override def rotateLeft: WTree =
    right match {
      // the tree is unbalanced, hence the right sub-tree is nonempty
      case Node(w, l, r) => Node(w, Node(word, left, l), r)
    }
  override def rotateRight: WTree =
    left match {
      case Node(w, l, r) => Node(w, l, Node(word, r, right))
    }
  override def rotateRightLeft: WTree =
    Node(word, left, right.rotateRight).rotateLeft
  override def rotateLeftRight: WTree =
    Node(word, left.rotateLeft, right).rotateRight
  override def rebalance: WTree = {
    if (balance < -1 && left.balance == -1) this.rotateRight
    else if (balance > 1 && right.balance == 1) this.rotateLeft
    else if (balance < -1 && left.balance == 1) this.rotateLeftRight
    else if (balance > 1 && right.balance == -1) this.rotateRightLeft
    else this
  }

  override def isEmpty = false

  override def ins(w: Token): WTree =
    if (w.freq > word.freq) Node(word, left, right.ins(w))
    else Node(word, left.ins(w), right)

  override def contains(s: String): Boolean = {
    // check if we found the word
    // else check both sides recursivly
    if (s == word.word) {
      true
    } else {
      left.contains(s) || right.contains(s)
    }
  }

  override def size: Int = {
    def aux(t: WTree): Int =
      t match {
      case Empty => 0
      case Node(_, l, r) => 1 + aux(l) + aux(r)
    }

    aux(this)
  }

  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = {
    val newAcc = if (pred(word)) Node(word, Empty, Empty) else Empty

    val leftFiltered = left.filterAux(pred, Empty)
    val rightFiltered = right.filterAux(pred, Empty)

    (leftFiltered, rightFiltered) match {
      case (Empty, Empty) => newAcc
      case (_, Empty) => Node(word, leftFiltered, Empty)
      case (Empty, _) => Node(word, Empty, rightFiltered)
      case _ => Node(word, leftFiltered, rightFiltered)
    }
  }
}


object Main {

  def tokenID:Int = 861679

  val scalaDescription: String = "Scala is a strong statically typed general-purpose programming language which supports both object-oriented programming and functional programming designed to be concise many of Scala s design decisions are aimed to address criticisms of Java Scala source code can be compiled to Java bytecode and run on a Java virtual machine. Scala provides language interoperability with Java so that libraries written in either language may be referenced directly in Scala or Java code like Java, Scala is object-oriented, and uses a syntax termed curly-brace which is similar to the language C since Scala 3 there is also an option to use the off-side rule to structure blocks and its use is advised martin odersky has said that this turned out to be the most productive change introduced in Scala 3 unlike Java, Scala has many features of functional programming languages like Scheme, Standard ML, and Haskell, including currying, immutability, lazy evaluation, and pattern matching it also has an advanced type system supporting algebraic data types, covariance and contravariance, higher-order types (but not higher-rank types), and anonymous types other features of Scala not present in Java include operator overloading optional parameters named parameters and raw strings conversely a feature of Java not in Scala is checked exceptions which has proved controversial"

  /* Split the text into chunks */
  def split(text: List[Char]): List[List[Char]] = {
    def aux(text: List[Char]): List[List[Char]] = text match {
      // empty text
      case Nil => Nil
      // have space, we move on to the rest of the phrase
      case ' ' :: rest => aux(rest)
      // if it's not a space we separate what we have in the word and the rest up until next space
      // takewhile takes stuff up until space
      // dropwhile drops stuff up until space (so it takes everything else)
      // then we add the word to the list and move on to the rest
      case _ =>
        val word = text.takeWhile(_ != ' ')
        val rest = text.dropWhile(_ != ' ')
        word :: aux(rest)
    }

    val l = aux(text)
    if (l == List(Nil)) Nil
    else l
  }

  /* compute the frequency of each chunk */
  def computeTokens(words: List[String]): List[Token] = {
    /* insert a new string in a list of tokens */
    // empty list we add the first word we got
    // check if we have the word, no => add to list
    // if yew => increase freq by 1
    def insWord(s: String, acc: List[Token]): List[Token] =
      acc match {
      case Nil => List(Token(s, 1))
      case Token(word, frequency) :: rest =>
        if (word == s) Token(word, frequency + 1) :: rest
        else Token(word, frequency) :: insWord(s, rest)

    }

    /* tail-recursive implementation of the list of tokens */
    // check if any words are left
    // or we take the first word and add it to the accumulator
    @tailrec
    def aux(rest: List[String], acc: List[Token]): List[Token] =
      rest match {
      case Nil => acc
      case head :: tail => aux(tail, insWord(head, acc))
    }

    val l = aux(words, Nil)
    l

  }

  def tokensToTree(tokens: List[Token]): WTree = {
    tokens.foldLeft(Empty: WTree)((tree, token) => tree.ins(token))
  }

  /* Using the previous function, which builds a tree from a list of tokens,
  *  write a function which takes a string,
  *  splits it into chunks, computes frequencies and constructs a tree.
  *  Use the function _.toList to construct a list of characters from a String.
  *
  *  A much cleaner implementation can be achieved by "sequencing" functions using
  *  andThen.
  * */

  def makeTree(s: String): WTree = {
    // string => list of chars
    val charList = s.toList

    // string => chunks using split on map + mkString
    val words = split(charList).map(_.mkString)

    // fequency of tokens
    val tokens = computeTokens(words)

    // Step 4: Convert the list of tokens into a tree using tokensToTree
    tokensToTree(tokens)
  }


  /* build a tree with the words and frequencies from the text in the scalaDescription text */
  def wordSet: WTree = ???

  /* find the number of occurrences of the keyword "Scala" in the scalaDescription text */
  def scalaFreq: Int = {
    // make tree with the description
    val tree = makeTree(scalaDescription)

    // search in tree, it's nodes, we take token and left subtreee and right subtree
    // check if word is Scala and add to count, only one will have it
    def countScala(tree: WTree): Int = {
      tree match {
        case Empty => 0
        case Node(word, left, right) =>
          val count = if (word.word == "Scala") word.freq else 0
          count + countScala(left) + countScala(right)
      }
    }

    countScala(tree)
  }

  /* find how many programming languages are referenced in the text.
     A PL is a keyword which starts with an uppercase
     You can reference a character from a string using (0) and you can
     also use the function isUpper

  */
  def progLang: Int = {
    // text to words map, mkstring
    val words = split(scalaDescription.toList).map(_.mkString)

    // filter uppercase non empty and count
    val count = words.filter(word => word.nonEmpty && word(0).isUpper).toSet.size

    count
  }




  /* find how many words which are not prepositions or conjunctions appear in the text (any word whose size is larger than 3). */

  def wordCount: Int = {
    // text to words map, mkstring
    val words = split(scalaDescription.toList).map(_.mkString)

    // filte words > 3
    val filteredWords = words.filter(_.length > 3)

    // no dupes
    filteredWords.size
  }

}

