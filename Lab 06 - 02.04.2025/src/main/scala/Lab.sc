type Str = List[Char]
type Email = Str

type Gradebook = List[(String,Int)] //the type Gradebook now refers to a list of pairs of String and Int
val gradebook = List(("George",3), ("Florica", 10), ("Maria",6), ("Paul",4), ("Aurel", 9), ("Teodora", 9))

// 6.1.1. Write a function which adds one point to
// all students which have a passing grade (>= 5),
// and leaves all other grades unchanged.
def increment(g: Gradebook): Gradebook =
  g.map((x: String, y: Int) => if (y >= 5 ) (x, y + 1) else (x, y))

increment(gradebook)

// 6.1.2. Find the average grade from a gradebook.
// You must use foldRight.
def average(g: Gradebook): Double =
{
  val sum = g.foldRight(0)((tuple: (String, Int), acc: Int) => tuple._2 + acc)
  sum.toDouble / g.length
}

average(gradebook)


//  6.1.3. Write a function which takes a gradebook and
//  returns the percentage of failed vs. passed students,
//  as a pair (x,y).
def percentage(g: Gradebook): (Double,Double) =
{
  val pass = g.foldRight(0)((tuple: (String, Int), acc: Int) => if(tuple._2 >= 5) 1 + acc else acc)
  val pass_percentage = (pass.toDouble / g.length) * 100
  (pass_percentage, 100 - pass_percentage)
}

percentage(gradebook)


//  6.1.4. Write a function which takes a gradebook and returns
//  the list of names which have passed. Use filter and map from Scala.
def pass(g: Gradebook): List[String] =
{
  // val passed = gradebook.filter((x: String, y: Int) => if(y >= 5) true else false)
  // get all the names of people who passed or change it to failed
  val passed = g.map((x: String, y: Int) => if (y >= 5) x else "failed")
  // filter so only non "failed" appears
  passed.filter(_ != "failed")
}

pass(gradebook)


//  6.1.5 Write a function which takes a gradebook and
//  reports all passing students in descending order of their grade.
def honorsList(g: Gradebook): List[String] =
{
  // sort ASCENDING based on grade then reverse it lol
  // then just get the passing students out
  pass(g.sortBy((x: String, y:Int) => y).reverse)
  pass(g.sortBy((x: String, y:Int) => 10-y))
}

honorsList(gradebook)


// 6.1.6. We extend the type Gradebook to:
type Name = String
type Lecture = String
type ExtGradebook = List[(Name,Lecture,Int)]
val egradebook = List(("John","FP",4), ("Maria", "DAP", 7), ("Ioana", "DIC", 3), ("John", "DIC", 2), ("Teodora", "FP", 7))
// the first string is the student name,
// the second is the course, and the final integer is the grade
// Write a function which reports all students that have failed at
// least one grade (each student will be reported once):
def atLeastOneFail(g: ExtGradebook): List[Name] =
{
  // all failed even duplicates
  val failed_with_dupes = g.map((x: String, y: String, z: Int) => if (z <= 5) x else "passed")
  failed_with_dupes.filter(_ != "passed").distinct
}

atLeastOneFail(egradebook)


//  6.2.1. Write a function which takes a list
//  of emails and extracts the prefix
//  (e.g. example@domain.com becomes example).
def getNames (l: List[Email]): List[Email] =
{
  l.map(x => x.takeWhile(_ != '@'))
}

val email = "example@domain.com".toList
val email2 = "two2@domain.com".toList
val email3 = "three3@domain3.ro".toList
getNames(List(email, email2, email3))


// 6.2.2. Write a function which filters out
// emails belonging to a specific Top-Level-Domain
// (e.g. those than end in com).
def removeTLD(l: List[Email], tld: Str): List[Email] =
{
  val list_with_empty = l.map(x => if(x.takeRight(4) != ".com".toList) x else Nil)
  list_with_empty.filter(_ != Nil)
}

val tld = ".com".toList
removeTLD(List(email, email2, email3), tld)


// 6.2.3. Write a function which checks
// if there exist identical names under
// different domains in a list of emails (e.g. ana@amazon.com and ana@gmail.com)
def containsDuplicates(l: List[Email]): Boolean =
{
  val names = getNames(l)
  if(names != names.distinct) true
  else false
}

val email4 = "three3@domain3.tld".toList
containsDuplicates(List(email, email2, email3, email4))
containsDuplicates(List(email, email2, email3))

// 6.2.4. Write a function which reports
// the number of duplicates of names under
// different domains in a list of emails.
// (e.g. [“ana@aol.com”, “ana@aol.ro”, “ana@amazon.com”,
// “jim@cx.com”, “mary@mail.com” , “mary@mail.ro”]
// will produce: 2 since there are two duplication
// instances: for ana and for mary.
def countDuplicates(l: List[Email]): Int =
{
  val distinct_emails = l.distinct
  val names = getNames(distinct_emails)
  names.length - names.distinct.length
}

val mail1 = "ana@aol.com".toList
