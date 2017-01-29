// prallel collection
(1 until 1000).par
  .filter(n => n % 3 == 0)
  .count(n => n.toString == n.toString.reverse)
//However, some operations are not parallelizable
def sum1(xs: Array[Int]): Int = {
  //... but it really does not execute parallel - becasue it depends from previous element/result !!!
  xs.par.foldLeft(0)(_ + _) // IDEA wants us to use sum :)
}

def sum2(xs: Array[Int]): Int = {
  //foldLeft, reduceLeft etc. is not parallel but... fold is :)
  xs.par.fold(0)(_ + _)
}
sum1(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
sum2(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))

def max(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)(math.max)
}

max(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))


/**
  * FOLD - to execute this the function should be:
  * - associative f(a, f(b,c)) == f(f(a,b),c)
  * - with zero element f(z,a) == f(a,z) == a
  * .....this called MONOID!!!
  *
  * f does not have to be commutative f(a,b) == f(b,a)
  *
  */
// paper, rock sissors
def play(a: String, b: String): String = List(a, b).sorted match {
  case List("paper", "scissors") => "scissors"
  case List("paper", "rock") => "paper"
  case List("rock", "scissors") => "rock"
  case List(x, y) if x == y => x
  case List("", y) => y
}
// could be scissors, paper
Array("paper", "rock", "paper", "scissors").par.fold("")(play)
Array("paper", "rock", "paper", "scissors").foldLeft("")(play)
Array("paper", "rock", "paper", "scissors").foldRight("")(play)
// depends how parallel would go
play(play("paper", "rock"), play("paper", "scissors")) == "scissors"
play("paper", play("rock", play("paper", "scissors"))) == "paper"

// checking associative - yep, - it is not associative
play("paper", play("rock", "scissors")) == play(play("paper", "rock"), "scissors")


/**
  * AGREGATE
  */

def isVowel(c: Char): Boolean = c == 'E' ||c == 'F'

//fold does not work - because it is based on f(A,A) =>A and this is f(Int, Char) => Int
//Array('E', 'P', 'F', 'L').par.fold(0)((count, c) =>  if (isVowel(c)) count + 1 else count)

//we an use foldLef, but it is not parallel
Array('E', 'P', 'F', 'L').par.foldLeft(0)((count, c) =>  if (isVowel(c)) count + 1 else count)

// that is why it is agregate!! - remember, z and second function must be MONOID 0 and +
Array('E', 'P', 'F', 'L').par.aggregate(0)(
  (count, c) => if (isVowel(c)) count + 1 else count,
  _ + _
)

// I don't know if count is parallel
Array('E', 'P', 'F', 'L').par.count(isVowel)
