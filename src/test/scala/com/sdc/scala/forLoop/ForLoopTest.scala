package com.sdc.scala.forLoop

import org.scalatest.FunSuite

class ForLoopTest extends FunSuite {

  case class Person(name: String, isMale: Boolean, children: Person*)

  val lara = Person("Lara", isMale = false)
  val bob = Person("Bob", isMale = true)
  val julie = Person("Julie", false, lara, bob)
  val tom = Person("Tom", true, bob)
  val persons = List(lara, bob, julie, tom)

  test("simple test") {
    val women = persons filter (p => !p.isMale)
    assert(List(lara, julie) === women)
    //you could find mother with child like that....
    var motherWithChild = persons filter (p => !p.isMale) flatMap (p => p.children map (c => (p.name, c.name)))
    assert(List(("Julie", "Lara"), ("Julie", "Bob")) === motherWithChild)

    //...or. Interestingly compiler will translate the second query into the first one
    motherWithChild = for (person <- persons; if !person.isMale; child <- person.children) yield (person.name, child.name)
    assert(List(("Julie", "Lara"), ("Julie", "Bob")) === motherWithChild)
  }

  test("for expression both ways") {
    //generators, definitions and filters
    var toNames = for (p <- persons; n = p.name; if n startsWith "To") yield n
    assert(List("Tom") === toNames)

    toNames = for {
      p <- persons //generator
      n = p.name //definition
      if n startsWith "To" //filter
    } yield n
    assert(List("Tom") === toNames)


    val testList = for (x <- List(1, 2); y <- List("one", "two")) yield (x, y)
    assert(testList === List((1, "one"), (1, "two"), (2, "one"), (2, "two")))
  }

  test("N queens problem") {

    def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) =
      queens forall (q => !inCheck(queen, q))

    def inCheck(q1: (Int, Int), q2: (Int, Int)) =
      q1._1 == q2._1 || // same row
        q1._2 == q2._2 || // same column
        (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal

    def queens(n: Int): List[List[(Int, Int)]] = {
      def placeQueens(k: Int): List[List[(Int, Int)]] =
        if (k == 0)
          List(List())
        else
          for {
            queens <- placeQueens(k - 1)
            column <- 1 to n
            queen = (k, column)
            if isSafe(queen, queens)
          } yield queen :: queens

      placeQueens(n)
    }

    assert(List(List()) === queens(0))
    assert(List(List((1, 1))) === queens(1))
    assert(List() === queens(2))
    assert(List() === queens(3))
    assert(List(List((4, 3), (3, 1), (2, 4), (1, 2)), List((4, 2), (3, 4), (2, 1), (1, 3))) === queens(4))
    assert(List(List((5, 4), (4, 2), (3, 5), (2, 3), (1, 1)), List((5, 3), (4, 5), (3, 2), (2, 4), (1, 1)), List((5, 5), (4, 3), (3, 1), (2, 4), (1, 2)), List((5, 4), (4, 1), (3, 3), (2, 5), (1, 2)), List((5, 5), (4, 2), (3, 4), (2, 1), (1, 3)), List((5, 1), (4, 4), (3, 2), (2, 5), (1, 3)), List((5, 2), (4, 5), (3, 3), (2, 1), (1, 4)), List((5, 1), (4, 3), (3, 5), (2, 2), (1, 4)), List((5, 3), (4, 1), (3, 4), (2, 2), (1, 5)), List((5, 2), (4, 4), (3, 1), (2, 3), (1, 5))) === queens(5))
  }

  case class Book(title: String, authors: String*)

  val books: List[Book] =
    List(
      Book(
        "Structure and Interpretation of Computer Programs",
        "Abelson, Harold", "Sussman, Gerald J."
      ),
      Book(
        "Principles of Compiler Design",
        "Aho, Alfred", "Ullman, Jeffrey"
      ),
      Book(
        "Programming in Modula-2",
        "Wirth, Niklaus"
      ),
      Book(
        "Elements of ML Programming",
        "Ullman, Jeffrey"
      ),
      Book(
        "The Java Language Specification", "Gosling, James",
        "Joy, Bill", "Steele, Guy", "Bracha, Gilad"
      )
    )

  test("querying with for expressions") {
    //then, to find the titles of all books whose author's last name is "Gosling":
    val goslingBooks = books.filter(book => {
      book.authors.exists(author => author.startsWith("Gosling"))
    }).map(book => book.title)
    assert(List("The Java Language Specification") === goslingBooks)

    val goslingBookTitles = for (book <- books; author <- book.authors; if author.startsWith("Gosling")) yield book.title
    assert(List("The Java Language Specification") === goslingBookTitles)

    //Or, to find the titles of all books that have the string "Program" in their title:
    val programTitles = for (book <- books; title = book.title if title.contains("Program")) yield title
    assert(List("Structure and Interpretation of Computer Programs", "Programming in Modula-2", "Elements of ML Programming") === programTitles)

    val resultOfFor = for (b1 <- books; b2 <- books if b1 != b2; a1 <- b1.authors; a2 <- b2.authors if a1 == a2) yield a1
    val resultOfMethods = books flatMap (b1 => books filter (b2 => b1 != b2) flatMap (b2 => b1.authors flatMap (a1 => b2.authors filter (a2 => a1 == a2) map (a2 => a1))))
    assert(resultOfFor === resultOfMethods)
  }

  test("demo of for translations") {
    object Demo {
      def map[A, B](xs: List[A], f: A => B): List[B] =
        for (x <- xs) yield f(x)

      def flatMap[A, B](xs: List[A], f: A => List[B]): List[B] =
        for (x <- xs; y <- f(x)) yield y

      def filter[A](xs: List[A], p: A => Boolean): List[A] =
        for (x <- xs if p(x)) yield x
    }
  }
}
