package com.sdc.scala

import com.sdc.scala.expressions._
import org.scalatest.FunSuite

class CommonTest extends FunSuite {
  test("simple Hello World with one argument") {
    //could be this
    println("Hello, world !")

    //or could be that
    println {
      "Hello, world !"
    }
  }

  test("simple increase function ") {
    def increase = {
      println("increase method")
      (x: Int) => x + 1
    }
    assert(11 === increase(10))
  }

  test("simple increase placeholder function ") {
    def increasePlaceholder = (_: Int) + 1
    assert(11 === increasePlaceholder(10))
  }

  test("simple for each") {
    val testList = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val result = testList.filter((x: Int) => x % 2 == 0)
    assert(List(2, 4, 6, 8) === result)
  }

  test("simple for each - short form") {
    val testList = List(-2, 24, 3, -2, -4, 5, 0)
    val result = testList.filter(x => x > 0)
    assert(List(24, 3, 5) === result)
  }

  test("placeholder simple test") {
    val testList = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val result = testList.filter(_ % 2 == 0)
    assert(List(0, 2, 4, 6, 8, 10) === result)
  }

  test("placeholder add test") {
    def add = (_: Int) + (_: Int)
    assert(7 === add(3, 4))
  }
  test("println list in different ways") {
    val testList = List(1, 2, 3, 4, 5)
    println(testList)
    testList foreach print
    testList.foreach(print)
    testList.foreach((x: Int) => print(x))
    testList.foreach(x => print(x))
    testList.foreach(print(_))
    for (x <- testList) print(x)
    println()
  }

  test("partially applied functions") {
    def sum(a: Int, b: Int, c: Int) = a + b + c

    val a = sum _
    assert(6 === sum(1, 2, 3))
    assert(6 === a(1, 2, 3))
    assert(6 === a.apply(1, 2, 3))


    val b = sum(1, _: Int, 3)
    assert(6 === b(2))
    assert(6 === b.apply(2))
  }

  test("sum of the list") {
    val list = List(-10, 8, 4, 0, -1, 3, 8)

    var sum = 0
    list.foreach(x => sum += x)
    assert(12 === sum)

    sum = 0
    list.foreach(sum += _)
    assert(12 === sum)
  }

  test("closure increase") {
    def increase(more: Int) = (x: Int) => x + more
    val increaseOne = increase(1)
    val increaseTen = increase(10)
    assert(4 === increaseOne(3))
    assert(13 === increaseTen(3))
  }

  test("repeated parameter") {
    def echo1(args: Array[String]) = for (arg <- args) print(arg + " ")
    echo1(Array("1", "2"))
    println()

    //but we could do like this
    def echo2(args: String*) = for (arg <- args) print(arg + " ")
    echo2("1", "2")
    println()

    val arr = Array("What's", "up", "doc?")
    echo1(arr)
    echo2("What's", "up", "doc?")
    echo2(arr: _*)
  }

  def containsNegativeFirst(numbers: List[Int]): Boolean = {
    var exist = false
    for (number <- numbers)
      if (number < 0) exist = true
    exist
  }

  def containsNegativeSecond(numbers: List[Int]): Boolean = {
    //numbers.exists(p => p > 0)//we could such way or ..
    numbers.exists(_ < 0)
  }

  test("if list contains negative number") {
    //containsNegativeFirst
    assert(true === containsNegativeFirst(List(-1)))
    assert(false === containsNegativeFirst(List(0)))
    assert(false === containsNegativeFirst(List()))
    assert(true === containsNegativeFirst(List(1, 2, -3, 4, 5)))
    assert(false === containsNegativeFirst(List(1, 2, 3, 4, 5)))


    assert(true === containsNegativeSecond(List(-1)))
    assert(false === containsNegativeSecond(List(0)))
    assert(false === containsNegativeSecond(List()))
    assert(true === containsNegativeSecond(List(1, 2, -3, 4, 5)))
    assert(false === containsNegativeSecond(List(1, 2, 3, 4, 5)))
  }

  def containsOddNumber(numbers: List[Int]): Boolean = {
    numbers.exists(_ % 2 == 1)
  }

  test("if list contains odd number") {
    assert(false === containsOddNumber(Nil))
    assert(false === containsOddNumber(List(0)))
    assert(true === containsOddNumber(List(1, 2)))
    assert(false === containsOddNumber(List(2, 4)))
  }

  test("currying methods") {
    def plainOldSum(x: Int, y: Int): Int = x + y
    def curriedSum(x: Int)(y: Int): Int = x + y

    //not - curried
    assert(4 === plainOldSum(1, 3))
    assert(10 === plainOldSum(2, 8))

    //curried
    assert(4 === curriedSum(1)(3))
    assert(10 === curriedSum(2)(8))

    //get first curried function
    val onePlus = curriedSum(1) _
    assert(4 === onePlus(3))
    assert(10 === onePlus(9))
  }


  test("twice") {
    def twice(method: Double => Double, x: Double) = method(method(x))

    assert(7 === twice(_ + 1, 5))
    assert(12 === twice(_ * 2, 3))
    assert(81 === twice(Math.pow(_, 2), 3))
  }

  var enabledBoolAssert = false

  def boolAssert(predicate: Boolean) = {
    if (enabledBoolAssert && !predicate)
      throw new AssertionError
  }

  var enabledByNameAssert = false

  def byNameAssert(predicate: => Boolean) = {
    if (enabledByNameAssert && !predicate)
      throw new AssertionError
  }

  test("myAssert in action") {
    intercept[ArithmeticException] {
      boolAssert(3 / 0 == 0)
    }
    byNameAssert(3 / 0 == 0)
  }

  test("zip two arrays") {
    assert(Array(("1", 1), ("2", 2)) === (Array("1", "2", "3") zip Array(1, 2)))
  }

  test("ensuring of simple test") {
    {
      Math.abs(9)
    } ensuring (result => result >= 0)
  }

  test("ensuring of simple test with placeholder") {
    {
      Math.abs(9)
    } ensuring (_ >= 0)
  }

  test("pattern matching: variable and constant") {
    import Math.{E, PI}
    assert(E match {
      case PI => false
      case _ => true // would be this
    })

    /*
        val pi = PI
        assert(E match {
          case pi => true
          case _ => false //unreachable code
        })
    */
    val pi = PI
    assert(E match {
      case `pi` => false
      case _ => true
    })
  }

  test("pattern matching: sequence") {
    assert(List(0, 1, 2, 3) match {
      case List(0, _, _, _) => true
    })

    assert(List(0, 1, 2, 3, 4, 5) match {
      case List(0, _*) => true
    })

    assert((0, "hi", true) match {
      case (a, b, c) =>
        println("matched: " + a + b + c)
        true
      case _ => false
    })
  }

  test("pattern matching: typed") {
    def getSize(x: Any): Int = {
      x match {
        case x: String => x.length
        case map: Map[_, _] => map.size
        case _ => -1
      }
    }

    /*
        def getSizeByInstanceOf(x: Any): Int = {//sic ! - more long-winded than match
          if(x.isInstanceOf[String])
            x.asInstanceOf[String].length
          else if(x.isInstanceOf[Map])
            x.asInstanceOf[Map].size
          else
            -1
        }
    */
    assert(-1 === getSize(UnaryOperator("-", Variable("x"))))
    assert(4 === getSize("test"))
    assert(2 === getSize(Map(1 -> "test", 2 -> "value")))
    assert(2 === getSize(Map((1, "test"), (2, "value"))))
  }

  test("pattern matching: typed map") {
    def test(x: Any): Boolean = {
      x match {
        //        case map: Map[Int, String] => false //no difference between Map[Int, String] and Map[String, String] in runtime
        //        case map: Map[Int, Int] => false
        case map: Map[_, _] => true
        case _ => false
      }
    }
    assert(test(Map(1 -> "a")))
    assert(test(Map(1 -> 1)))
  }

  test("pattern matching: typed arrays - different than map") {
    def isStringArray(x: Any): Boolean = {
      x match {
        case array: Array[String] => true
        case array: Array[Int] => false
        case _ => false
      }
    }

    assert(isStringArray(Array(1, 2, 3)) === false)
    assert(isStringArray(Array("1", "2", "3")))
  }

  test("pattern matching: variable binding") {
    def test(expression: Expression): String = {
      expression match {
        case UnaryOperator("-", UnaryOperator(_, variable@Variable(_))) => variable.name
        case _ => null
      }
    }

    assert(test(UnaryOperator("-", UnaryOperator("-", Variable("a")))) === "a")
    assert(test(UnaryOperator("-", UnaryOperator("+", Variable("b")))) === "b")
  }

  test("patterns in variable definitions") {
    val template = (1, "abc")
    val (number, text) = template

    assert(1 === number)
    assert("abc" === text)

    val templateBinaryOperator = BinaryOperator("+", Variable("a"), Variable("b"))
    val BinaryOperator(operator, right, left) = templateBinaryOperator
    assert(operator === "+")
    assert(right === Variable("a"))
    assert(left === Variable("b"))
  }

  test("case sequences as partial functions") {
    val multiply = (a: Int) => 3 * a //declare of function - memory
    assert(6 === multiply(2))

    val withDefault: Option[Int] => Int = {
      case Some(x) => x
      case None => 0
    }
    assert(3 === withDefault(Some(3)))
    assert(0 === withDefault(None))

    val second: List[Int] => Int = {
      case List(_, sec, _*) => sec //this
      //      case x :: sec :: _ => sec //or this
    }
    assert(2 === second(List(1, 2, 3, 4, 5, 6, 7, 8, 9)))

    val secondImproved: PartialFunction[List[Int], Int] = {
      case List(_, sec, _*) => sec //this
      //      case x :: sec :: _ => sec //or this
    }
    assert(secondImproved.isDefinedAt(List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
    assert(secondImproved.isDefinedAt(List()) === false)

    assert(2 === secondImproved.applyOrElse(List(1, 2, 3, 4, 5, 6, 7, 8, 9), (a: List[Int]) => 3))
    assert(3 === secondImproved.applyOrElse(List(), (a: List[Int]) => 3))
  }

  test("create new partial function") {
    val second = new PartialFunction[List[Int], Int] {
      override def isDefinedAt(x: List[Int]): Boolean = x match {
        case list:List[Int] if list.size >= 2 => true
        case _ => false
      }

      override def apply(v1: List[Int]): Int = v1 match {
        case List(_, sec, _*) => sec
      }
    }
    assert(2 === second.applyOrElse(List(1,2,3,4), (a: List[Int]) => -1))
    assert(-1 === second.applyOrElse(List(), (a: List[Int]) => -1))
  }

  test("instance Of and cast to type - more long-winded than match") {
    val test:Any = "test"
    assert(test.isInstanceOf[String])
    assert(test.asInstanceOf[String].isInstanceOf[String])
  }

  test("pattern guards") {
    def simplifyAdd(expression: Expression): Expression = {
      expression match {
        case BinaryOperator("+", x, y) if x == y => BinaryOperator("*", x, Number(2))
        case _ => expression
      }
    }
    assert(BinaryOperator("*", Variable("a"), Number(2)) === simplifyAdd(BinaryOperator("+", Variable("a"), Variable("a"))))
    assert(BinaryOperator("+", Variable("a"), Variable("b")) === simplifyAdd(BinaryOperator("+", Variable("a"), Variable("b"))))

    def test(x: Any): Any = {
      x match {
        case a: Int if a > 0 => a
        case b: String if b(0) == 'a' => b
        case _ => throw new IllegalArgumentException("wrong")
      }
    }
    assert(3 === test(3))
    assert("abc" === test("abc"))
  }

  test("sealed case class") {
    def describe(expression: Expression): String = (expression: @unchecked) match {
      //if not all the options
      case Number(_) => "number"
      case Variable(_) => "variable"
    }
    assert("number" === describe(Number(3)))
    assert("variable" === describe(Variable("3")))
  }

  test("option type") {
    val capitols = Map("France" -> "Paris", "Poland" -> "Warsaw")
    assert(Some("Paris") === capitols.get("France"))
    assert(Some("Warsaw") === capitols.get("Poland"))
    assert(None === capitols.get("Russia"))
    assert("Sorry" === capitols.getOrElse("Russia", "Sorry"))
  }

  test("patterns in for expressions") {
    val capitols = Map("France" -> "Paris", "Poland" -> "Warsaw")
    val cities = for((country, city) <- capitols) yield {
      println("The capitol of " + country + " is " + city + ".")
      city
    }
    assert(Array("Paris", "Warsaw") === cities)
  }

  test("patterns in for expressions tha is matched") {
    val fruits = List(Some("apple"), None, Some("orange"))
    val realFruit = for (Some(fruit) <- fruits) yield fruit
    assert(Array("apple", "orange") === realFruit)
  }
}
