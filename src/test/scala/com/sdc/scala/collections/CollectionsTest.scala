package com.sdc.scala.collections

import org.scalatest.FunSuite

import scala.collection.immutable.{Nil, Queue => ImmutableQueue, TreeMap, TreeSet}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Queue => MutableQueue}

class CollectionsTest extends FunSuite {

  test("List buffer") {
    var buffer = new ListBuffer[Int]
    assert(Nil === buffer)

    buffer += 1
    assert(ListBuffer(1) === buffer)
    buffer += 2
    assert(ListBuffer(1, 2) === buffer)

    buffer = 3 +: buffer
    assert(ListBuffer(3, 1, 2) === buffer)

    assert(List(3, 1, 2) === buffer.result())
    assert(3 === buffer.length)
    assert(ListBuffer(3) === buffer.take(1))
    assert(ListBuffer(3, 1) === buffer.take(2))
    assert(2 === buffer(2))
  }

  test("Array buffer") {
    var buffer = new ArrayBuffer[Int]

    buffer += 1
    assert(ArrayBuffer(1) === buffer)
    buffer += 2
    assert(ArrayBuffer(1, 2) === buffer)

    buffer = 3 +: buffer
    assert(ListBuffer(3, 1, 2) === buffer)

    assert(List(3, 1, 2) === buffer.result())
    assert(3 === buffer.length)
    assert(ListBuffer(3) === buffer.take(1))
    assert(ListBuffer(3, 1) === buffer.take(2))
    assert(2 === buffer(2))
  }

  test("Queue") {
    var immutableQueue = ImmutableQueue[Int]()
    assert(ImmutableQueue() === immutableQueue)

    immutableQueue = immutableQueue.enqueue(1)
    assert(ImmutableQueue(1) === immutableQueue)

    immutableQueue = immutableQueue.enqueue(List(2, 3))
    assert(ImmutableQueue(1, 2, 3) === immutableQueue)

    val (element, newImmutableQueue) = immutableQueue.dequeue
    assert(1 === element)
    assert(ImmutableQueue(2, 3) === newImmutableQueue)


    val mutableQueue = MutableQueue[Int]()
    assert(MutableQueue[Int]() === mutableQueue)

    mutableQueue += 1
    assert(MutableQueue(1) === mutableQueue)

    mutableQueue ++= List(2, 3)
    assert(MutableQueue(1, 2, 3) === mutableQueue)

    assert(1 === mutableQueue.dequeue)
    assert(MutableQueue(2, 3) === mutableQueue)
  }

  test("Stack") {
    val stack = mutable.Stack[Int]()
    stack.push(1)
    stack.push(2, 3, 4)
    assert(mutable.Stack(4, 3, 2, 1) === stack)
    assert(4 === stack.pop())
    assert(mutable.Stack(3, 2, 1) === stack)
    assert(3 === stack.top)
    assert(mutable.Stack(3, 2, 1) === stack)
  }

  test("String (via RichString)") {
    def hasUpperCase(string: String): Boolean = {
      //exists is not from String class but from Seq of chars - implicit conversion
      //you can go like:  string.exists((character:Char) => character.isUpper)
      //or like this: string.exists(character => character.isUpper)
      string.exists(_.isUpper)
    }
    assert(hasUpperCase("Tom Mucha"))
    assert(!hasUpperCase("tom mucha"))
  }

  test("Sets") {
    val text = "See Spot run. Run, Spot. Run!"
    val wordsList = text.split("[ !,.]+")
    assert(List("See", "Spot", "run", "Run", "Spot", "Run") === wordsList)

    val words = mutable.Set.empty[String]
    for (word <- wordsList) words += word.toLowerCase
    assert(Set("see", "spot", "run") === words)
    words += "test"
    assert(Set("see", "spot", "run", "test") === words)
    assert(Set("run", "test") === words.intersect(Set("test", "run", "sick")))

    var words2 = Set[String]()
    for (word <- wordsList) words2 += word.toLowerCase
    assert(Set("see", "spot", "run") === words2)
    words2 += "test"
    assert(Set("see", "spot", "run", "test") === words2)
    assert(Set("run", "test") === words.intersect(Set("test", "run", "sick")))
  }

  test("Maps") {
    val map = mutable.Map.empty[String, Int]
    map("hello") = 1
    map.put("word", 2)

    assert(Map("hello" -> 1, "word" -> 2) === map)
    assert(1 === map("hello"))
    assert(2 === map("word"))
    intercept[NoSuchElementException] {
      map("shit")
    }

    var map2 = Map[String, Int]()
    map2 += "hello" -> 1 //for immutable += is like a = a + x, not as method +=, fo mutable is method += !!!
    map2 += "word" -> 2

    assert(Map("hello" -> 1, "word" -> 2) === map2)
    assert(1 === map2("hello"))
    assert(2 === map2("word"))
    intercept[NoSuchElementException] {
      map2("shit")
    }


    def countWords(text: String): mutable.Map[String, Int] = {
      val map = mutable.Map.empty[String, Int]
      for {
        rawWord <- text.split("[ !,.]+")
        word = rawWord.toLowerCase
      }
        if (map.contains(word)) map(word) += 1
        else map(word) = 1
      map
    }
    val text = "See Spot run. Run, Spot. Run!"
    assert(1 === countWords(text)("see"))
    assert(2 === countWords(text)("spot"))
    assert(3 === countWords(text)("run"))
  }

  test("Sorted set") {
    val intSet = TreeSet(9, 3, 1, 8, 0, 2, 7, 4, 6, 5)
    assert(Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) == intSet)

    val charSet = TreeSet('f', 'u', 'a')
    assert(Set('a', 'f', 'u') === charSet)
  }

  test("Sorted map") {
    var tm = TreeMap(3 -> 'x', 1 -> 'x', 4 -> 'x')
    assert(Map(1 -> 'x', 3 -> 'x', 4 -> 'x') === tm)
    tm += (2 -> 'x')
    assert(Map(1 -> 'x', 2 -> 'x', 3 -> 'x', 4 -> 'x') === tm)
  }

  test("Synchronized set and maps") {
    //deprecated - use ConcurrentHashMap from java
    /*
        new mutable.HashMap[String, String] with mutable.SynchronizedMap {
          //we could override any method
          override def default(key: String): String = super.default(key)
        }
    */
    //Consider java.util.concurrent.ConcurrentHashMap
    //val synchronizedSet = new mutable.HashSet[Int] with mutable.SynchronizedSet[Int]
  }

  test("Initialize set") {
    val colors = List("blue", "yellow", "red", "green")
    val treeColors = TreeSet[String]() ++ colors
    assert(Set("blue", "green", "red", "yellow") === treeColors)
  }
}
