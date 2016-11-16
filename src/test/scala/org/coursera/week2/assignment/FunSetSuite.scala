package org.coursera.week2.assignment

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  * - run the "test" command in the SBT console
  * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    * - test
    * - ignore
    * - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val positiveNumbers = (x: Int) => x > 0
    val evenNumbers = (x: Int) => x % 2 == 0

    def getElements(set: Set) = {
      for (i <- -bound to bound if contains(set, i)) yield i
    }
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet test") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      val s1Elements = getElements(s1)
      assert(contains(s1, 1), "Singleton s1 should contain {1}")
      assert(s1Elements.size === 1, "Singleton s1 should contain only one element {1}")
      assert(s1Elements.head === 1, "Singleton s1 should contain only {1}")

      val s2Elements = getElements(s2)
      assert(contains(s2, 2), "Singleton s1 should contain {2}")
      assert(s2Elements.size === 1, "Singleton s1 should contain only one element {2}")
      assert(s2Elements.head === 2, "Singleton s1 should contain only {2}")

      val s3Elements = getElements(s3)
      assert(contains(s3, 3), "Singleton s1 should contain {3}")
      assert(s3Elements.size === 1, "Singleton s1 should contain only one element {3}")
      assert(s3Elements.head === 3, "Singleton s1 should contain only {3}")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")

      val set = union(s, s3)
      val setElements = getElements(set)
      assert(contains(set, 1), "Set should contain {1, 2, 3}")
      assert(contains(set, 2), "Set should contain {1, 2, 3}")
      assert(contains(set, 3), "Set should contain {1, 2, 3}")
      assert(setElements.size === 3, "Set should contain 3 elements {1, 2, 3}")
    }
  }

  test("intersect contains elements which are in both of each set") {
    new TestSets {
      val set = intersect(union(s1, s2), union(s2, s3))
      val setElements = getElements(set)
      assert(contains(set, 2), "Set should contain {2}")
      assert(setElements.size === 1, "Set should contain 1 element {2}")
      assert(setElements.head === 2, "Set should contain 1 element {2}")
    }
  }

  test("intersect contains all elements of `s` that are not in `t`.") {
    new TestSets {
      val set = diff(union(s1, s2), s2)
      val setElements = getElements(set)
      assert(contains(set, 1), "Set should contain {1}")
      assert(setElements.size === 1, "Set should contain 1 element {1}")
      assert(setElements.head === 1, "Set should contain 1 element {1}")
    }
  }

  test("filer of even number from positive numbers set") {
    new TestSets {
      val positiveEvenNumber = filter(positiveNumbers, x => x % 2 == 0)
      assert(!contains(positiveEvenNumber, 1), "Set should not contain {1}")
      assert(contains(positiveEvenNumber, 2), "Set should contain {2}")
      assert(!contains(positiveEvenNumber, -1), "Set should not contain {-1}")

    }
  }

  test("filer of negative numbers from positive numbers set - empty set") {
    new TestSets {
      val emptySet = filter(positiveNumbers, x => x < 0)
      val emptySetElements = getElements(emptySet)
      assert(emptySetElements.size === 0, "Set should contain no element")
    }
  }

  test("forall all numbers [0-9] modulo of 13 should return 0") {
    new TestSets {
      val digits = union(x => x < 10, x => x >= 0)
      assert(forall(digits, x => x % 13 == 0), "All numbers [0-9] modulo of 13 should return 0")
    }
  }

}
