package com.sdc.scala.person

import org.scalatest.FunSuite

class PersonTest extends FunSuite {

  test("person objects") {
    val robert = new Person("Robert", "Jones")
    val sally = new Person("Sally", "Smith")
    assert("Robert" === robert.firstName)
    assert("Jones" === robert.lastName)
    assert("Sally" === sally.firstName)
    assert("Smith" === sally.lastName)
  }


  class Parent {
    def method() = println("parent")
  }
  class Child extends Parent {
    override def method() = println("child")
  }

  class SuperChild extends Child {
    override def method() = println("super child")
  }

  test("upper bound") {
    def test[T <: Child](arg:T): Unit = {
      arg.method()
    }
    //test(new Parent)// won't compile
    test(new Child)
    test(new SuperChild)
  }
  test("lower bound") {
    def test[T >: Child](arg:T): Unit = {
      //no arg.method
    }
    test(new Parent)
    test(new Child)
    test(new SuperChild) //hmm
  }

  test("sort of the people") {
    val people = List(
      new Person("Larry", "Wall"),
      new Person("Anders", "Hejlsberg"),
      new Person("Guido", "van Rossum"),
      new Person("Alan", "Kay"),
      new Person("Yukihiro", "Matsumoto"))
    //Person.orderedMergeSort(testList) //won't compile since testList is not type of Ordered[T]
    val sortedList = Person.orderedMergeSort(people)
    val result = List(
      new Person("Anders", "Hejlsberg"),
      new Person("Alan", "Kay"),
      new Person("Yukihiro", "Matsumoto"),
      new Person("Guido", "van Rossum"),
      new Person("Larry", "Wall"))
    assert(result === sortedList)
  }

  test("won't compile for List[Int]") {
    //Int does not implement trait Ordered
    //val wontCompile = Person.orderedMergeSort(List[Int](3, 2, 1))
  }
}
