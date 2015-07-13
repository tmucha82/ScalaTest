package com.sdc.scala.element

import org.scalatest.FunSuite

class ElementTest extends FunSuite {

  test("element construct") {
    val element = new Element {
      override def contents: Array[String] = Array("1", "22", "333")
    }
    assert(3 === element.height)
    assert(1 === element.width)
  }

  test("element override by field") {
    val element = new Element {
      val contents: Array[String] = Array("1", "22", "333")
    }
    assert(3 === element.height)
    assert(1 === element.width)
    assert(Array("1", "22", "333") === element.contents)
  }

  test("all the same: parametric field") {
    class TestMethodElement(data: Array[String]) extends Element {
      override def contents: Array[String] = data
    }
    class TestFieldElement(data: Array[String]) extends Element {
      val contents: Array[String] = data
    }
    class TestParametricFieldElement(val contents: Array[String]) extends Element {
    }

    val contents: Array[String] = Array("1", "22", "333")
    val testMethodElement = new TestMethodElement(contents)
    val testFieldElement = new TestFieldElement(contents)
    val testParametricFieldElement = new TestParametricFieldElement(contents)
    assert(3 === testMethodElement.height)
    assert(1 === testMethodElement.width)
    assert(Array("1", "22", "333") === testMethodElement.contents)
    assert(3 === testFieldElement.height)
    assert(1 === testFieldElement.width)
    assert(Array("1", "22", "333") === testFieldElement.contents)
    assert(3 === testParametricFieldElement.height)
    assert(1 === testParametricFieldElement.width)
    assert(Array("1", "22", "333") === testParametricFieldElement.contents)
  }

  test("simple array element") {
    val arrayElement = new ArrayElement(Array("first", "second"))
    assert(2 === arrayElement.height)
    assert(5 === arrayElement.width)
    assert(Array("first", "second") === arrayElement.contents)
  }

  test("simple line element") {
    val lineElement = new LineInheritanceElement("test")
    assert(1 === lineElement.height)
    assert(4 === lineElement.width)
    assert(Array("test") === lineElement.contents)
  }

  test("simple uniform element") {
    val uniformElement = new UniformElement('x', 2, 5)
    assert(2 === uniformElement.height)
    assert(5 === uniformElement.width)
    assert(Array("xxxxx", "xxxxx") === uniformElement.contents)
  }

  def invokeDemo(element: Element): Unit = {
    element.demo()
  }

  test("demo method") {
    invokeDemo(new Element {
      override def contents: Array[String] = Nil.toArray
    })
    invokeDemo(new ArrayElement(Nil.toArray))
    invokeDemo(new LineInheritanceElement(""))
    invokeDemo(new LineCompositionElement(""))
    invokeDemo(new UniformElement('x', 2, 5))
  }

  test("composition or inheritance") {
    val lineInheritanceElement = new LineInheritanceElement("test")
    assert(1 === lineInheritanceElement.height)
    assert(4 === lineInheritanceElement.width)
    assert(Array("test") === lineInheritanceElement.contents)

    val lineCompositionElement = new LineCompositionElement("test")
    assert(1 === lineCompositionElement.height)
    assert(4 === lineCompositionElement.width)
    assert(Array("test") === lineCompositionElement.contents)
  }

  test("above method") {
    var result = new LineCompositionElement("hello") above new LineCompositionElement("world")
    assert(2 === result.height)
    assert(5 === result.width)
    assert(Array("hello", "world") === result.contents)

    result = new ArrayElement(Array("hello", "world")) above new LineCompositionElement("scala")
    assert(3 === result.height)
    assert(5 === result.width)
    assert(Array("hello", "world", "scala") === result.contents)

    result = new UniformElement('x', 2, 5) above new LineCompositionElement("scala")
    assert(3 === result.height)
    assert(5 === result.width)
    assert(Array("xxxxx", "xxxxx", "scala") === result.contents)
  }

  test("beside method") {
    var result = new LineCompositionElement("hello") beside new LineCompositionElement("world")
    assert(1 === result.height)
    assert(10 === result.width)
    assert(Array("helloworld") === result.contents)

    result = new LineCompositionElement("scala") beside new ArrayElement(Array("hello", "world"))
    assert(1 === result.height)
    assert(10 === result.width)
    assert(Array("scalahello") === result.contents)

    result = new UniformElement('x', 2, 5) beside new ArrayElement(Array("hello", "world"))
    assert(2 === result.height)
    assert(10 === result.width)
    assert(Array("xxxxxhello", "xxxxxworld") === result.contents)

  }

}
