package com.sdc.scala.unit

import org.scalatest.{FunSpec, FunSuite}

class AssertTest extends FunSuite {

  test("simple assertion with ==")({
    val test = 2
    assert(2 === test)
  })

  test("simple assertion with ===") {
    val test = 2
    assert(2 === test)
  }

  test("simple assertion with assertResult") {
    val test = 2
    assertResult(2)(test)
    assertResult(2){
      test
    }
    assertResult{
      2
    }{
      test
    }
  }

  test("simple assertion with intercept") {
    intercept[IllegalArgumentException] {
      throw new IllegalArgumentException
    }
  }
}

object AssertTest extends App {
  new ElementSpec().execute
}


class ElementSpec extends FunSpec {
  describe("a UniformedElement") {
    it("should have a width equal to the passed value") {
      val width = 2
      assert(width === 2)
    }

    it("should have a height equal to the passed value") {
      val height = 3
      assert(height === 3)
    }

    it("should throw an IAE if passed a negative width") {
      intercept[IllegalArgumentException] {
        throw new IllegalArgumentException
      }
    }
  }
}
