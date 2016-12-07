package com.sdc.scala.annotation

import org.scalatest.FunSuite

class AnnotationTest extends FunSuite {

  @TMucha
  def sampleMethod(): Unit = {
  }

  test("example") {
    assert("sampleMethod" === (for {
      method <- AnnotationTest.this.getClass.getMethods
      if method.getName.startsWith("sample")
      if method.getAnnotation(classOf[TMucha]) != null
    } yield method.getName).head)
  }
}
