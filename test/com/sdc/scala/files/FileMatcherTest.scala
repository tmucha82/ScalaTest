package com.sdc.scala.files

import java.io.File

import org.scalatest.FunSuite

class FileMatcherTest extends FunSuite {

  test("simple file matching: ends") {
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesEnding("iml"))
  }

  test("simple file matching: contains") {
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesContaining("ScalaTest"))
  }

  test("simple file matching: matches") {
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesRegex("Sc.*Test.*"))
  }
}
