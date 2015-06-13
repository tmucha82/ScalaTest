package com.sdc.scala.files

import java.io.File

import org.scalatest.FunSuite

class FilesTest extends FunSuite {

  test("get files form dir") {
    val files = Files.getFiles("./scripts")
    assert(3 === files.length)
    assert("car.sc" === files(0).getName)
    assert("files.sc" === files(1).getName)
    assert("test.txt" === files(2).getName)
  }

  test("get lines from file") {
    val lines = Files.fileLines(new File("./scripts/car.sc"))
    assert(lines.head.contains("import"))
  }

  test("simple grep file") {
    val results = Files.grep(Array(new File("./scripts/car.sc")), "import .*")
    results.foreach(pair => {
      assert(pair._1.endsWith("scala"))
      assert(pair._2.contains("import "))
    })
  }

  test("get line length for scala file wth for sentence") {
    val results = Files.getLineLengthWithForSentenceForScalaFile("./scripts")
    assert(2 === results.length)
    assert(16 === results(0))
    assert(22 === results(1))
  }
}
