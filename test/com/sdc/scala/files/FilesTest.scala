package com.sdc.scala.files

import java.io.File

import org.scalatest.FunSuite

class FilesTest extends FunSuite {

  test("get files form dir") {
    val files = Files.getFiles("./scripts")
    assert(3 === files.length)
    assert("car.script.scala" === files(0).getName)
    assert("files.script.scala" === files(1).getName)
    assert("test.txt" === files(2).getName)
  }

  test("get lines from file") {
    val lines = Files.fileLines(new File("./scripts/car.script.scala"))
    assert(lines.head.contains("import"))
  }

  test("simple grep file") {
    val results = Files.grep(Array(new File("./scripts/car.script.scala")), "import .*")
    results.foreach(pair => {
      assert(pair._1.endsWith("scala"))
      assert(pair._2.contains("import "))
    })
  }
}
