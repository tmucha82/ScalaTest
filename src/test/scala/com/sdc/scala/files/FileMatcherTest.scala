package com.sdc.scala.files

import java.io.File
import java.util.Date

import org.scalatest.FunSuite

class FileMatcherTest extends FunSuite {

  test("simple file matching: ends") {
    assert(Array(new File("./build.sbt")) === FileMatcher.filesEnding("sbt"))
  }

  test("simple file matching: contains") {
    assert(Array(new File("./build.sbt")) === FileMatcher.filesContaining("build"))
  }

  test("simple file matching: regexp") {
    assert(Array(new File("./build.sbt")) === FileMatcher.filesRegex("bu.*l.*"))
  }

  test("simple file matching: first matches") {
    assert(Array(new File("./build.sbt")) === FileMatcher.filesMatchingOld("sbt", (name: String, query: String) => name.endsWith(query)))
    assert(Array(new File("./build.sbt")) === FileMatcher.filesMatchingOld("build", (name, query) => name.contains(query)))
    assert(Array(new File("./build.sbt")) === FileMatcher.filesMatchingOld("bu.*l.*", _.matches(_)))
  }

  test("simple file matching: second matches") {
    val query1 = "sbt"
    assert(Array(new File("./build.sbt")) === FileMatcher.filesMatching(query1, (name: String) => name.endsWith(query1)))

    val query2 = "build"
    assert(Array(new File("./build.sbt")) === FileMatcher.filesMatching(query2, (name) => name.contains(query2)))

    val query3 = "bu.*l.*"
    assert(Array(new File("./build.sbt")) === FileMatcher.filesMatching(query3, _.matches(query3)))
  }

  test("withPrintWriter write file") {
    val filePath = "./date.txt"
    FileMatcher.withPrintWriter(new File(filePath))(writer => writer.println(new Date()))
    new File(filePath).delete()
  }

  test("withPrintWriter write file with extra {}") {
    val filePath = "./date.txt"
    FileMatcher.withPrintWriter(new File(filePath)) {
      writer => writer.println(new Date())
    }
    new File(filePath).delete()
  }

}
