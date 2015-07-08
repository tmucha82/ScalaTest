package com.sdc.scala.files

import java.io.File
import java.util.Date

import org.scalatest.FunSuite

class FileMatcherTest extends FunSuite {

  test("simple file matching: ends") {
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesEnding("iml"))
  }

  test("simple file matching: contains") {
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesContaining("ScalaTest"))
  }

  test("simple file matching: regexp") {
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesRegex("Sc.*Test.*"))
  }

  test("simple file matching: first matches") {
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesMatchingOld("iml", (name: String, query: String) => name.endsWith(query)))
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesMatchingOld("ScalaTest", (name, query) => name.contains(query)))
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesMatchingOld("Sc.*Test.*", _.matches(_)))
  }

  test("simple file matching: second matches") {
    val query1 = "iml"
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesMatching(query1, (name: String) => name.endsWith(query1)))

    val query2 = "ScalaTest"
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesMatching(query2, (name) => name.contains(query2)))

    val query3 = "Sc.*Test.*"
    assert(Array(new File("./ScalaTest.iml")) === FileMatcher.filesMatching(query3, _.matches(query3)))
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
