package com.sdc.scala.files

import java.io.File

object FileMatcher {
  val filesHere: Array[File] = new File(".").listFiles()

  def printFiles() = filesHere.foreach(println(_))

  def filesEnding(query: String): Array[File] = {
    for (file <- filesHere; if file.getName.endsWith(query)) yield file
  }

  def filesContaining(query: String): Array[File] = {
    for {file <- filesHere
         if file.getName.contains(query)
    } yield file
  }

  def filesRegex(query: String): Array[File] = {
    for (file <- filesHere; if file.getName.matches(query)) yield file
  }

  def filesMatchingOld(query: String, matcher: (String, String) => Boolean): Array[File] = {
    for (file <- filesHere; if matcher(file.getName, query)) yield file
  }

  def filesMatching(query: String, matcher: (String) => Boolean): Array[File] = {
    for (file <- filesHere; if matcher(file.getName)) yield file
  }
}
