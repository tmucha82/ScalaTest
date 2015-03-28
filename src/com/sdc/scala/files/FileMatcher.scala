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
}
