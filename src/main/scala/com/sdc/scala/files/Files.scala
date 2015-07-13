package com.sdc.scala.files

import java.io.File

import scala.io.Source

object Files {

  def getFiles(path: String): Array[File] = new File(path).listFiles

  def fileLines(file: File) = Source.fromFile(file).getLines().toList

  def grep(files: Array[File], pattern: String): Map[String, String] = {
    var result: Map[String, String] = Map.empty
    for (file <- files
         if file.getName.endsWith("scala");
         line <- fileLines(file);
         trimmed = line.trim
         if trimmed.matches(pattern))
      result += file.getName -> trimmed
    result
  }

  def getLineLengthWithForSentenceForScalaFile(path: String): List[Int] = {
    (for {file <- getFiles(path)
          if file.getName.endsWith("sc")
          line <- fileLines(file)
          trimmed = line.trim
          if trimmed.matches(".*for.*")
    } yield trimmed.length).toList
  }


}
