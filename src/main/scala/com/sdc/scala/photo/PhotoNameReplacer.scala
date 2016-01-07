package com.sdc.scala.photo

import java.io.File
import java.text.SimpleDateFormat
import java.util.regex.Pattern

object PhotoNameReplacer extends App {
  val photoDirectoryPath = "/Users/tomaszmucha/Pictures/Alusia/Początki 2015"
  val regExpInputFile = Pattern.compile("WP_(\\d{8}_\\d{2}_\\d{2}_\\d{2})_Pro\\.(.*)")

  val photoDirectory = new File(photoDirectoryPath)
  if(photoDirectory.exists && photoDirectory.isDirectory)
    for(file <- photoDirectory.listFiles) {
      val matcher = regExpInputFile.matcher(file.getName)
      if(matcher.matches()) {
        val date = new SimpleDateFormat("yyyyMMdd_HH_mm_ss").parse(matcher.group(1))
        val newFormat = new SimpleDateFormat("yyyy-MM-dd HH.mm.ss").format(date)
        try {
          file.renameTo(new File(photoDirectoryPath + "/" + newFormat + "." + matcher.group(2)))
        } catch {
          case ex: Exception => println("Probelm with file: " + file.getName)
        }
      }

    }

}
