import java.io.File

import scala.io.Source

val filesHere = new File("./scripts").listFiles

def fileLines(file: File) = Source.fromFile(file).getLines().toList

def grep(pattern: String) =
  for (file <- filesHere
       if file.getName.endsWith("scala");
       line <- fileLines(file);
       trimmed = line.trim
       if trimmed.matches(pattern))
    println(file + ":" + trimmed)

grep(".*scripts.*")