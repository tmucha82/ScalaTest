name := "scala-test"

scalaVersion := "2.11.6"

version := "1.0.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc()

libraryDependencies += "com.google.guava" % "guava" % "16.0"

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"
