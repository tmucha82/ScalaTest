name := "scala-test"

scalaVersion := "2.11.8"

version := "1.0.1"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.7",
  "org.scala-lang" % "scala-swing" % "2.11+",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc(),
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.apache.spark" %% "spark-core" % "2.1.0",
  "org.apache.spark" %% "spark-sql" % "2.1.0",
  "com.typesafe.akka" % "akka-actor_2.11" % "2.3.14",
  "com.typesafe.akka" % "akka-testkit_2.11" % "2.3.14",
  "org.scalacheck" %% "scalacheck" % "1.12.1",
  "com.storm-enroute" %% "scalameter-core" % "0.7",
  "junit" % "junit" % "4.11" % "test",
  "com.google.guava" % "guava" % "16.0"
)