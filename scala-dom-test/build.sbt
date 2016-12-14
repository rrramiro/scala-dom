import Dependencies._

name := "scala-dom-test"

organization := "fr.ramiro"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
  scalaTest,
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
)