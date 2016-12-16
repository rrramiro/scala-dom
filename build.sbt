import Dependencies._

val scalaDomTest = project in file("scala-dom-test")

dependsOn(scalaDomTest % Test)

aggregateProjects(scalaDomTest)

name := "scala-dom"

organization := "com.example"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
  scalaTest % Test,
  "org.xmlunit" % "xmlunit-core" % "2.3.0",
  "junit" % "junit" % "4.12"  % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "net.sf.saxon" % "Saxon-HE" % "9.5.1-5"
)
