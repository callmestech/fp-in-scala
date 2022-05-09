name := "fp-in-scala"

version := "0.1"

scalaVersion := "2.13.8"

val testDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "org.scalatest" %% "scalatest" % "3.2.2",
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.2.0"
).map(_ % "test")

libraryDependencies ++= testDependencies
