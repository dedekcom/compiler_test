name := "compilert"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.2.0" % "test",
  "org.specs2" %% "specs2-junit" % "4.2.0" % "test",
  "junit" % "junit" % "4.12",
  "org.scala-sbt" % "test-interface" % "1.0"
)

scalacOptions in Test ++= Seq("-Yrangepos")
