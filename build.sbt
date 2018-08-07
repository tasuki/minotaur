name := "minotaur"
version := "0.0.0"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.1",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "org.specs2" %% "specs2-core" % "3.9.5" % Test
)

run / connectInput := true
compile / mainClass := Some("Client")
Test / scalacOptions ++= Seq("-Yrangepos")
