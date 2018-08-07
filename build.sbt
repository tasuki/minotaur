name := "minotaur"
version := "0.0.0"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.1",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "com.softwaremill.sttp" %% "core" % "1.3.0-RC5",
  "org.specs2" %% "specs2-core" % "3.9.5" % Test
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.9.3")

run / connectInput := true
compile / mainClass := Some("Client")
Test / scalacOptions ++= Seq("-Yrangepos")
