name := "minotaur"
version := "0.0.0"

connectInput in run := true

scalaVersion := "2.11.11"

val dl4jVersion = "1.0.0-alpha"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.1",
  "org.nd4j" % "nd4j-native-platform" % dl4jVersion,
  "org.deeplearning4j" % "deeplearning4j-core" % dl4jVersion,
  "org.deeplearning4j" % "deeplearning4j-modelimport" % dl4jVersion,
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "org.specs2" %% "specs2-core" % "3.9.5" % Test
)

scalacOptions in Test ++= Seq("-Yrangepos")
