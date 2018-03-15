name := "minotaur"
version := "0.0.0"

connectInput in run := true

scalaVersion := "2.11.11"

val dl4jVersion = "0.9.1"
val nd4jOS = "linux-x86_64"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.1",
  "org.nd4j" % "nd4j-native-platform" % dl4jVersion,
  "org.nd4j" %% "nd4s" % dl4jVersion,
  "org.deeplearning4j" % "deeplearning4j-core" % dl4jVersion,
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "org.specs2" %% "specs2-core" % "3.9.5" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
