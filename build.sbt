import scala.sys.process._

name := "minotaur"
version := "0.0.0"

connectInput in run := true

scalaVersion := "2.11.11"

resolvers += Resolver.sonatypeRepo("snapshots")

cleanFiles += baseDirectory.value / "lib"
val mvnInstall = Seq("mvn", "install", "-q", "-f", "sbt-pom.xml")
update := { mvnInstall !; update.value }

val dl4jVersion = "0.9.2-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.1",
  //"org.nd4j" % "nd4j-native-platform" % dl4jVersion, // use this for stable
  "org.nd4j" % "nd4j-native" % dl4jVersion,
  "org.deeplearning4j" % "deeplearning4j-core" % dl4jVersion,
  "org.deeplearning4j" % "deeplearning4j-modelimport" % dl4jVersion,
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "org.specs2" %% "specs2-core" % "3.9.5" % Test
)

scalacOptions in Test ++= Seq("-Yrangepos")
