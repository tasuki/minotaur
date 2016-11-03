name := "minotaur"
version := "0.0.0"

connectInput in run := true

libraryDependencies += "com.typesafe" % "config" % "1.3.1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")
