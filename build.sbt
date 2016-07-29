name := "minotaur"
version := "0.0.0"

connectInput in run := true

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")
