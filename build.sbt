name := "kaze-class"
organization := "kaze-class"

version := "0.1-SNAPSHOT"
scalaVersion := "2.12.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

initialCommands :=
  """|
     |import pl.project13.kaze.KazeClass
     |""".stripMargin
