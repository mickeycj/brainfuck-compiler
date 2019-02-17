scalaVersion := "2.12.7"
organization := "com.mickeycj"

lazy val brainfuckcompiler = (project in file("."))
  .settings(
    name := "BrainFuckCompiler",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
  )
