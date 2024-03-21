scalaVersion := "2.13.13"
organization := "com.mickeycj"

lazy val brainfuckcompiler = (project in file("."))
  .settings(
    name := "BrainfuckCompiler",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    libraryDependencies += "org.scalatestplus" %% "mockito-5-10" % "3.2.18.0" % Test
  )
