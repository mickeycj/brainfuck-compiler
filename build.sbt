scalaVersion := "2.12.7"
organization := "com.mickeycj"

lazy val brainfuckcompiler = (project in file("."))
  .settings(
    name := "BrainfuckCompiler",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "org.mockito" %% "mockito-scala" % "1.1.4" % Test
  )
