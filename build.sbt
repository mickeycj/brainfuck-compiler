scalaVersion := "2.13.13"
organization := "com.mickeycj"

lazy val brainfuckcompiler = (project in file("."))
  .settings(
    name := "BrainfuckCompiler",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    libraryDependencies += "org.mockito" %% "mockito-scala" % "1.17.30" % Test
  )
