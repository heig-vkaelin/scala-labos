val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Bot-tender",
    version := "0.2.1",
    scalaVersion := scala3Version,
    libraryDependencies ++= List(
        "org.scalactic" %% "scalactic" % "3.2.15",
        "org.scalatest" %% "scalatest" % "3.2.15" % "test",
    ),
    libraryDependencies ++= List(        
        "com.lihaoyi" %% "scalatags" % "0.12.0",
        "com.lihaoyi" %% "cask" % "0.9.1",
    ),
  )
