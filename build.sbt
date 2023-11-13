import sbt.*

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "taskdsl",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0"
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
