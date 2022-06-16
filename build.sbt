import Dependencies._

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "interview-project",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.lihaoyi" %% "upickle" % "2.0.0",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
  )
