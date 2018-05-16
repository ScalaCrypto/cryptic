import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.bitbonanza",
      scalaVersion := "2.12.5",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "cryptic",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      fst)
  )
