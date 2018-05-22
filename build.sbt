import Dependencies._

lazy val cryptic = (project in file(".")).
  enablePlugins(TutPlugin).
  enablePlugins(ParadoxPlugin).
  settings(
    inThisBuild(List(
      organization := "ScalaCrypto",
      scalaVersion := "2.12.5",
      version := "0.2.0-SNAPSHOT"
    )),
    name := "cryptic",
    paradoxProperties in Compile ++= Map(
      "github.base_url" -> s"https://github.com/ScalaCrypto/cryptic/tree/${version.value}"))
  .aggregate(core, `serialization-fst`, `crypto-javax`, `crypto-test`)

lazy val core = (project in file("core")).
  settings(
    name := "core",
    libraryDependencies ++= Seq(scalaTest % Test)
  )

lazy val `serialization-fst` = (project in file("serialization-fst")).
  settings(
    name := "serialization-fst",
    libraryDependencies ++= Seq(scalaTest % Test, fst)
  )
  .dependsOn(core)

lazy val `crypto-javax` = (project in file("crypto-javax")).
  settings(
    name := "crypto-javax",
    libraryDependencies ++= Seq(scalaTest % Test)
  )
  .dependsOn(core)

lazy val `crypto-test` = (project in file("crypto-test")).
  settings(
    name := "crypto-test",
    libraryDependencies ++= Seq(scalaTest % Test)
  )
  .dependsOn(core, `serialization-fst`, `crypto-javax`)
