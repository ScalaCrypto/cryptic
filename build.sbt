lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9" withSources()
lazy val fst = "de.ruedigermoeller" % "fst" % "2.57" withSources()

lazy val cryptic = (project in file(".")).
  //enablePlugins(TutPlugin).
  enablePlugins(ParadoxPlugin).
  settings(
    inThisBuild(List(
      organization := "ScalaCrypto",
      scalaVersion := "2.13.6",
      version := "0.4.0-SNAPSHOT"
    )),
    name := "cryptic",
    publish / skip := true,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    Compile / paradoxProperties ++= Map(
      "github.base_url" -> s"https://github.com/ScalaCrypto/cryptic/tree/${version.value}")).
  aggregate(core, `serialization-fst`, `crypto-javax`, `crypto-bouncycastle`, `crypto-test`)

lazy val core = (project in file("core")).
  settings(
    name := "core",
    libraryDependencies ++= Seq(scalaTest % Test),
    artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
      "cryptic-" + artifact.name + "-" + module.revision + "." + artifact.extension
    }    
  )

lazy val `serialization-fst` = (project in file("serialization-fst")).
  settings(
    name := "serialization-fst",
    libraryDependencies ++= Seq(scalaTest % Test, fst),
    artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
      "cryptic-" + artifact.name.replace("serialization-", "") + "-" + module.revision + "." + artifact.extension
    }
  ).
  dependsOn(core)

lazy val `crypto-javax` = (project in file("crypto-javax")).
  settings(
    name := "crypto-javax",
    libraryDependencies ++= Seq(scalaTest % Test),
    artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
      "cryptic-" + artifact.name.replace("crypto-", "") + "-" + module.revision + "." + artifact.extension
    }
  ).
  dependsOn(core)

lazy val `crypto-bouncycastle` = (project in file("crypto-bouncycastle")).
    settings(
      name := "crypto-bouncycastle",
      libraryDependencies ++= Seq(
        "org.bouncycastle" % "bcprov-jdk15on" % "1.60",
        scalaTest % Test),
      artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
        "cryptic-" + artifact.name.replace("crypto-", "") + "-" + module.revision + "." + artifact.extension
      }
    )
    .dependsOn(core)

lazy val `crypto-test` = (project in file("crypto-test")).
  settings(
    name := "crypto-test",
    libraryDependencies ++= Seq(scalaTest % Test),
    publish / skip := true,
  ).
  dependsOn(core, `serialization-fst`, `crypto-bouncycastle`, `crypto-javax`)
