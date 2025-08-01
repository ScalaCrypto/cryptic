import scala.collection.Seq

lazy val scalaTest = ("org.scalatest" %% "scalatest" % "3.2.19").withSources()
lazy val chill = ("com.twitter" % "chill" % "0.10.0").cross(CrossVersion.for3Use2_13).withSources()
lazy val fst = ("de.ruedigermoeller" % "fst" % "3.0.4-jdk17").withSources()
lazy val upickle = ("com.lihaoyi" %% "upickle" % "4.2.1").withSources()
lazy val bc = ("org.bouncycastle" % "bcprov-jdk18on" % "1.81").withSources()

lazy val javaBaseOpens = Seq(
  "--add-opens=java.base/java.lang=ALL-UNNAMED",
  "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
  "--add-opens=java.base/java.math=ALL-UNNAMED",
  "--add-opens=java.base/java.net=ALL-UNNAMED",
  "--add-opens=java.base/java.nio=ALL-UNNAMED",
  "--add-opens=java.base/java.util=ALL-UNNAMED",
  "--add-opens=java.base/java.text=ALL-UNNAMED",
  "--add-opens=java.base/java.util.concurrent=ALL-UNNAMED",
  "--add-opens=java.base/java.util.regex=ALL-UNNAMED",
  "--add-opens=java.base/jdk.internal.misc=ALL-UNNAMED",
  "--add-opens=java.sql/java.sql=ALL-UNNAMED")

lazy val commonSettings =
  Seq(
    organization := "scalacrypto",
    scalaVersion := "3.6.4",
    version := "0.7.0-SNAPSHOT",
    Compile / packageDoc / publishArtifact := false,
    Compile / packageSrc / publishArtifact := true,
    Compile / packageBin / publishArtifact := true,
    Test / publishArtifact := false,
    javaOptions ++= javaBaseOpens
  ) ++ testSettings

lazy val testSettings = Seq(Test / fork := true, Test / javaOptions ++= javaBaseOpens)

lazy val coreSettings = commonSettings ++ Seq(
  name := "core",
  libraryDependencies ++= Seq(scalaTest % Test),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name}-${module.revision}.${artifact.extension}"
  })

lazy val serializationChillSettings = commonSettings ++ Seq(
  name := "serialization-chill",
  libraryDependencies ++= Seq(scalaTest % Test, chill),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name.replace("serialization-", "")}-${module.revision}.${artifact.extension}"
  })
lazy val serializationFstSettings = commonSettings ++ testSettings ++ Seq(
  name := "serialization-fst",
  libraryDependencies ++= Seq(scalaTest % Test, fst),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name.replace("serialization-", "")}-${module.revision}.${artifact.extension}"
  })

lazy val serializationUpickleSettings = commonSettings ++ Seq(
  name := "serialization-upickle",
  libraryDependencies ++= Seq(scalaTest % Test, upickle),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name.replace("serialization-", "")}-${module.revision}.${artifact.extension}"
  })

lazy val cryptoCommonSettings = (projectName: String) =>
  commonSettings ++ Seq(
    name := projectName,
    libraryDependencies ++= Seq(scalaTest % Test),
    Compile / packageBin / mappings += {
      file("LICENSE") -> "LICENSE"
    },
    artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
      s"cryptic-${artifact.name.replace("crypto-", "")}-${module.revision}.${artifact.extension}"
    })

lazy val cryptoTestSettings =
  commonSettings ++ testSettings ++ Seq(name := "crypto-test", libraryDependencies ++= Seq(scalaTest % Test), publish / skip := true)

lazy val core = (project in file("core")).settings(coreSettings)

lazy val `serialization-chill` = (project in file("serialization-chill")).settings(serializationChillSettings).dependsOn(core)

lazy val `serialization-fst` = (project in file("serialization-fst")).settings(serializationFstSettings).dependsOn(core)

lazy val `serialization-upickle` = (project in file("serialization-upickle")).settings(serializationUpickleSettings).dependsOn(core)

lazy val `crypto-javax` = (project in file("crypto-javax")).settings(cryptoCommonSettings("crypto-javax")).dependsOn(core)

lazy val `crypto-bouncycastle` = (project in file("crypto-bouncycastle"))
  .settings(cryptoCommonSettings("crypto-bouncycastle") ++ Seq(libraryDependencies ++= Seq(bc, scalaTest % Test)))
  .dependsOn(core)

lazy val `crypto-test` = (project in file("crypto-test"))
  .settings(cryptoTestSettings)
  .dependsOn(core, `serialization-chill`, `serialization-fst`, `serialization-upickle`, `crypto-bouncycastle`, `crypto-javax`)

lazy val cryptic = (project in file("."))
  .enablePlugins(ParadoxPlugin)
  .settings(
    inThisBuild(commonSettings),
    name := "cryptic",
    publish / skip := true,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    Compile / paradoxProperties ++= Map("github.base_url" -> s"https://github.com/ScalaCrypto/cryptic/tree/${version.value}"))
  .aggregate(core, `serialization-chill`, `serialization-fst`, `serialization-upickle`, `crypto-javax`, `crypto-bouncycastle`, `crypto-test`)