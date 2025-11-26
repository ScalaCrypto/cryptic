import scala.collection.Seq

lazy val scalaTest = ("org.scalatest" %% "scalatest" % "3.2.19").withSources()
lazy val chill = ("com.twitter" % "chill" % "0.10.0")
  .cross(CrossVersion.for3Use2_13)
  .withSources()
lazy val fst = ("de.ruedigermoeller" % "fst" % "3.0.3").withSources()
lazy val upickle = ("com.lihaoyi" %% "upickle" % "4.3.2").withSources()
lazy val bc = ("org.bouncycastle" % "bcprov-jdk18on" % "1.82").withSources()

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
  "--add-opens=java.sql/java.sql=ALL-UNNAMED"
)

// Github Actions
// sbt-github-actions defaults to using JDK 8 for testing and publishing.
// The following adds JDK 21 for testing.
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("21"))
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(
    RefPredicate.StartsWith(Ref.Tag("v")),
    RefPredicate.Equals(Ref.Branch("master"))
  )
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    commands = List("ci-release"),
    name = Some("Publish project"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

inThisBuild(
  List(
    organization := "io.scalacrypto.cryptic",
    homepage := Some(url("https://github.com/scalacrypto/cryptic")),
    licenses := List(
      "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "cyberzac",
        "Martin Zachrison",
        "zac@cyberzac.se",
        url("https://github.com/cyberzac")
      )
    )
  )
)
lazy val commonSettings =
  Seq(
    scalaVersion := "3.7.3",
    scalacOptions ++= Seq(
      "-Xmax-inlines",
      "64",
      "-Yexplicit-nulls",
      "-Wsafe-init"
    ),
    Compile / packageDoc / publishArtifact := true,
    Compile / packageSrc / publishArtifact := true,
    Compile / packageBin / publishArtifact := true,
    Test / publishArtifact := false,
    javaOptions ++= javaBaseOpens
  ) ++ testSettings

lazy val testSettings =
  Seq(Test / fork := true, Test / javaOptions ++= javaBaseOpens)

lazy val coreSettings = commonSettings ++ Seq(
  name := "core",
  libraryDependencies ++= Seq(scalaTest % Test),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name}-${module.revision}.${artifact.extension}"
  }
)

lazy val codecChillSettings = commonSettings ++ Seq(
  name := "codec-chill",
  libraryDependencies ++= Seq(scalaTest % Test, chill),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name.replace("codec-", "")}-${module.revision}.${artifact.extension}"
  }
)
lazy val codecFstSettings = commonSettings ++ testSettings ++ Seq(
  name := "codec-fst",
  libraryDependencies ++= Seq(scalaTest % Test, fst),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name.replace("codec-", "")}-${module.revision}.${artifact.extension}"
  }
)

lazy val codecUpickleSettings = commonSettings ++ Seq(
  name := "codec-upickle",
  libraryDependencies ++= Seq(scalaTest % Test, upickle),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name.replace("codec-", "")}-${module.revision}.${artifact.extension}"
  }
)

lazy val cipherCommonSettings = (projectName: String) =>
  commonSettings ++ Seq(
    name := projectName,
    libraryDependencies ++= Seq(scalaTest % Test),
    Compile / packageBin / mappings += {
      file("LICENSE") -> "LICENSE"
    },
    artifactName := {
      (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
        s"cryptic-${artifact.name.replace("cipher-", "")}-${module.revision}.${artifact.extension}"
    }
  )

lazy val cipherTestSettings =
  commonSettings ++ testSettings ++ Seq(
    name := "cipher-test",
    libraryDependencies ++= Seq(scalaTest % Test),
    publish / skip := true
  )

lazy val core = (project in file("core")).settings(coreSettings)

lazy val `codec-chill` =
  (project in file("codec-chill")).settings(codecChillSettings).dependsOn(core)

lazy val `codec-fst` =
  (project in file("codec-fst")).settings(codecFstSettings).dependsOn(core)

lazy val `codec-upickle` = (project in file("codec-upickle"))
  .settings(codecUpickleSettings)
  .dependsOn(core)

lazy val `cipher-javax` = (project in file("cipher-javax"))
  .settings(
    cipherCommonSettings("cipher-javax") ++ Seq(
      libraryDependencies ++= Seq(bc, scalaTest % Test)
    )
  )
  .dependsOn(core)
lazy val `cipher-bouncycastle` = (project in file("cipher-bouncycastle"))
  .settings(
    cipherCommonSettings("cipher-bouncycastle") ++ Seq(
      libraryDependencies ++= Seq(bc, scalaTest % Test)
    )
  )
  .dependsOn(core, `cipher-javax`)

lazy val `cipher-test` = (project in file("cipher-test"))
  .settings(cipherTestSettings)
  .dependsOn(
    core,
    `codec-chill`,
    `codec-fst`,
    `codec-upickle`,
    `cipher-javax`,
    `cipher-bouncycastle`
  )

lazy val `cipher-enigma` = (project in file("cipher-enigma"))
  .settings(
    cipherCommonSettings("cipher-enigma")
  )
  .dependsOn(core)

lazy val cryptic = (project in file("."))
  .enablePlugins(ParadoxPlugin)
  .settings(
    inThisBuild(commonSettings),
    name := "cryptic",
    publish / skip := true,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    Compile / paradoxProperties ++= Map(
      "github.base_url" -> s"https://github.com/Scalacipher/cryptic/tree/${version.value}"
    )
  )
  .aggregate(
    core,
    `codec-chill`,
    `codec-fst`,
    `codec-upickle`,
    `cipher-bouncycastle`,
    `cipher-javax`,
    `cipher-enigma`,
    `cipher-test`
  )
