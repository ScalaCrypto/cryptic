import scala.collection.Seq
// sbt-assembly keys and helpers
import sbtassembly.AssemblyPlugin.autoImport.*
import sbtassembly.{MergeStrategy, PathList}

lazy val scalaTest = ("org.scalatest" %% "scalatest" % "3.2.19").withSources()
lazy val chill = ("com.twitter" % "chill" % "0.10.0")
  .cross(CrossVersion.for3Use2_13)
  .withSources()
lazy val fst = ("de.ruedigermoeller" % "fst" % "3.0.3").withSources()
lazy val upickle = ("com.lihaoyi" %% "upickle" % "4.3.2").withSources()
lazy val bc = ("org.bouncycastle" % "bcprov-jdk18on" % "1.82").withSources()
lazy val scribe = ("com.outr" %% "scribe" % "3.17.0").withSources()

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
  Seq(
    Test / fork := true,
    Test / javaOptions ++= javaBaseOpens,
    libraryDependencies += scalaTest % Test
  )

lazy val coreSettings = commonSettings ++ Seq(
  name := "core",
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name}-${module.revision}.${artifact.extension}"
  }
)

lazy val codecChillSettings = commonSettings ++ Seq(
  name := "codec-chill",
  libraryDependencies ++= Seq(chill),
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name.replace("codec-", "")}-${module.revision}.${artifact.extension}"
  }
)
lazy val codecFstSettings = commonSettings ++ Seq(
  name := "codec-fst",
  libraryDependencies += fst,
  Compile / packageBin / mappings += {
    file("LICENSE") -> "LICENSE"
  },
  artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    s"cryptic-${artifact.name.replace("codec-", "")}-${module.revision}.${artifact.extension}"
  }
)

lazy val codecUpickleSettings = commonSettings ++ Seq(
  name := "codec-upickle",
  libraryDependencies += upickle,
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
    Compile / packageBin / mappings += {
      file("LICENSE") -> "LICENSE"
    },
    artifactName := {
      (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
        s"cryptic-${artifact.name.replace("cipher-", "")}-${module.revision}.${artifact.extension}"
    }
  )

lazy val cipherTestSettings =
  commonSettings ++ Seq(
    name := "cipher-test",
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
  .settings(cipherCommonSettings("cipher-javax"))
  .dependsOn(core)
lazy val `cipher-bouncycastle` = (project in file("cipher-bouncycastle"))
  .settings(
    cipherCommonSettings("cipher-bouncycastle") ++ Seq(
      libraryDependencies += bc
    )
  )
  .dependsOn(core, `cipher-javax`)

lazy val `cipher-enigma` = (project in file("cipher-enigma"))
  .settings(
    cipherCommonSettings("cipher-enigma") ++ Seq(
      libraryDependencies ++= Seq(
        scribe,
        "com.lihaoyi" %% "mainargs" % "0.7.6"
      ),
      // Make the packaged jar executable by setting the Main-Class
      Compile / packageBin / packageOptions += sbt.Package.MainClass(
        "cryptic.cipher.enigma.Enigma"
      ),
      // --- Assembly (fat jar) configuration ---
      // Ensure the assembly has the correct entry point
      assembly / mainClass := Some("cryptic.cipher.enigma.CLI"),
      // Name of the assembled jar produced by the subproject (we still copy/rename below)
      assembly / assemblyJarName := s"${name.value}-fat-${version.value}.jar",
      // Include scala-library and all dependencies inside the fat jar
      assembly / assemblyOption := (assembly / assemblyOption).value
        .withIncludeScala(true)
        .withIncludeDependency(true),
      // Merge strategy to avoid META-INF clashes and concatenate reference.conf when present
      assembly / assemblyMergeStrategy := {
        case PathList("reference.conf")          => MergeStrategy.concat
        case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
        case PathList("META-INF", "INDEX.LIST")  => MergeStrategy.discard
        case PathList("META-INF", _*)            => MergeStrategy.discard
        case PathList("module-info.class")       => MergeStrategy.discard
        case x if x.endsWith(".proto")           => MergeStrategy.first
        case x if x.endsWith("io.netty.versions.properties") =>
          MergeStrategy.first
        case _ => MergeStrategy.first
      },
      // Project-scoped task to produce a self-contained bash launcher script with the fat jar embedded
      enigma := {
        import java.util.Base64
        val log = streams.value.log
        val fat = (Compile / assembly).value
        val targetDir = (ThisProject / target).value
        val jarOut = targetDir / "enigma.jar"
        // Keep writing the jar for convenience
        IO.copyFile(fat, jarOut)
        log.info(s"Wrote jar ${jarOut.getAbsolutePath}")

        val scriptOut = targetDir / "enigma"
        val jarBytes = IO.readBytes(fat)
        val b64 = Base64
          .getMimeEncoder(76, "\n".getBytes("UTF-8"))
          .encodeToString(jarBytes)

        val header =
          """#!/usr/bin/env bash
set -euo pipefail

# Self-extract embedded Enigma jar and run it
TMPDIR_="${TMPDIR:-/tmp}"
JAR_="$(mktemp "${TMPDIR_%/}/enigma-jar-XXXXXX")"
cleanup_() { rm -f "$JAR_"; }
trap cleanup_ EXIT

# Extract base64 jar payload that starts after the marker line
awk 'found{print} /^__JAR_BASE64__$/ {found=1; next}' "$0" | base64 --decode > "$JAR_"

exec java ${JAVA_OPTS:-} -jar "$JAR_" "$@"
cleanup_
__JAR_BASE64__
"""

        IO.write(scriptOut, header + b64 + "\n")
        // Make the script executable
        scriptOut.setExecutable(true)
        log.info(s"Wrote launcher ${scriptOut.getAbsolutePath}")
        scriptOut
      }
    )
  )
  .dependsOn(core)

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

// Task key to build an executable Enigma bash script with embedded fat jar (scoped per project)
lazy val enigma = taskKey[File](
  "Create an executable 'enigma' bash script with embedded fat jar"
)
