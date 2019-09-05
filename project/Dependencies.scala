import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8" withSources()
  lazy val fst = "de.ruedigermoeller" % "fst" % "2.57" withSources()
}
