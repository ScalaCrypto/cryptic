import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" withSources()
  lazy val fst = "de.ruedigermoeller" % "fst" % "2.56" withSources()
}
