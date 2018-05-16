import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.4"
  lazy val fst = "de.ruedigermoeller" % "fst" % "2.56" withSources()
}
