# Cryptic

Keeps your secrets

## Getting started

Add dependecy on core and your chosen codec and crypto providers to your build.sbt file:
@@dependency[sbt,Maven,Gradle] { group="scalacrypto" artifact="cryptic-core$scala.binary_version$" version="$version$" }

Static:

```scala
libraryDependencies ++= Seq(
  "scalacrypto" %% "cryptic-core" % "0.2.0-SNAPSHOT",
  "scalacrypto" %% "cryptic-codec-fst" % "0.2.0-SNAPSHOT",
  "scalacrypto" %% "cryptic-crypto-javax" % "0.2.0-SNAPSHOT"
)
```

Import base package and syntax extension:
@@snip [GettingStartedSpec.scala](../../../crypto-test/src/test/scala/cryptic/test/GettingStartedSpec.scala) { #import }

Define your data types:
@@snip [GettingStartedSpec.scala](../../../crypto-test/src/test/scala/cryptic/test/GettingStartedSpec.scala) { #data-types }

Encrypt your data using convenient syntax (a crypto and a codec must be available):
@@snip [GettingStartedSpec.scala](../../../crypto-test/src/test/scala/cryptic/test/GettingStartedSpec.scala) { #encrypt }

Access your data in encrypted form (can be done without crypto/codec):
@@snip [GettingStartedSpec.scala](../../../crypto-test/src/test/scala/cryptic/test/GettingStartedSpec.scala) { #access }

Transform your data (can be done without crypto/codec):
@@snip [GettingStartedSpec.scala](../../../crypto-test/src/test/scala/cryptic/test/GettingStartedSpec.scala) { #transform }

Run your staged transformations (a crypto and a codec must be available):
@@snip [GettingStartedSpec.scala](../../../crypto-test/src/test/scala/cryptic/test/GettingStartedSpec.scala) { #run }

Decrypt your transformed data (a crypto and a codec must be available):
@@snip [GettingStartedSpec.scala](../../../crypto-test/src/test/scala/cryptic/test/GettingStartedSpec.scala) { #decrypt }

