# Cryptic
Keeps your secrets

## Getting started
Add dependecy on core and your chosen serialization and crypto providers to your build.sbt file:
```scala
libraryDependencies ++= Seq(
  "scalacrypto" %% "cryptic-core" % "0.2.0-SNAPSHOT",
  "scalacrypto" %% "cryptic-serialization-fst" % "0.2.0-SNAPSHOT",
  "scalacrypto" %% "cryptic-crypto-javax" % "0.2.0-SNAPSHOT"
)
```

Import base package and syntax extension:
@@snip [GettingStartedSpec.scala](../../../crypto-javax/src/test/scala/cryptic/crypto/GettingStartedSpec.scala) { #import }

Define your data types:
@@snip [GettingStartedSpec.scala](../../../crypto-javax/src/test/scala/cryptic/crypto/GettingStartedSpec.scala) { #data-types }

Encrypt your data using convenient syntax (a crypto and a serializer must be available):
@@snip [GettingStartedSpec.scala](../../../crypto-javax/src/test/scala/cryptic/crypto/GettingStartedSpec.scala) { #encrypt }

Access your data in encrypted form (can be done without crypto/serializer):
@@snip [GettingStartedSpec.scala](../../../crypto-javax/src/test/scala/cryptic/crypto/GettingStartedSpec.scala) { #access }

Transform your data (can be done without crypto/serializer):
@@snip [GettingStartedSpec.scala](../../../crypto-javax/src/test/scala/cryptic/crypto/GettingStartedSpec.scala) { #transform }

Run your staged transformations (a crypto and a serializer must be available):
@@snip [GettingStartedSpec.scala](../../../crypto-javax/src/test/scala/cryptic/crypto/GettingStartedSpec.scala) { #run }

Decrypt your transformed data (a crypto and a serializer must be available):
@@snip [GettingStartedSpec.scala](../../../crypto-javax/src/test/scala/cryptic/crypto/GettingStartedSpec.scala) { #decrypt }

