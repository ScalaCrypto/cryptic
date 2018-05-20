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
```scala
import cryptic._
import cryptic.syntax._
```

Define your data types:
```scala
case class EmailAddress(literal: String)
case class User(id: Long, email: Encrypted[EmailAddress])
```

Encrypt your data using convenient syntax (a crypto and a serializer must be available):
```scala
import cryptic.crypto.RSA._
import cryptic.serialization.Fst._

val user = User(123, EmailAddress("Odd@Example.com").encrypted
```

Access your data in encrypted form (can be done without crypto/serializer):
```scala
val bytes: Array[Byte] = user.email.bytes
```

Transform your data (can be done without crypto/serializer):
```scala
val lowered = user.email.
    map(_.copy(literal = _.literal.toLower))
```

Run your staged transformations (a crypto and a serializer must be available):
```scala
import cryptic.crypto.RSA._
import cryptic.serialization.Fst._

val user2 = user.copy(
  email = lowered.run())
```

Decrypt your transformed data (a crypto and a serializer must be available):
```scala
import cryptic.crypto.RSA._
import cryptic.serialization.Fst._

val emailInLower = user2.email.decrypted
```

