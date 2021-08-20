# Cryptic
_Keeps your secrets._
Cryptic is a monad for encrypting and decryptic data.
To use cryptic you must select a crypto and a serializer.

Cryptic supports several crypto libraries and serializers, 
select the modules you nedd or write your own

| Module | Supports |
|:-------- |:------------| 
| crypto-javax | AES, RSA |
| crypto-bouncycastle | ECIES (Elliptic curve) |
| serialization-fst | FST| 


## Installation
### sbt

```sbt
"scalacrypto" %% "core" % "1.0.0"
"scalacrypto" %% "serialization-fst" % "1.0.0"
```
To use AES or RSA 
```sbt 
"scalacrypto" %% "crypto-javax" % "1.0.0"
```

## Usage
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
import java.security.{KeyPair, PrivateKey, PublicKey}
import cryptic.serialization.Fst._
import cryptic.syntax._
import cryptic.crypto.ECIES

// We need an implicit public key to enable encryption
private val keyPair: KeyPair = ECIES.keygen(256)
implicit private val publicKey: PublicKey = keyPair.getPublic

val user = User(123, EmailAddress("odd@example.com").encrypted)
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
## Provided cryptos

- AES
- RSA
- ECIES

## Provided serializers

- FST


