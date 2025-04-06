# Cryptic

_Keeps your secrets._
Cryptic is a monad for encrypting and decryptic data.
To use cryptic you must select a crypto and a serializer.

Cryptic supports several crypto libraries and serializers,
select the modules you nedd or write your own

| Module                | Supports            |
|:----------------------|:--------------------| 
| crypto-javax          | AES, RSA            |
| crypto-bouncycastle   | EC (Elliptic curve) |
| serialization-fst     | Fst                 | 
| serialization-upickle | Upickle             | 

## Installation

### sbt

```sbt
"scalacrypto" %% "core" % "1.0.0"
```

To encrypt with AES or RSA

```sbt 
"scalacrypto" %% "crypto-javax" % "1.0.0"
```

To encrypt with EC

```sbt 
"scalacrypto" %% "crypto-bouncycastle" % "1.0.0"
```

Select serializer one of

```sbt
"scalacrypto" %% "serialization-fst" % "1.0.0"

"scalacrypto" %% "serialization-upickle" % "1.0.0"
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

object EmailAddress {
  val rw: ReadWriter[EmailAddress] = macroRW // If you  use the Upickle serializer
}

case class User(id: Long, email: Encrypted[EmailAddress])

object User {
  val rw: ReadWriter[User] = macroRW // If you use the Upickle serializer
}

```

Encrypt your data using convenient syntax, a crypto and a serializer must be available:

```scala
import java.security.{KeyPair, PrivateKey, PublicKey}
import cryptic.crypto.EC // Elliptic Curve encryption

// We need an implicit public key to enable encryption for EC
private val keyPair: KeyPair = EC.keygen(256)
implicit private val publicKey: PublicKey = keyPair.getPublic

val user = User(123, EmailAddress("odd@example.com").encrypted)
```

To use the Upickle serializer:

```scala
import cryptic.serialization.Upickle

implicit def serializer[V](implicit rw: ReadWriter[V]): Serializer[V] = Upickle[V]
```

Access your data in encrypted form (can be done without crypto/serializer):

```scala
val bytes: Array[Byte] = user.email.bytes
```

Transform your data, can be done without crypto/serializer:

```scala
val lowered = user.email.
  map(_.copy(literal = _.literal.toLower))
```

Run your staged transformations, a crypto and a serializer must be in scope:

```scala
import cryptic.crypto.RSA._

val user2 = user.copy(email = lowered.run())
```

Decrypt your transformed data, a crypto and a serializer must be in scope:

```scala
import cryptic.crypto.RSA._
import cryptic.serialization.Fst._

val emailInLower = user2.email.decrypted
```

## Provided cryptos

- AES
- RSA
- EC

## Provided serializers

- Fst
- Upickle

## Roll your own

### Serializer

Provide an implementation of the Serializer trait:

```scala
trait Serializer[V] {
  def serialize(value: V): PlainText

  def deserialize(plainText: PlainText): Either[String, V]
}
```

Example:

```scala

import scala.util.Try

object MySerializer {
  implicit def serializer[V]: Serializer[V] = new Serializer[V] {

    override def serialize(value: V): PlainText = ???

    override def deserialize(plainText: PlainText): Either[String, V] = ???
  }
}
```

### Crypto

Provide implementations of the Encrypt and Decrypt traits and probably a Key:

```scala
trait Encrypt {
  def apply(plainText: PlainText): CipherText
}

trait Decrypt {
  def apply(cipherText: CipherText): Either[String, PlainText]
}
```

Example Caesar crypto:

```scala
object Caesar {
  case class Key(offset: Int) {
    require(offset != 0)
  }

  implicit def encrypt(implicit key: Key): Encrypt = (plainText: PlainText) => {
    val bytes = plainText.map(b ⇒ (b + key.offset).toByte)
    CipherText(bytes)
  }

  implicit def decrypt(implicit key: Key): Decrypt = (cipherText: CipherText) =>
    Right[String, PlainText](
      PlainText(cipherText.bytes.map(b => (b - key.offset).toByte))
    )

  def keygen(offset: Int): Key = Key(offset)
}
```




