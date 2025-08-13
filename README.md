# Cryptic

_Keeps your secrets._
Cryptic is a monad for encrypting and decryptic data.
To use cryptic you must select a crypto and a codec.

Cryptic supports several crypto libraries and codecs,
select the modules you nedd or write your own

| Module                | Supports            |
|:----------------------|:--------------------| 
| crypto-javax          | AES, RSA            |
| crypto-bouncycastle   | EC (Elliptic curve) |
| codec-chill   | Chill               | 
| codec-fst     | Fst                 | 
| codec-upickle | Upickle             | 

## Installation

### sbt

```sbt
"scalacrypto" %% "core" % "1.0.0"
```

To encrypt with AES or RSA

```sbt 
"scalacrypto" %% "crypto-javax" % "1.0.0"
```

To encrypt with Elliptic Curve, ECIES 

```sbt 
"scalacrypto" %% "crypto-bouncycastle" % "1.0.0"
```

Use the default codec for scala types or select one of

```sbt
"scalacrypto" %% "codec-chill" % "1.0.0"

"scalacrypto" %% "codec-fst" % "1.0.0"

"scalacrypto" %% "codec-upickle" % "1.0.0"
```

## Usage

Import base package and syntax extension:

```scala
import cryptic.{given,*}
```

Define your data types:

```scala
case class EmailAddress(literal: String)

object EmailAddress:
  val rw: ReadWriter[EmailAddress] = macroRW // If you  use the Upickle codec

case class User(id: Long, email: Encrypted[EmailAddress])

object User:
  val rw: ReadWriter[User] = macroRW // If you use the Upickle codec
```

Encrypt your data using convenient syntax, a crypto and a codec must be available:

```scala
import java.security.{KeyPair, PrivateKey, PublicKey}
import cryptic.crypto.EllipticCurve.{ given, * } // Elliptic Curve encryption
import cryptic.codec.Chill.* // Brings the Chill codec in scope

// We need a given public key to enable encryption for Elliptic Curve
private val keyPair: KeyPair = EllipticCurve.keygen(256)
given private val publicKey: PublicKey = keyPair.getPublic

val user = User(123, EmailAddress("odd@example.com").encrypted)
```

To use the Upickle codec:

```scala
import cryptic.codec.Upickle

given def codec[V](using rw: ReadWriter[V]): Codec[V] = Upickle[V]
```

Access your data in encrypted form (can be done without crypto/codec):

```scala
val bytes: Array[Byte] = user.email.bytes
```

Transform your data, can be done without crypto/codec:

```scala
val lowered = user.email.
  map(_.copy(literal = _.literal.toLower))
```

Run your staged transformations, a crypto and a codec must be in scope:

```scala
import cryptic.crypto.RSA.*
import cryptic.codec.Chill.{ given, * }

val user2 = user.copy(email = lowered.run())
```

Decrypt your transformed data, a crypto and a codec must be in scope:

```scala
import cryptic.crypto.RSA.{ given, * }
import cryptic.codec.Fst._

val emailInLower = user2.email.decrypted
```

## Provided cryptos

- AES
- RSA
- ECIES

## Provided codecs

- Default
- Chill
- Fst
- Upickle

## Roll your own

### Codec

Provide an implementation of the Codec trait:

```scala
trait Codec[V]:
  def encode(value: V): PlainText
  def decode(plainText: PlainText): Either[String, V]
```

Example:

```scala

import scala.util.Try

object MyCodec:
  given codec[V]: Codec[V] = new Codec[V]:
    def encode(value: V): PlainText = ???
    def decode(plainText: PlainText): Either[String, V] = ???
```

### Crypto

Provide implementations of the Encrypt and Decrypt traits and probably a Key:

```scala
given encrypt(plainText: PlainText): CipherText
given decrypt(cipherText: CipherText): Either[String, PlainText]
```

Example Caesar crypto:

```scala
object Caesar:
  case class Key(offset: Int):
    require(offset != 0)

  given encrypt(using key: Key): Encrypt = (plainText: PlainText) =>  
    val bytes = plainText.map(b => (b + key.offset).toByte)
    CipherText(bytes)

  given decrypt(using key: Key): Decrypt = (cipherText: CipherText) =>
    Right[String, PlainText](
      PlainText(cipherText.bytes.map(b => (b - key.offset).toByte))
    )

  def keygen(offset: Int): Key = Key(offset)
```