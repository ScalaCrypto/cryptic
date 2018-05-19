# cryptic
Keeps your secrets

## Usage
Import a crypto and a serializer:
```scala
import cryptic.crypto.RSA._
import cryptic.serialization.Fst._
```

Define your data types:
```scala
import cryptic._
case class EmailAddress(literal: String)
case class User(id: Long, email: Encrypted[EmailAddress])
```

Encrypt your data (a crypto and a serializer must be available):
```scala
import cryptic.syntax._
import cryptic.crypto.RSA._
import cryptic.serialization.Fst._

val user = User(123, EmailAddress("Odd@Example.com").encrypted)
```

Access your data in encrypted form:
```scala
val bytes: Array[Byte] = user.email.bytes
```

Transform your data:
```scala
import Cryptic._
val lowered: Operation[EmailAddress] = user.email.
    map(_.copy(literal = _.literal.toLower))
```

Run your staged transformations (a crypto and a serializer must be available):
```scala
val user2 = user.copy(
  email = lowered.run())
```

Decrypt your transformed data (a crypto and a serializer must be available):
```scala
val emailInLower = user2.email.decrypted
```

