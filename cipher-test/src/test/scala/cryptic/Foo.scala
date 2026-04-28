package cryptic

import upickle.{macroRW, ReadWriter}

case class Foo(bar: String)

object Foo:
  // Need for upickle
  given rw: ReadWriter[Foo] = macroRW[Foo]
