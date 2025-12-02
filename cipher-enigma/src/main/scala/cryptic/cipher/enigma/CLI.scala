package cryptic
package cipher
package enigma

import scala.io.Source
import scala.util.{Failure, Success, Try}

object CLI:
  private def usage(): String =
    """
      |Usage: Enigma [-d] [-s SETTINGS] [TEXT | -f FILE]
      |
      |  -d            Decrypt input (encryption is default)
      |  -s, --settings SETTINGS
      |                One string: "names rings [positions] reflector [plugpairs]"
      |                Optional if ENIGMA_SETTINGS is set in the environment. The flag overrides.
      |                Example: "III-II-I AAA AAZ B ABCD"
      |                If no positions are given a preamble will be generated when
      |                encrypting and is expected in the message when decrypting
      |  TEXT          Optional second argument containing the message text
      |  -f FILE       Read message text from FILE instead of TEXT or stdin
      |
      |Input text is normalized to letters A–Z (non-letters are ignored, lowercase allowed).
      |Encrypt output is grouped into five-letter groups. Decrypt output is a continuous string.
      |""".stripMargin

  private def readAllStdin(): String = Source.stdin.slurp

  private def readFile(path: String): String = Source.fromFile(path).slurp

  private def group5(s: String): String = s.grouped(5).mkString(" ")

  import mainargs.{ParserForMethods, Flag, arg, main as m}

  @m(name = "enigma")
  def run(
      @arg(
        name = "decrypt",
        short = 'd',
        doc = "Decrypt input (default is encrypt)"
      ) decrypt: Flag,
      @arg(
        name = "settings",
        short = 's',
        doc =
          "\"names rings [positions] reflector [plug-pairs]\" (can also be provided via ENIGMA_SETTINGS)"
      )
      settingsStr: Option[String],
      @arg(name = "f", short = 'f', doc = "Read message text from FILE")
      file: Option[String],
      @arg(doc = "Optional message TEXT (otherwise stdin)")
      text: String*
  ): Unit =

    val encryptMode = !decrypt.value

    import Enigma.default.{given, *}
    import Functor.tryFunctor

    def settings: Try[Settings] =
      settingsStr
        .orElse(sys.env.get("ENIGMA_SETTINGS"))
        .fold[Try[Settings]](
          Failure(
            new IllegalArgumentException(
              "Missing settings: use --settings or set ENIGMA_SETTINGS"
            )
          )
        )(Settings.parse)

    def input: Try[String] =
      Try:
        file
          .map(readFile)
          .orElse(if text.nonEmpty then Some(text.mkString(" ")) else None)
          .getOrElse(readAllStdin())

    def encodePreamble(input: String)(using Settings): Try[String] =
      input.encrypted
        .splitWith:
          case IArray(start, key, message) =>
            Success(
              (glyph(start).string, glyph(key).string, glyph(message).string)
            )
        .map:
          case (start, key, message) =>
            s"$start $key ${group5(message)}"

    def decodePreamble(input: String)(using Settings): Try[String] =
      val arr = input.split(" ")
      val start = arr(0).glyph
      val key = arr(1).glyph
      val message = arr.drop(2).mkString.glyph
      val enc = Encrypted[Try, String](
        Success(CipherText(start.iarray, key.iarray, message.iarray))
      )
      enc.decrypted

    def encode(input: String)(using Settings): Try[String] =
      Try(group5(Enigma.run(input.glyph).string))

    def decode(input: String)(using Settings): Try[String] =
      Try(Enigma.run(input.glyph).string)

    def selectOperation(input: String)(using settings: Settings): Try[String] =
      val op = (encryptMode, settings.preamble) match
        case (true, true)   => encodePreamble
        case (true, false)  => encode
        case (false, true)  => decodePreamble
        case (false, false) => decode
      op(input)

    val result = for
      s <- settings
      i <- input
      result <- selectOperation(i)(using s)
    yield result
    result.fold(
      throwable =>
        Console.err.println(throwable.getMessage)
        Console.err.println(usage())
        sys.exit(1)
      ,
      println
    )

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
