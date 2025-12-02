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
  import Enigma.default.given
  import Functor.tryFunctor

  @m(name = "enigma")
  def run(
      @arg(
        name = "decrypt",
        short = 'd',
        doc = "Decrypt input (default is encrypt)"
      ) decryptMode: Flag,
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

    val encryptMode = !decryptMode.value
    settings
      .map(process)
      .flatten
      .fold(reportError, println)

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

    def encodePreamble(using Settings): Try[String] =
      input.flatMap:
        _.encrypted
          .splitWith:
            case IArray(start, key, message) =>
              Success(
                (start.glyph.string, key.glyph.string, message.glyph.string)
              )
          .map:
            case (start, key, message) => s"$start $key ${group5(message)}"

    def decodePreamble(using Settings): Try[String] =
      input.flatMap:
        _.split(" ") match
          case Array(s, k, tail*) =>
            val start = s.glyph.iarray
            val key = k.glyph.iarray
            val message = tail.mkString.glyph.iarray
            Encrypted[Try, String](
              Success(CipherText(start, key, message))
            ).decrypted
          case _ =>
            Failure(
              new IllegalArgumentException(
                "Invalid input format: expected start and key"
              )
            )

    def encode(using Settings): Try[String] =
      input.map: i =>
        group5(Enigma.run(i.glyph).string)

    def decode(using Settings): Try[String] =
      input.map: i =>
        Enigma.run(i.glyph).string

    def process(settings: Settings): Try[String] =
      given s: Settings = settings

      (encryptMode, settings.preamble) match
        case (true, true)   => encodePreamble
        case (true, false)  => encode
        case (false, true)  => decodePreamble
        case (false, false) => decode

    def reportError(throwable: Throwable) =
      Console.err.println(throwable.getMessage)
      Console.err.println(usage())
      sys.exit(1)

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
