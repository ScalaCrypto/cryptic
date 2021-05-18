import scala.concurrent.Future

Future.successful("nisse").filter(_ ⇒ false)
