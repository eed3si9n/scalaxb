package scalaxb.compiler

class SbtApp extends xsbti.AppMain {
  private val logger = Log.forName("main")

  def run(config: xsbti.AppConfiguration) = {
    try {
      Main.start(config.arguments)
      Exit(0)
    }
    catch {
      case e: ReferenceNotFound =>
        logger.error(e.getMessage)
        Exit(1)
      case e: CaseClassTooLong =>
        logger.error(e.getMessage)
        Exit(1)
      case e: Exception =>
        logger.error(e.getStackTrace.mkString("", Module.NL, Module.NL))
        Exit(1)
    }
  }

  case class Exit(val code: Int) extends xsbti.Exit
}
