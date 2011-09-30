package scalaxb.compiler

import com.weiglewilczek.slf4s.Logger

class SbtApp extends xsbti.AppMain {
  lazy val logger = Logger("main")

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
        logger.error(e.getStackTraceString)
        Exit(1)
    }
  }

  case class Exit(val code: Int) extends xsbti.Exit
}
