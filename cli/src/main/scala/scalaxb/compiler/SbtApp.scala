package scalaxb.compiler

class SbtApp extends xsbti.AppMain {
  def run(config: xsbti.AppConfiguration) = {
    try {
      Main.start(config.arguments)
      Exit(0)
    }
    catch {
      case e: ReferenceNotFound =>
        println(e.getMessage)
        Exit(1)
      case e: Exception =>
        e.printStackTrace
        Exit(1)
    }
  }

  case class Exit(val code: Int) extends xsbti.Exit
}
