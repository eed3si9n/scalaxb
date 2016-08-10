/*
 * Copyright (c) 2010 e.e d3si9n
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package scalaxb.compiler

import org.apache.log4j.{Logger, Level, ConsoleAppender, EnhancedPatternLayout}
import org.apache.log4j.spi.LoggingEvent

case class Log(logger: Logger) {

  def formattedMessage(message: String, args: Any*): String =
    if (args.toSeq.isEmpty)          message
    else try {                       message format (args.toSeq: _*)}
         catch {case _: Throwable => message}

  def info (message: String, args: Any*) = logger.info (formattedMessage(message, args))
  def debug(message: String, args: Any*) = logger.debug(formattedMessage(message, args))
  def warn (message: String, args: Any*) = logger.warn (formattedMessage(message, args))
  def error(message: String, args: Any*) = logger.error(formattedMessage(message, args))
  def fatal(message: String, args: Any*) = logger.fatal(formattedMessage(message, args))
}

object Log {
  def forName(name: String) = Log(Logger.getLogger(name))

  def configureLogger(verbose: Boolean) = {

    val root    = Logger.getRootLogger()
    val console = new ConsoleAppender(new Formatter)

    root   .setLevel    (if (verbose) Level.TRACE else Level.WARN)
    console.setThreshold(if (verbose) Level.TRACE else Level.INFO)

    root.addAppender(console)
  }

  /**
   * Formats log messages. Prepends a '!' to each line of an exception.
   */
  class Formatter extends EnhancedPatternLayout("%-5p [%d] %c: %m\n") {

    override def ignoresThrowable = false

    override def format(event: LoggingEvent) : String = {
      val message = super.format(event)
      val frames  = event.getThrowableStrRep()
      if (frames == null) message
      else                frames.map{f => s"! $f \n"}.mkString
    }
  }
}
