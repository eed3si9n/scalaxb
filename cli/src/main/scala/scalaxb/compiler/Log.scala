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

import org.apache.logging.log4j.{Logger, LogManager, Level}
import org.apache.logging.log4j.core.appender.ConsoleAppender
import org.apache.logging.log4j.core.layout.{PatternLayout, AbstractStringLayout}
import org.apache.logging.log4j.core.LogEvent

case class Log(logger: Logger) {
  def info(message: String, args: Any*): Unit = {
    if (args.isEmpty) logger.info(message)
    else try {
      logger.info(message.format(args: _*))
    }
    catch {
      case _: Throwable => logger.info(message)
    }
  }

  def debug(message: String, args: Any*): Unit = {
    if (args.isEmpty) logger.debug(message)
    else try {
      logger.debug(message.format(args: _*))
    }
    catch {
      case _: Throwable => logger.debug(message)
    }
  }

  def warn(message: String, args: Any*): Unit = {
    if (args.isEmpty) logger.warn(message)
    else try {
      logger.warn(message.format(args: _*))
    }
    catch {
      case _: Throwable => logger.warn(message)
    }
  }

  def error(message: String, args: Any*): Unit = {
    if (args.isEmpty) logger.error(message)
    else try {
      logger.error(message.format(args: _*))
    }
    catch {
      case _: Throwable => logger.error(message)
    }
  }

  def fatal(message: String, args: Any*): Unit = {
    if (args.isEmpty) logger.fatal(message)
    else try {
      logger.fatal(message.format(args: _*))
    }
    catch {
      case _: Throwable => logger.fatal(message)
    }
  }
}

object Log {
  def forName(name: String) = Log(LogManager.getLogger(name))

  def configureLogger(verbose: Boolean): Unit = {
    val root = LogManager.getRootLogger.asInstanceOf[org.apache.logging.log4j.core.Logger]
    val level = if (verbose) Level.TRACE else Level.WARN
    root.setLevel(level)

    val console = ConsoleAppender
      .createDefaultAppenderForLayout(new Formatter())

    console.start()
    root.addAppender(console)
  }

  /**
   * Formats log messages. Prepends a '!' to each line of an exception.
   */
  class Formatter extends AbstractStringLayout(PatternLayout.createDefaultLayout().getCharset) {

    override def toSerializable(event: LogEvent): String = {
      val message = event.getMessage.getFormattedMessage
      val throwable = event.getThrown
      if (throwable == null) {
        message
      } else {
        val msg = new StringBuilder(message)
        for (line <- throwable.getStackTrace) {
          msg.append("! ").append(line.toString).append("\n")
        }
        msg.toString
      }
    }

    override def toByteArray(event: LogEvent): Array[Byte] = {
      toSerializable(event).getBytes(getCharset)
    }
  }
}