package scalaxb.compiler

import ch.qos.logback.classic.pattern.ClassicConverter
import ch.qos.logback.classic.spi.ILoggingEvent

class LogLevelConverter extends ClassicConverter {
  override def convert(event: ILoggingEvent): String = event.getLevel.toString.toLowerCase
}
