package org.scalaxb.maven;

/*
 * Copyright (c) 2011 Martin Ellis
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

import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.spi.LoggingEvent;
import org.apache.log4j.spi.ThrowableInformation;
import org.apache.maven.plugin.logging.Log;

public class LogAppender extends AppenderSkeleton {

    private final Log log;

    public LogAppender(Log log) {
        this.log = log;
    }

    @Override
    protected void append(LoggingEvent event) {
        Level level = event.getLevel();
        if (!levelEnabled(level)) {
            return;
        }

        String message = event.getRenderedMessage();
        ThrowableInformation info = event.getThrowableInformation();
        Throwable throwable = info != null ? info.getThrowable() : null;
        if (message != null) {
            if (throwable == null) {
                log(level, message);
            } else {
                log(level, message, throwable);
            }
        } else {
            if (throwable != null) {
                log(level, throwable);
            }
        }
    }

    private boolean levelEnabled(Level level) {
        final boolean enabled;
        switch (level.toInt()) {
        case Level.TRACE_INT:
        case Level.DEBUG_INT:
            enabled = log.isDebugEnabled();
            break;
        case Level.INFO_INT:
            enabled = log.isInfoEnabled();
            break;
        case Level.WARN_INT:
            enabled = log.isWarnEnabled();
            break;
        case Level.ERROR_INT:
            enabled = log.isErrorEnabled();
            break;
        case Level.FATAL_INT:
            enabled = log.isErrorEnabled();
            break;
        default:
            enabled = false;
            break;
        }
        return enabled;
    }

    private void log(Level level, String message) {
        switch (level.toInt()) {
        case Level.TRACE_INT:
        case Level.DEBUG_INT:
            log.debug(message);
            break;
        case Level.INFO_INT:
            log.info(message);
            break;
        case Level.WARN_INT:
            log.warn(message);
            break;
        case Level.ERROR_INT:
            log.error(message);
            break;
        case Level.FATAL_INT:
            log.error(message);
            break;
        default:
            break;
        }
    }

    private void log(Level level, Throwable throwable) {
        switch (level.toInt()) {
        case Level.TRACE_INT:
        case Level.DEBUG_INT:
            log.debug(throwable);
            break;
        case Level.INFO_INT:
            log.info(throwable);
            break;
        case Level.WARN_INT:
            log.warn(throwable);
            break;
        case Level.ERROR_INT:
            log.error(throwable);
            break;
        case Level.FATAL_INT:
            log.error(throwable);
            break;
        default:
            break;
        }
    }

    private void log(Level level, String message, Throwable throwable) {
        switch (level.toInt()) {
        case Level.TRACE_INT:
        case Level.DEBUG_INT:
            log.debug(message, throwable);
            break;
        case Level.INFO_INT:
            log.info(message, throwable);
            break;
        case Level.WARN_INT:
            log.warn(message, throwable);
            break;
        case Level.ERROR_INT:
            log.error(message, throwable);
            break;
        case Level.FATAL_INT:
            log.error(message, throwable);
            break;
        default:
            break;
        }
    }

    @Override
    public void close() {
        // Nothing to do.
    }

    @Override
    public boolean requiresLayout() {
        return false;
    }
    
}
