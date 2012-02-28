package org.slf4j.impl;

/*
 * Copyright (c) 2012 Martin Ellis
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

import static org.slf4j.helpers.MessageFormatter.arrayFormat;
import static org.slf4j.helpers.MessageFormatter.format;

import org.apache.maven.plugin.logging.Log;
import org.slf4j.Logger;
import org.slf4j.helpers.FormattingTuple;
import org.slf4j.helpers.MarkerIgnoringBase;

/**
 * SLF4J Logger implementation that delegates to a Maven Log instance.
 */
public class MavenLoggerAdapter extends MarkerIgnoringBase implements Logger {

    private static final long serialVersionUID = 1L;

    private final Log log;

    MavenLoggerAdapter(Log log) {
        super.name = log.getClass().getName();
        this.log = log;
    }

    @Override
    public boolean isTraceEnabled() {
        return log.isDebugEnabled();
    }

    @Override
    public void trace(String msg) {
        log.debug(msg);
    }

    @Override
    public void trace(String format, Object arg) {
        if (log.isDebugEnabled()) {
            debug(format(format, arg));
        }
    }

    @Override
    public void trace(String format, Object arg1, Object arg2) {
        if (log.isDebugEnabled()) {
            debug(format(format, arg1, arg2));
        }
    }

    @Override
    public void trace(String format, Object[] argArray) {
        if (log.isDebugEnabled()) {
            debug(arrayFormat(format, argArray));
        }
    }

    @Override
    public void trace(String msg, Throwable t) {
        log.debug(msg, t);
    }

    @Override
    public boolean isDebugEnabled() {
        return log.isDebugEnabled();
    }

    @Override
    public void debug(String msg) {
        log.debug(msg);
    }

    @Override
    public void debug(String format, Object arg) {
        if (log.isDebugEnabled()) {
            debug(format(format, arg));
        }
    }

    @Override
    public void debug(String format, Object arg1, Object arg2) {
        if (log.isDebugEnabled()) {
            debug(format(format, arg1, arg2));
        }
    }

    @Override
    public void debug(String format, Object[] argArray) {
        if (log.isDebugEnabled()) {
            debug(arrayFormat(format, argArray));
        }
    }

    @Override
    public void debug(String msg, Throwable t) {
        log.debug(msg, t);
    }

    @Override
    public boolean isInfoEnabled() {
        return log.isInfoEnabled();
    }

    @Override
    public void info(String msg) {
        log.info(msg);
    }

    @Override
    public void info(String format, Object arg) {
        if (log.isInfoEnabled()) {
            info(format(format, arg));
        }
    }

    @Override
    public void info(String format, Object arg1, Object arg2) {
        if (log.isInfoEnabled()) {
            info(format(format, arg1, arg2));
        }
    }

    @Override
    public void info(String format, Object[] argArray) {
        if (log.isInfoEnabled()) {
            info(arrayFormat(format, argArray));
        }
    }

    @Override
    public void info(String msg, Throwable t) {
        log.info(msg, t);
    }

    @Override
    public boolean isWarnEnabled() {
        return log.isWarnEnabled();
    }

    @Override
    public void warn(String msg) {
        log.warn(msg);
    }

    @Override
    public void warn(String format, Object arg) {
        if (log.isWarnEnabled()) {
            warn(format(format, arg));
        }
    }

    @Override
    public void warn(String format, Object[] argArray) {
        if (log.isWarnEnabled()) {
            warn(arrayFormat(format, argArray));
        }
    }

    @Override
    public void warn(String format, Object arg1, Object arg2) {
        if (log.isWarnEnabled()) {
            warn(format(format, arg1, arg2));
        }
    }

    @Override
    public void warn(String msg, Throwable t) {
        log.warn(msg, t);
    }

    @Override
    public boolean isErrorEnabled() {
        return log.isErrorEnabled();
    }

    @Override
    public void error(String msg) {
        log.error(msg);
    }

    @Override
    public void error(String format, Object arg) {
        if (log.isErrorEnabled()) {
            error(format(format, arg));
        }
    }

    @Override
    public void error(String format, Object arg1, Object arg2) {
        if (log.isErrorEnabled()) {
            error(format(format, arg1, arg2));
        }
    }

    @Override
    public void error(String format, Object[] argArray) {
        if (log.isErrorEnabled()) {
            error(arrayFormat(format, argArray));
        }
    }

    @Override
    public void error(String msg, Throwable t) {
        log.error(msg, t);
    }

    private void debug(FormattingTuple tuple) {
        Throwable t = tuple.getThrowable();
        if (t == null) {
            log.debug(tuple.getMessage());
        } else {
            log.debug(tuple.getMessage(), t);
        }
    }

    private void info(FormattingTuple tuple) {
        Throwable t = tuple.getThrowable();
        if (t == null) {
            log.info(tuple.getMessage());
        } else {
            log.info(tuple.getMessage(), t);
        }
    }

    private void warn(FormattingTuple tuple) {
        Throwable t = tuple.getThrowable();
        if (t == null) {
            log.warn(tuple.getMessage());
        } else {
            log.warn(tuple.getMessage(), t);
        }
    }

    private void error(FormattingTuple tuple) {
        Throwable t = tuple.getThrowable();
        if (t == null) {
            log.error(tuple.getMessage());
        } else {
            log.error(tuple.getMessage(), t);
        }
    }

}
