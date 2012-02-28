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

import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.slf4j.ILoggerFactory;
import org.slf4j.LoggerFactory;
import org.slf4j.spi.LoggerFactoryBinder;

/**
 * Provides a MavenLoggerFactory instance to the SLF4J LoggerFactory.
 */
public class StaticLoggerBinder implements LoggerFactoryBinder {

    private static final StaticLoggerBinder SINGLETON = new StaticLoggerBinder();

    private final ILoggerFactory loggerFactory;

    /**
     * Private constructor: this is a singleton object and should not be
     * instantiated by clients.
     */
    private StaticLoggerBinder() {
        Log log = MavenLoggerFactory.getLog();
        if (log == null) {
            log = createSystemLogger();
        }
        loggerFactory = new MavenLoggerFactory(log);
    }

    /**
     * Factory method used by SLF4J's {@link LoggerFactory} class.
     * @return The singleton instance of this class.
     */
    public static final StaticLoggerBinder getSingleton() {
        return SINGLETON;
    }

    @Override
    public ILoggerFactory getLoggerFactory() {
        return loggerFactory;
    }

    @Override
    public String getLoggerFactoryClassStr() {
        return MavenLoggerFactory.class.getName();
    }

    private Log createSystemLogger() {
        Log log = new SystemStreamLog();
        log.warn("Logging has not been initialised correctly. "
                + "Mojos should call 'MavenLoggerFactory.setLog(Log)' before "
                + "instantiating an ILogger or ILoggerFactory instance.");
        return log;
    }

}
