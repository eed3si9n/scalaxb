package org.scalaxb.maven.it;

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

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Pattern;

import org.codehaus.plexus.util.IOUtil;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests that the help mojo is generated and included in the plugin descriptor.
 */
public class ITP05Help extends AbstractIT {

    private static String output;

    @BeforeClass
    public static void setUp() throws IOException {
        File basedir = IntegrationTests.projectDir("itp05-help");
        File buildLog = new File(basedir, "build.log");
        InputStream is = new FileInputStream(buildLog);
        output = IOUtil.toString(is);
    }

    @Test
    public void helpMojoDescribesPlugin() {
        Pattern goals = Pattern.compile("This plugin has 2 goals:");
        assertTrue("Help should show how many goals are available",
                goals.matcher(output).find());
    }

}
