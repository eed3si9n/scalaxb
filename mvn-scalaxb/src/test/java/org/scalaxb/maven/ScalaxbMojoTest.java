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

import java.io.File;
import java.net.URISyntaxException;
import java.util.List;

import junit.framework.TestCase;

/**
 * ScalaxbMojo unit tests.
 */
public class ScalaxbMojoTest extends TestCase {

    private static final char SEP = File.separatorChar;

    /**
     * The files returned by inputFiles must be returned in alphabetical order,
     * for consistency with sbt-scalaxb.
     * See https://github.com/eed3si9n/scalaxb/issues/110
     */
    public void testInputFilesAreInAlphabeticalOrder() throws Exception {
        File tmp = File.createTempFile("test", "", targetDirectory());
        tmp.delete();
        tmp.mkdir();

        // Create files in non-alphabetical order
        String[] names = {"test2", "test1", "test4", "test3"};
        for (String name : names) {
            new File(tmp, name + ".xsd").createNewFile();
        }

        // Check that they're returned in alphabetical order
        try {
            List<String> files = new ScalaxbMojo().inputFiles(tmp, "xsd");
            assertEquals(4, files.size());
            assertEquals(files.get(0), tmp.getAbsolutePath() + SEP + "test1.xsd");
            assertEquals(files.get(1), tmp.getAbsolutePath() + SEP + "test2.xsd");
            assertEquals(files.get(2), tmp.getAbsolutePath() + SEP + "test3.xsd");
            assertEquals(files.get(3), tmp.getAbsolutePath() + SEP + "test4.xsd");
        } finally {
            // Clean up
            for (String name : names) {
                new File(tmp, name + ".xsd").delete();
            }
            tmp.delete();
        }
    }

    //////////////////////////////////////////////////////////////////////
    // Test utilities

    private static File targetDirectory() {
        return testClassesDirectory().getParentFile();
    }

    private static File testClassesDirectory() {
        try {
            return new File(ScalaxbMojoTest.class
                    .getProtectionDomain()
                    .getCodeSource()
                    .getLocation()
                    .toURI());
        } catch (URISyntaxException ex) {
            throw new RuntimeException(ex);
        }
    }

}
