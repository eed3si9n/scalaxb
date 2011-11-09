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
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;

public class ScalaxbMojoTest extends AbstractMojoTestCase {

    public void testPackageNameMapIsConfigured() throws Exception {
        ScalaxbMojo mojo = getMojo("packageNames");
        Map<String, String> map = mojo.packageNameMap();

        Iterator<Entry<String, String>> it = map.entrySet().iterator();
        Entry<String, String> maplet1 = it.next();
        assertEquals("http://example.com/namespace1", maplet1.getKey());
        assertEquals("com.example.namespace1", maplet1.getValue());
        Entry<String, String> maplet2 = it.next();
        assertEquals("http://example.com/namespace2", maplet2.getKey());
        assertEquals("com.example.namespace2", maplet2.getValue());
    }

    private ScalaxbMojo getMojo(String project) throws Exception {
        String pomFile = new StringBuilder("src/test/resources/")
            .append(getClass().getPackage().getName().replace('.', '/'))
            .append('/')
            .append(project)
            .append(".xml")
            .toString();
        File pom = getTestFile(pomFile);
        assertNotNull(pom);
        assertTrue(pom.exists());

        ScalaxbMojo mojo = (ScalaxbMojo) lookupMojo("generate", pom);
        assertNotNull(mojo);
        return mojo;
    }
}
