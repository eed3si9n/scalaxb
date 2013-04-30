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

import static java.util.Arrays.asList;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import junit.framework.TestCase;

import org.codehaus.classworlds.ClassRealm;
import org.codehaus.classworlds.ClassWorld;
import org.codehaus.plexus.component.configurator.BasicComponentConfigurator;
import org.codehaus.plexus.component.configurator.ComponentConfigurator;
import org.codehaus.plexus.component.configurator.expression.DefaultExpressionEvaluator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.configuration.PlexusConfiguration;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomBuilder;
import org.sonatype.plexus.build.incremental.DefaultBuildContext;

/**
 * ScalaxbMojo unit tests.
 */
public class ScalaxbMojoTest extends TestCase {

    private static final char SEP = File.separatorChar;

    /**
     * Arguments that don't need to be escaped should be returned as-is.
     * Other arguments should be enclosed in single quotes, which must be
     * escaped.
     */
    public void testArgumentsToString() {
        expect("-p:http://example.com/S1=f", "-p:http://example.com/S1=f");
        expect("'-pfoo$bar'", "-pfoo$bar");
        expect("'a'\\''x'", "a'x");
    }

    private void expect(String expect, String... arguments) {
        assertEquals(expect, ScalaxbMojo.argumentsToString(asList(arguments)));
    }

    /**
     * Test URI to package name mapping is read from configuration correctly.
     * See https://github.com/eed3si9n/scalaxb/issues/111
     */
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

    public void testDispatchVersionIsConfigured() throws Exception {
        ScalaxbMojo mojo = getMojo("dispatchVersion");
        List<String> args = mojo.arguments();
        int i = args.indexOf("--dispatch-version");
        assertTrue(i > 0);
        assertEquals("0.10.0", args.get(i + 1));
    }

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
            ScalaxbMojo mojo = getMojo();
            List<String> files = mojo.inputFiles(tmp, "xsd");
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

    private ScalaxbMojo getMojo() throws Exception {
        ScalaxbMojo mojo = new ScalaxbMojo();
        Field ctxtField = ScalaxbMojo.class.getDeclaredField("context");
        ctxtField.setAccessible(true);
        ctxtField.set(mojo, new DefaultBuildContext());
        return mojo;
    }

    private ScalaxbMojo getMojo(String project) throws Exception {
        File pom = new File(getClass().getResource(project + ".xml").toURI());
        assertTrue("Couldn't find " + pom, pom.exists());
        ScalaxbMojo mojo = getMojo();
        configureMojo(mojo, new FileInputStream(pom));
        return mojo;
    }

    private void configureMojo(Object mojo, InputStream is) throws Exception {
        ComponentConfigurator configurator = new BasicComponentConfigurator();
        ExpressionEvaluator evaluator = new DefaultExpressionEvaluator();
        Xpp3Dom dom = Xpp3DomBuilder.build(is, "UTF-8")
                .getChild("build")
                .getChild("plugins")
                .getChild("plugin")
                .getChild("configuration");
        PlexusConfiguration config = new XmlPlexusConfiguration(dom);
        ClassRealm realm = new ClassWorld()
                .newRealm(null, getClass().getClassLoader());
        configurator.configureComponent(mojo, config, evaluator, realm);
    }

}
