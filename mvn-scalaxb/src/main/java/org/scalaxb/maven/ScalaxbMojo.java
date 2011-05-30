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

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.DirectoryScanner;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import static java.util.Collections.unmodifiableList;

/**
 * @goal generate
 * @phase generate-sources
 */
public class ScalaxbMojo extends AbstractMojo {

    /**
     * @parameter expression="${project}"
     * @required
     * @readonly
     */
    private MavenProject project;

    /**
     * The directory containing the XSD files.
     * @parameter
     *   expression="${scalaxb.xsdDirectory}"
     *   default-value="${project.basedir}/src/main/xsd"
     * @required
     */
    private File xsdDirectory;

    /**
     * The output directory.
     * @parameter
     *   expression="${scalaxb.outputDirectory}"
     *   default-value="${project.build.directory}/generated-sources/scalaxb"
     * @required
     */
    private File outputDirectory;

    /**
     * The package in which to generate classes.
     * @parameter
     *   expression="${scalaxb.packageName}"
     *   default-value="generated"
     */
    private String packageName;

    /**
     * Map of namespace URIs to package names for generated classes.
     * @parameter
     */
    private Map<String, String> packageNames;

    /**
     * The prefix to use on generated classes.
     * @parameter expression="${scalaxb.classPrefix}"
     */
    private String classPrefix;

    /**
     * The prefix to use on generated parameter names.
     * @parameter expression="${scalaxb.parameterPrefix}"
     */
    private String parameterPrefix;

    /**
     * @parameter
     */
    private List<String> wrapContents;

    /**
     *
     * @parameter expression="${scalaxb.verbose}"
     */
    private boolean verbose;

    public void execute() throws MojoExecutionException {
        prepareOutputDirectory();
        List<String> arguments = arguments();
        generate(arguments);

        getLog().debug("Adding source root: " + outputDirectory.getAbsolutePath());
        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
    }

    private void prepareOutputDirectory() {
        outputDirectory.mkdirs();
    }

    private void generate(List<String> arguments) {
        if (getLog().isInfoEnabled()) {
            getLog().info("Running in process: scalaxb " + argumentsToString(arguments));
        }
        String[] args = arguments.toArray(new String[arguments.size()]);
        scalaxb.compiler.Main.main(args);
    }

    /**
     * Formats arguments into a form that can be copied and pasted into the command line.
     */
    private static String argumentsToString(List<String> arguments) {
        Pattern safe = Pattern.compile("[\\p{Alnum}/\\.-]*");
        StringBuilder str = new StringBuilder();
        for (String arg : arguments) {
            if (safe.matcher(arg).matches()) {
                str.append(arg);
            } else {
                str.append('"' + arg.replaceAll("$", "\\$") + '"');
            }
            str.append(' ');
        }
        str.deleteCharAt(str.length() - 1);
        return str.toString();
    }

    private List<String> arguments() {
        List<String> args = new ArrayList<String>();
        if (verbose) {
            args.add("-v");
        }

        args.add("-d");
        args.add(outputDirectory.getPath());

        args.add("-p");
        args.add(packageName);

        if (packageNames != null) {
            for (Map.Entry<String, String> e : packageNames.entrySet()) {
                args.add("-p" + e.getKey() + "=" + e.getValue());
            }
        }

        if (classPrefix != null) {
            args.add("--class-prefix");
            args.add(classPrefix);
        }

        if (parameterPrefix != null) {
            args.add("--param-prefix");
            args.add(parameterPrefix);
        }

        if (wrapContents != null) {
            for (String type : wrapContents) {
                args.add("--wrap-contents");
                args.add(type);
            }
        }

        args.addAll(schemaFiles());
        return unmodifiableList(args);
    }

    private List<String> schemaFiles() {
        DirectoryScanner ds = new DirectoryScanner();
        String[] includes = {"**\\*.xsd"};
        ds.setIncludes(includes);
        ds.setBasedir(xsdDirectory);
        ds.scan();

        List<String> result = new ArrayList<String>();
        for (String xsdFile : ds.getIncludedFiles()) {
            result.add(new File(xsdDirectory, xsdFile).getAbsolutePath());
        }
        return result;
    }

}
