package org.scalaxb.maven;

/*
 * Copyright (c) 2011-2012 Martin Ellis
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

import static java.util.Collections.emptyList;
import static java.util.Collections.sort;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.DirectoryScanner;

import scala.collection.JavaConversions;
import scalaxb.compiler.CaseClassTooLong;
import scalaxb.compiler.ReferenceNotFound;

/**
 * @goal generate
 * @phase generate-sources
 */
public class ScalaxbMojo extends AbstractScalaxbMojo {

    /**
     * @parameter expression="${project}"
     * @required
     * @readonly
     */
    private MavenProject project;

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        List<String> schemaFiles = inputFiles(getXsdDirectory(), "xsd");
        List<String> wsdlFiles = inputFiles(getWsdlDirectory(), "wsdl");
        if (schemaFiles.isEmpty() && wsdlFiles.isEmpty()) {
            getLog().warn("No XSD or WSDL files found: not running scalaxb");
        } else {
            List<String> inputFiles = new ArrayList<String>();
            inputFiles.addAll(wsdlFiles);
            inputFiles.addAll(schemaFiles);
            generateBindings(inputFiles);
        }
    }

    private void generateBindings(List<String> schemaFiles)
            throws MojoExecutionException, MojoFailureException {

        getOutputDirectory().mkdirs();
        List<String> arguments = new ArrayList<String>();
        arguments.addAll(arguments());
        arguments.addAll(schemaFiles);

        invokeCompiler(arguments);

        String outputPath = getOutputDirectory().getAbsolutePath();
        getLog().debug("Adding source root: " + outputPath);
        project.addCompileSourceRoot(outputPath);
    }

    private void invokeCompiler(List<String> arguments)
            throws MojoExecutionException, MojoFailureException {

        if (getLog().isInfoEnabled()) {
            getLog().info("Running in process: scalaxb " + argumentsToString(arguments));
        }

        try {
            scalaxb.compiler.Main.start(JavaConversions.asScalaBuffer(arguments));
        } catch (ReferenceNotFound ex) {
            throw new MojoFailureException(ex.getMessage());
        } catch (CaseClassTooLong ex) {
            throw new MojoFailureException(ex.getMessage());
        } catch (Exception ex) {
            throw new MojoExecutionException("Error running scalaxb", ex);
        }
    }

    /**
     * Formats arguments into a form that can be copied and pasted into the command line.
     */
    static String argumentsToString(List<String> arguments) {
        Pattern safe = Pattern.compile("[\\p{Alnum}:/=\\.-]*");
        StringBuilder str = new StringBuilder();
        for (String arg : arguments) {
            if (safe.matcher(arg).matches()) {
                str.append(arg);
            } else {
                String escapedArg = arg.replaceAll("'", "'\\\\''");
                str.append('\'').append(escapedArg).append('\'');
            }
            str.append(' ');
        }
        str.deleteCharAt(str.length() - 1);
        return str.toString();
    }

    List<String> inputFiles(File directory, String type) {
        if (!directory.exists()) {
            return emptyList();
        }

        DirectoryScanner ds = new DirectoryScanner();
        String[] includes = {"**\\*." + type};
        ds.setIncludes(includes);
        ds.setBasedir(directory);
        ds.scan();

        List<String> result = new ArrayList<String>();
        for (String xsdFile : ds.getIncludedFiles()) {
            result.add(new File(directory, xsdFile).getAbsolutePath());
        }
        sort(result);
        return result;
    }

}
