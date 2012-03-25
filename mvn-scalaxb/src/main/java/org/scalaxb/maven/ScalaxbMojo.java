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
import org.codehaus.plexus.util.Scanner;
import org.sonatype.plexus.build.incremental.BuildContext;

import scala.collection.JavaConversions;
import scala.collection.mutable.Buffer;
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

    /**
     * @component
     */
    private BuildContext context;

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        getOutputDirectory().mkdirs();
        String outputPath = getOutputDirectory().getAbsolutePath();
        getLog().debug("Adding source root: " + outputPath);
        project.addCompileSourceRoot(outputPath);

        List<String> inputFiles = inputFiles();
        if (!inputFiles.isEmpty() && buildRequired(inputFiles)) {
            generateBindings(inputFiles);
        }
    }

    public List<String> inputFiles() {
        List<String> schemaFiles = inputFiles(getXsdDirectory(), "xsd");
        List<String> wsdlFiles = inputFiles(getWsdlDirectory(), "wsdl");
        if (schemaFiles.isEmpty() && wsdlFiles.isEmpty()) {
            if (!context.isIncremental()) {
                getLog().warn("No XSD or WSDL files found: not running scalaxb");
            }
            return emptyList();
        }

        List<String> inputFiles = new ArrayList<String>();
        inputFiles.addAll(wsdlFiles);
        inputFiles.addAll(schemaFiles);
        return inputFiles;
    }

    /**
     * Returns true if scalaxb should be run. This method returns true for CLI
     * builds, or full builds in the IDE. It also returns true for IDE builds
     * where any of the input files are outside the project directory.
     * This method returns false if this is an incremental build, and none of
     * the input files have changed.
     * <p>
     * @param inputFiles The XSD and WSDL files.
     * @return True if a build is required, and false otherwise.
     */
    public boolean buildRequired(List<String> inputFiles) {
        if (!context.isIncremental()) {
            return true;
        }
        String basedir = project.getBasedir().toString() + File.separatorChar;
        boolean changes = false;
        for (String file : inputFiles) {
            if (file.startsWith(basedir)) {
                String relPath = file.substring(basedir.length());
                if (context.hasDelta(relPath)) {
                    changes = true;
                    break;
                }
            }
        }
        return changes;
    }

    private void generateBindings(List<String> schemaFiles)
            throws MojoExecutionException, MojoFailureException {
        List<String> arguments = new ArrayList<String>();
        arguments.addAll(arguments());
        arguments.addAll(schemaFiles);
        invokeCompiler(arguments);
    }

    private void invokeCompiler(List<String> arguments)
            throws MojoExecutionException, MojoFailureException {

        if (getLog().isInfoEnabled()) {
            getLog().info("Running in process: scalaxb " + argumentsToString(arguments));
        }

        try {
            Buffer<String> args = JavaConversions.asScalaBuffer(arguments);
            scalaxb.compiler.Main.start(args);
            context.refresh(getOutputDirectory());
        } catch (ReferenceNotFound ex) {
            throw new MojoFailureException(ex.getMessage(), ex);
        } catch (CaseClassTooLong ex) {
            throw new MojoFailureException(ex.getMessage(), ex);
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

    /**
     * Returns the path of all files in a directory (or its subdirectories) with
     * a given extension. The returned File objects are prefixed with the given
     * directory.
     * <p>
     * @param directory The directory in which to search.
     * @param type The required file extension.
     * @return A list of all files contained in the directory with the specified
     *         extension.
     */
    List<String> inputFiles(File directory, String type) {
        if (!directory.exists()) {
            return emptyList();
        }

        Scanner ds = context.newScanner(directory, true);
        String[] includes = {"**\\*." + type};
        ds.setIncludes(includes);
        ds.scan();

        List<String> result = new ArrayList<String>();
        for (String file : ds.getIncludedFiles()) {
            result.add(new File(directory, file).getPath());
        }

        sort(result);
        return result;
    }

}
