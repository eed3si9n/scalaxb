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
import static org.apache.maven.plugins.annotations.LifecyclePhase.GENERATE_SOURCES;
import static scala.collection.JavaConversions.asScalaBuffer;
import static scala.collection.JavaConversions.seqAsJavaList;
import static scalaxb.compiler.Module$.MODULE$;

import java.io.File;
import java.text.ChoiceFormat;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.Scanner;
import org.sonatype.plexus.build.incremental.BuildContext;

import scala.Option;
import scalaxb.compiler.Arguments;
import scalaxb.compiler.Module;
import scalaxb.compiler.ReferenceNotFound;

/**
 * Generates Scala code from XSD and WSDL.
 */
@Mojo(name = "generate", defaultPhase = GENERATE_SOURCES)
public class ScalaxbMojo extends AbstractScalaxbMojo {

    @Parameter(property = "project", required = true, readonly = true)
    private MavenProject project;

    @Component
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

    /**
     * Build up ('command line') arguments for scalaxb, and invoke the scalaxb
     * compiler.
     * @param schemaFiles The WSDL and XSD input files.
     */
    private void generateBindings(List<String> schemaFiles)
            throws MojoExecutionException, MojoFailureException {
        List<String> arguments = new ArrayList<String>();
        arguments.addAll(arguments());
        arguments.addAll(schemaFiles);

        Option<Arguments> option = Arguments.apply(asScalaBuffer(arguments));
        String args = argumentsToString(arguments);
        if (!option.isDefined()) {
            throw new MojoExecutionException("Error parsing arguments " + args);
        }

        if (getLog().isDebugEnabled()) {
            getLog().debug("Running: scalaxb " + args);
        }
        invokeCompiler(option.get());
    }

    /**
     * Runs scalaxb using the specified arguments, wrapping any exceptions
     * thrown with appropriate Maven exceptions.
     */
    private void invokeCompiler(Arguments args)
            throws MojoExecutionException, MojoFailureException {
        try {
            List<File> generated = generateSources(args);
            if (getLog().isInfoEnabled()) {
                getLog().info(generatedFilesMessage(generated));
            }
            context.refresh(getOutputDirectory());
        } catch (ReferenceNotFound ex) {
            throw new MojoFailureException(ex.getMessage(), ex);
        } catch (Exception ex) {
            throw new MojoExecutionException("Error running scalaxb", ex);
        }
    }

    /**
     * Returns a message indicating the number of files generated, for logging.
     */
    private String generatedFilesMessage(List<File> generated) {
        double[] limits = {0, 1, 2};
        String[] files = {"No files", "1 file", "{0,number} files"};
        ChoiceFormat choice = new ChoiceFormat(limits, files);
        MessageFormat format = new MessageFormat("{0} generated.");
        format.setFormatByArgumentIndex(0, choice);
        return format.format(new Object[]{generated.size()});
    }

    /**
     * Runs scalaxb using the specified arguments.
     */
    private List<File> generateSources(Arguments args) {
        File file = args.files().head();
        boolean verbose = args.verbose();
        Module module = MODULE$.moduleByFileName(file);
        configureLogging(verbose);
        List<File> generated = seqAsJavaList(
                module.processFiles(args.files(), args.config()));
        return generated;
    }

    /**
     * Reconfigures logging so that logging goes through Maven Log.
     * @param verbose True if verbose logging is required.
     */
    private void configureLogging(boolean verbose) {
        LogAppender appender = new LogAppender(getLog());
        Logger rootLogger = Logger.getRootLogger();
        rootLogger.removeAllAppenders();
        rootLogger.addAppender(appender);
        if (!verbose) {
            rootLogger.setLevel(Level.WARN);
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
