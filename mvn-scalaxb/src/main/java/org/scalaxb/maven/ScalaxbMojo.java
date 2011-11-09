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

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.sort;
import static java.util.Collections.unmodifiableList;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.maven.plugin.AbstractMojo;
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
     * The directory containing the WSDL files.
     * @parameter
     *   expression="${scalaxb.wsdlDirectory}"
     *   default-value="${project.basedir}/src/main/wsdl"
     * @required
     */
    private File wsdlDirectory;

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
     * <pre>
     * &lt;packageNames&gt;
     *   &lt;packageName&gt;
     *     &lt;uri&gt;http://example.com/myservice&lt;/uri&gt;
     *     &lt;package&gt;com.example.service&lt;package&gt;
     *   &lt;/packageName&gt;
     * &lt;/packageNames&gt;
     * </pre>
     *
     * @parameter
     */
    private PackageName[] packageNames;

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
     * Generate the scalaxb classes required to use the generated bindings.
     * This option is useful for preventing duplicate copies of the scalaxb
     * runtime being present on the classpath when more than one jar contains
     * scalaxb bindings.  To prevent the scalaxb runtime sources being
     * generated, this option should be set to false.
     * @parameter default-value="true"
     */
    private boolean generateRuntime;

    /**
     * Maximum number of parameters to use in generated case class constructors.
     * This allows parameters sequences to be separated into chunks of the given
     * size.
     * @parameter
     */
    private Integer chunkSize;

   /**
    * Determines whether generated Scala files will be written into a directory
    * corresponding to their package name.  By default, the generated files are
    * written in the output directory under a sub-directory that corresponds to
    * the package name. For example, if the generated classes are in package
    * 'foo', they will be generated in ${scalaxb.outputDirectory}/foo.  Setting
    * this value to false will cause the generated sources to be written
    * directly into the output directory, without creating a directory for the
    * package.
    *
    * @parameter
    *   default-value="true"
    *   expression="${scalaxb.package-dir}"
    */
   private boolean packageDir;

   /**
    * The name of the file to generate that includes the protocol
    * implementation; that is, the code that marshals values to and from XML.
    * @parameter
    *   @default-value="xmlprotocol.scala"
    */
   private String protocolFile;

   /**
    * The package in which to generate the 'protocol' code; that is, the code
    * that marshals values to and from XML. The generated code defines a package
    * object for the named package. The package object defines implicit values
    * required for using the <code>scalaxb.toXML</code> and
    * <code>scalaxb.fromXML</code> functions. If unspecified, the protocol code
    * is generated in the same package as the generated classes that define the
    * values marshalled to and from XML.
    * @parameter
    */
   private String protocolPackage;

   /**
    * Relaxes namespace constraints of <code>xs:any</code>.
    * <br/>
    * This option allows <code>xs:any</code> elements declared with a namespace
    * attribute of <code>##local</code> to contain qualified XML elements.
    * According to the W3C XML Schema recommendation, an XML element that is
    * declared to be in a namespace is not permitted content for an
    * <code>xs:any</code> element with a namespace of <code>##local</code>.
    * By default, this option is false, thus enforcing this requirement. Setting
    * this option to true allows namespaced content to be used.
    *
    * @parameter
    *   default-value="false"
    *   expression="${scalaxb.lax-any}"
    */
   private boolean laxAny;

    /**
     *
     * @parameter expression="${scalaxb.verbose}"
     */
    private boolean verbose;

    public void execute() throws MojoExecutionException, MojoFailureException {
        List<String> schemaFiles = inputFiles(xsdDirectory, "xsd");
        List<String> wsdlFiles = inputFiles(wsdlDirectory, "wsdl");
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

        outputDirectory.mkdirs();
        List<String> arguments = new ArrayList<String>();
        arguments.addAll(arguments());
        arguments.addAll(schemaFiles);

        invokeCompiler(arguments);

        getLog().debug("Adding source root: " + outputDirectory.getAbsolutePath());
        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
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
    private static String argumentsToString(List<String> arguments) {
        Pattern safe = Pattern.compile("[\\p{Alnum}/\\.-]*");
        StringBuilder str = new StringBuilder();
        for (String arg : arguments) {
            if (safe.matcher(arg).matches()) {
                str.append(arg);
            } else {
                String escapedArg = arg.replaceAll("$", "\\$");
                str.append('"').append(escapedArg).append('"');
            }
            str.append(' ');
        }
        str.deleteCharAt(str.length() - 1);
        return str.toString();
    }

    Map<String, String> packageNameMap() {
        if (packageNames == null) {
            return emptyMap();
        }

        Map<String, String> names = new LinkedHashMap<String, String>();
        for (PackageName name : packageNames) {
            names.put(name.getUri(), name.getPackage());
        }
        return names;
    }

    private List<String> arguments() {
        List<String> args = new ArgumentsBuilder()
            .flag("-v", verbose)
            .flag("--package-dir", packageDir)
            .param("-d", outputDirectory.getPath())
            .param("-p", packageName)
            .map("-p:", packageNameMap())
            .param("--class-prefix", classPrefix)
            .param("--param-prefix", parameterPrefix)
            .param("--chunk-size", chunkSize)
            .flag("--no-runtime", !generateRuntime)
            .intersperse("--wrap-contents", wrapContents)
            .param("--protocol-file", protocolFile)
            .param("--protocol-package", protocolPackage)
            .flag("--lax-any", laxAny)
            .getArguments();
        return unmodifiableList(args);
    }

    private List<String> inputFiles(File directory, String type) {
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
