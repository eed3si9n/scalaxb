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

import static java.util.Collections.emptyMap;
import static java.util.Collections.unmodifiableList;

import java.io.File;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Parameter;

public abstract class AbstractScalaxbMojo extends AbstractMojo {

    /**
     * The directory containing the XSD files. If the specified directory does
     * not exist or is empty, then it is ignored.
     */
    @Parameter(property = "scalaxb.xsdDirectory",
            defaultValue = "${project.basedir}/src/main/xsd",
            required = true)
    private File xsdDirectory;

    /**
     * The directory containing the WSDL files. If the specified directory does
     * not exist or is empty, then it is ignored.
     */
    @Parameter(property = "scalaxb.wsdlDirectory",
            defaultValue = "${project.basedir}/src/main/wsdl",
            required = true)
    private File wsdlDirectory;

    /**
     * The output directory.
     */
    @Parameter(property = "scalaxb.outputDirectory",
            defaultValue = "${project.build.directory}/generated-sources/scalaxb",
            required = true)
    private File outputDirectory;

    /**
     * The package in which to generate classes. Classes are generated in the
     * package specified, unless the <code>packageNames</code> parameter is used
     * to override this value.
     */
    @Parameter(property = "scalaxb.packageName",
            defaultValue = "generated")
    private String packageName;

    /**
     * Map of namespace URIs to package names for generated classes.
     * <br/>
     * This option can be used to override the <code>packageName</code>
     * parameter (see above) for elements in specific namespaces. The mapping
     * between namespace URIs and package names can be specifying any number of
     * <code>packageName</code> elements within the <code>packageNames</code>
     * element. For example:
     * <pre>
     * &lt;packageNames&gt;
     *   &lt;packageName&gt;
     *     &lt;uri&gt;http://example.com/service1&lt;/uri&gt;
     *     &lt;package&gt;com.example.service1&lt;package&gt;
     *   &lt;/packageName&gt;
     *   &lt;packageName&gt;
     *     &lt;uri&gt;http://example.com/service2&lt;/uri&gt;
     *     &lt;package&gt;com.example.service2&lt;package&gt;
     *   &lt;/packageName&gt;
     * &lt;/packageNames&gt;
     * </pre>
     */
    @Parameter
    private PackageName[] packageNames;

    /**
     * The prefix to use on generated classes.
     */
    @Parameter(property = "scalaxb.classPrefix")
    private String classPrefix;

    /**
     * The prefix to use on generated parameter names.
     */
    @Parameter(property = "scalaxb.parameterPrefix")
    private String parameterPrefix;

    /**
     * Wraps the content of specified complex types into separate case classes.
     * <br/>
     * By default, when a type inherits from a complex type, the case class
     * generated for that type will include fields for the content of the
     * inherited type. With some XML schemas, this cause case classes with many
     * fields to be generated. Many fields can cause case classes to become
     * inconvenient to use, and there is a limit to the number of fields that
     * can appear in a case class (see <code>chunkSize</code>).
     * <br/>
     * The <code>&lt;wrapContents&gt;</code> option causes a separate case class
     * to be generated for the named complex types. Where a type inherits from
     * the complex type, it will have a reference to the case class for that
     * complex type, instead of a number of fields for the content of that
     * complex type.
     * <br/>
     * This option is useful when inheriting from complexTypes would cause a
     * case class to contain too many fields. It may be a better alternative to
     * using the <code>chunkSize</code> option, because that simply limits the
     * number of fields in every generated case class. In contrast, this option
     * provides more fine-grained control: it allows related fields to be
     * grouped for a given set of types.
     * <br/>
     * For example, the following will generate a separate case class for the
     * complex type named in the &lt;wrapContent&gt; element.
     * <pre>
     * &lt;wrapContents&gt;
     *   &lt;wrapContent&gt;{http://example.com/schema}ComplexType&lt;/wrapContent&gt;
     * &lt;/wrapContents&gt;
     * </pre>
     */
    @Parameter
    private List<String> wrapContents;

    /**
     * Generate the scalaxb classes required to use the generated bindings.
     * This option is useful for preventing duplicate copies of the scalaxb
     * runtime being present on the classpath when more than one jar contains
     * scalaxb bindings.  To prevent the scalaxb runtime sources being
     * generated, this option should be set to false.
     */
    @Parameter(defaultValue = "true")
    private boolean generateRuntime;

    /**
     * Maximum number of parameters to use in generated case class constructors.
     * This allows parameters sequences to be separated into chunks of the given
     * size.
     */
    @Parameter
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
    */
    @Parameter(property = "scalaxb.package-dir",
            defaultValue = "true")
    private boolean packageDir;

   /**
    * The name of the file to generate that includes the protocol
    * implementation; that is, the code that marshals values to and from XML.
    */
    @Parameter(defaultValue = "xmlprotocol.scala")
    private String protocolFile;

   /**
    * The package in which to generate the 'protocol' code; that is, the code
    * that marshals values to and from XML. The generated code defines a package
    * object for the named package. The package object defines implicit values
    * required for using the <code>scalaxb.toXML</code> and
    * <code>scalaxb.fromXML</code> functions. If unspecified, the protocol code
    * is generated in the same package as the generated classes that define the
    * values marshalled to and from XML.
    */
    @Parameter
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
    */
    @Parameter(property = "scalaxb.lax-any",
            defaultValue = "false")
    private boolean laxAny;

   /**
    * Prefix to prepend to the names of generated parameters for XML attributes.
    * <br/>
    * This option sets a prefix to be used in the names of parameters for XML
    * attributes. It is useful when a schema defines both an element and an
    * attribute of the same name within a complex type.
    */
    @Parameter(property = "scalaxb.attributePrefix")
    private String attributePrefix;

    @Parameter(property = "scalaxb.verbose")
    private boolean verbose;

    /**
     * Returns the directory in which to search for XSD schema files.
     * @return The directory containing the XSD files.
     */
    public File getXsdDirectory() {
        return xsdDirectory;
    }

    /**
     * Returns the directory in which to search for WSDL files.
     * @return The directory containing WSDL files.
     */
    public File getWsdlDirectory() {
        return wsdlDirectory;
    }

    /**
     * Returns the directory into which generated sources should be written.
     * @return The output directory.
     */
    public File getOutputDirectory() {
        return outputDirectory;
    }

    /**
     * Returns the command line options to be used for scalaxb, excluding the
     * input file names.
     */
    protected List<String> arguments() {
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
            .param("--attribute-prefix", attributePrefix)
            .flag("--lax-any", laxAny)
            .getArguments();
        return unmodifiableList(args);
    }

    /**
     * Returns a map of URIs to package name, as specified by the packageNames
     * parameter.
     */
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

}
