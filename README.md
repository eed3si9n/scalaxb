scalaxb
=======

scalaxb is an XML data-binding tool for Scala that supports W3C XML 
Schema (xsd) as the input file.

Status
------

This is still at pre-ALPHA state, and many things don't work.
I'd really appreciate if you could run it against your favorite xsd
file and let me know the result.

Installation
------------

scalaxb is tested only under Scala 2.8. You can install it using sbaz:

    $ sudo sbaz install scalaxb

or build from source:

    $ git clone git://github.com/eed3si9n/scalaxb.git scalaxb
    $ cd scalaxb
    $ sbt sbaz

See the file called INSTALL for details.

Usage
-----

    $ scalaxb [options] <schema_file>...

      -d <directory> | --outdir <directory>
            generated files will go into <directory>
      -p <package> | --package <package>
            specifies the target package
      -p:<namespaceURI>=<package> | --package:<namespaceURI>=<package>
            specifies the target package for <namespaceURI>
      -v | --verbose
            be extra verbose
      <schema_file>
            input schema to be converted

Documents
---------

Further info is available at [scalaxb.org](http://scalaxb.org/).

Example
-------

Suppose you have address.xsd:

    <schema targetNamespace="http://www.example.com/IPO"
            xmlns="http://www.w3.org/2001/XMLSchema"
            xmlns:ipo="http://www.example.com/IPO">
      <complexType name="Address">
        <sequence>
          <element name="name"   type="string"/>
          <element name="street" type="string"/>
          <element name="city"   type="string"/>
        </sequence>
      </complexType>
    </schema>

You then run the following:

    $ scalaxb address.xsd
  
You get address.scala that contains case classes that can convert XML 
documents conforming to the address.xsd into a case class object, and turn it back again
into XML document:

    import org.scalaxb.rt

    case class Address(name: String,
      street: String,
      city: String) extends rt.DataModel {
  
      def toXML(namespace: String, elementLabel: String): scala.xml.Node = {
        var scope: scala.xml.NamespaceBinding = scala.xml.TopScope
        scope = scala.xml.NamespaceBinding("xsi", "http://www.w3.org/2001/XMLSchema-instance", scope)
        scope = scala.xml.NamespaceBinding("ipo", "http://www.example.com/IPO", scope)
        scope = scala.xml.NamespaceBinding(null, "http://www.example.com/IPO", scope)
        val node = toXML(namespace, elementLabel, scope)
        node match {
          case elem: scala.xml.Elem => elem % new scala.xml.PrefixedAttribute("xsi", "type",
            "ipo:Address", elem.attributes)
          case _ => node
        }
      } 
  
      def toXML(namespace: String, elementLabel: String, scope: scala.xml.NamespaceBinding): scala.xml.Node = {
        val prefix = scope.getPrefix(namespace)
        var attribute: scala.xml.MetaData  = scala.xml.Null
        
        scala.xml.Elem(prefix, elementLabel,
          attribute, scope,
          Seq(scala.xml.Elem(prefix, "name", scala.xml.Null, scope, scala.xml.Text(name.toString)),
            scala.xml.Elem(prefix, "street", scala.xml.Null, scope, scala.xml.Text(street.toString)),
            scala.xml.Elem(prefix, "city", scala.xml.Null, scope, scala.xml.Text(city.toString))).flatten: _*)
      }
    }

    object Address extends rt.ElemNameParser[Address] {
      val targetNamespace = "http://www.example.com/IPO"
    
      def parser(node: scala.xml.Node): Parser[Address] =
        (rt.ElemName(targetNamespace, "name")) ~ 
          (rt.ElemName(targetNamespace, "street")) ~ 
          (rt.ElemName(targetNamespace, "city")) ^^
            { case p1 ~ 
          p2 ~ 
          p3 => Address(p1.text,
          p2.text,
          p3.text) }
    }

Bug Reporting
-------------

You can send bug reports to [Issues](http://github.com/eed3si9n/scalaxb/issues),
send me a [message](http://github.com/inbox/new), or email.

Licensing
---------

It's the MIT License. See the file called LICENSE.
     
Contacts
--------

- eed3si9n at gmail dot com
- [@eed3si9n](http://twitter.com/eed3si9n)
