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

Example
-------

Suppose you have address.xsd:

    <xs:schema targetNamespace="http://www.example.com/IPO"
            xmlns="http://www.example.com/IPO"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:ipo="http://www.example.com/IPO">
      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="name"   type="xs:string"/>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city"   type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="USAddress">
        <xs:complexContent>
          <xs:extension base="ipo:Address">
            <xs:sequence>
              <xs:element name="state" type="xs:string"/>
              <xs:element name="zip"   type="xs:positiveInteger"/>
            </xs:sequence>
          </xs:extension>
        </xs:complexContent>
      </xs:complexType>
    </xs:schema>

You then run the following:

    $ scalaxb address.xsd
  
You get address.scala that contains case classes that can parse XML 
documents conforming to the address.xsd:

    case class Address(name: String,
      street: String,
      city: String) extends DataModel with Addressable {
    }

    object Address {
      def fromXML(node: scala.xml.Node): Address =
        Address((node \ "name").text,
          (node \ "street").text,
          (node \ "city").text) 
    }

    trait Addressable {
      val name: String;
      val street: String;
      val city: String;
    }

    object Addressable {
      def fromXML(node: scala.xml.Node): Addressable = {
        val typeName = (node \ "@{http://www.w3.org/2001/XMLSchema-instance}type").text    
        val namespace = if (typeName.contains(':'))
          node.scope.getURI(typeName.dropRight(typeName.length - typeName.indexOf(':')))
        else
          node.scope.getURI(null)
      
        val value = if (typeName.contains(':'))
          typeName.drop(typeName.indexOf(':') + 1)
        else
          typeName
    
        (namespace, value) match {
          case ("http://www.example.com/IPO", "USAddress") => USAddress.fromXML(node)
          case _ => Address.fromXML(node)
        }
      }
    }

    case class USAddress(name: String,
      street: String,
      city: String,
      state: String,
      zip: Int) extends DataModel with Addressable {
    }

    object USAddress {
      def fromXML(node: scala.xml.Node): USAddress =
        USAddress((node \ "name").text,
          (node \ "street").text,
          (node \ "city").text,
          (node \ "state").text,
          (node \ "zip").text.toInt) 
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
