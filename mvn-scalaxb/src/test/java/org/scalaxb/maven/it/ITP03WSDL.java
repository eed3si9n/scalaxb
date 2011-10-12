package org.scalaxb.maven.it;

import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Test;

public class ITP03WSDL {

    private String[] expected = new String[] {
            "generated/stockquote.scala",
            "generated/xmlprotocol.scala",
            "scalaxb/httpclients_dispatch.scala",
            "scalaxb/scalaxb.scala",
            "scalaxb/soap.scala",
            "soapenvelope12/soapenvelope12.scala",
            "soapenvelope12/soapenvelope12_xmlprotocol.scala"
    };

    @Test
    public void filesAreGeneratedInCorrectLocation() {
        File basedir = IntegrationTests.projectDir("itp03-wsdl");
        File generated = new File(basedir, "target/generated-sources/scalaxb");
        for (String f : expected) {
            File file = new File(generated, f);
            assertTrue("File was not generated: " + file, file.exists());
        }
    }
    
}
