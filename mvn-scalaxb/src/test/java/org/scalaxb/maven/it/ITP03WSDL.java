package org.scalaxb.maven.it;

import org.junit.Test;

public class ITP03WSDL extends AbstractIT {

    private String[] expected = new String[] {
            "generated/stockquote.scala",
            "generated/xmlprotocol.scala",
            "scalaxb/httpclients_dispatch.scala",
            "scalaxb/scalaxb.scala",
            "scalaxb/soap12.scala",
            "soapenvelope12/soapenvelope12.scala",
            "soapenvelope12/soapenvelope12_xmlprotocol.scala"
    };

    @Test
    public void filesAreGeneratedInCorrectLocation() {
        assertFilesGenerated("itp03-wsdl", expected);
    }
    
}
