package org.scalaxb.maven.it;

import org.junit.Test;

public class ITP03WSDL extends AbstractIT {

    private String[] expected = new String[] {
            "generated/stockquote.scala",
            "generated/stockquote_type1.scala",
            "generated/xmlprotocol.scala",
            "scalaxb/execution_context_provider.scala",
            "scalaxb/httpclients_async.scala",
            "scalaxb/httpclients_dispatch_async.scala",
            "scalaxb/scalaxb.scala",
            "scalaxb/soap12_async.scala",
            "soapenvelope12/soapenvelope12.scala",
            "soapenvelope12/soapenvelope12_xmlprotocol.scala"
    };

    @Test
    public void filesAreGeneratedInCorrectLocation() {
        assertFilesGenerated("itp03-wsdl", expected);
    }
    
}
