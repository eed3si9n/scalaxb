package org.scalaxb.maven.it;

import org.junit.Test;

public class ITP04Packages extends AbstractIT {

    private String[] expected = new String[] {
            "ipo/address.scala",
            "ipo/xmlprotocol.scala",
            "person/person.scala"
    };

    @Test
    public void filesAreGeneratedInCorrectLocation() {
        assertFilesGenerated("itp04-packages", expected);
        assertFilesNotGenerated("itp04-packages", "scalaxb/scalaxb.scala");
    }

}
