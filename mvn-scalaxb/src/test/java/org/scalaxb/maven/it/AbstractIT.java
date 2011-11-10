package org.scalaxb.maven.it;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

abstract class AbstractIT {

    public void assertFilesGenerated(String project, String... expected) {
        File basedir = IntegrationTests.projectDir(project);
        File generated = new File(basedir, "target/generated-sources/scalaxb");
        for (String f : expected) {
            File file = new File(generated, f);
            assertTrue("File was not generated: " + file, file.exists());
        }
    }

    public void assertFilesNotGenerated(String project, String... expected) {
        File basedir = IntegrationTests.projectDir(project);
        File generated = new File(basedir, "target/generated-sources/scalaxb");
        for (String f : expected) {
            File file = new File(generated, f);
            assertFalse("File was generated erroneously: " + file, file.exists());
        }
    }

}
