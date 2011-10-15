package org.scalaxb.maven.it;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.internal.TextListener;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;

public class IntegrationTests {

    static File projectDir(String name) {
        try {
            URL url = IntegrationTests.class.getProtectionDomain()
                    .getCodeSource().getLocation();
            File f = new File(url.toURI());
            File basedir = f.getParentFile().getParentFile();
            return new File(basedir, "target/it/" + name);
        } catch (URISyntaxException ex) {
            throw new RuntimeException(ex);
        }
    }

    /**
     * Run post-build tests in a new thread.  This is a simple way of reducing
     * the size of the stack traces reported when tests fail.
     */
    public static void run(final Class<?> test, File f) throws Throwable {
        final AtomicReference<Exception> ex =
                new AtomicReference<Exception>();
        Thread t = new Thread(new Runnable() {
            public void run() {
                ex.set(runTest(test));
            }
        });
        t.start();
        t.join();
        if (ex.get() != null) {
            throw ex.get();
        }
    }

    static Exception runTest(Class<?> test) {
        JUnitCore core = new JUnitCore();
        core.addListener(new TextListener(System.out));
        Result result = core.run(test);
        if (result.wasSuccessful()) {
            return null;
        }
        return new Exception(result.getFailures().get(0).getMessage());
    }

}
