package org.scalaxb.maven;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonMap;

import java.util.LinkedHashMap;
import java.util.Map;

import junit.framework.TestCase;

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

public class ArgumentsBuilderTest extends TestCase {

    private ArgumentsBuilder builder;

    @Override
    public void setUp() {
        builder = new ArgumentsBuilder();
    }

    public void testSingletonMap() {
        builder.map("-p:", singletonMap("key", "value"));
        assertEquals(asList("-p:key=value"), builder.getArguments());
    }

    public void testMapWithTwoEntries() {
        Map<String, String> map = new LinkedHashMap<String, String>();
        map.put("key1", "value1");
        map.put("key2", "value2");
        builder.map("-p:", map);
        assertEquals(asList("-p:key1=value1", "-p:key2=value2"),
                builder.getArguments());
    }

}
