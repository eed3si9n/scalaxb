package org.scalaxb.maven;

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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Builds the command line argument list that is used when invoking scalaxb.
 */
public class ArgumentsBuilder {

    private final List<String> args = new ArrayList<String>();

    ArgumentsBuilder map(String param, Map<String, String> values) {
        if (values != null) {
            for (Entry<String, String> e : values.entrySet()) {
                String arg = new StringBuilder()
                    .append(param)
                    .append(e.getKey())
                    .append('=')
                    .append(e.getValue())
                    .toString();
                args.add(arg);
            }
        }
        return this;
    }

    /**
     * Adds a parameter to the argument list if the given flag is
     * {@value Boolean#TRUE}. If the flag is false (or null), then the argument
     * list remains unchanged.
     */
    ArgumentsBuilder flag(String param, Boolean value) {
        if (Boolean.TRUE.equals(value)) {
            args.add(param);
        }
        return this;
    }

    /**
     * Adds a parameter to the argument list if the given integer is non-null.
     * If the value is null, then the argument list remains unchanged.
     */
    ArgumentsBuilder param(String param, Integer value) {
        if (value != null) {
            args.add(param);
            args.add(value.toString());
        }
        return this;
    }

    /**
     * Adds a parameter that requires a string argument to the argument list.
     * If the given argument is null, then the argument list remains unchanged.
     */
    ArgumentsBuilder param(String param, String value) {
        if (value != null) {
            args.add(param);
            args.add(value);
        }
        return this;
    }

    /**
     * Adds a parameter that takes a value for each value in a list.
     * For example, <code>intersperse("--foo", Arrays.asList("a", "b"))</code>
     * will add <code>--foo a --foo b</code> to the argument list.
     */
    ArgumentsBuilder intersperse(String param, List<String> values) {
        if (values != null) {
            for (String value : values) {
                args.add(param);
                args.add(value);
            }
        }
        return this;
    }

    List<String> getArguments() {
        return args;
    }

}
