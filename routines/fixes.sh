#!/bin/bash

SOURCES_DIR=$1

# Illegal characters
find $SOURCES_DIR -type f -name *.scala -print0 | xargs -0 sed -i '' -E -e 's/^(package .*)-(.*)$/\1\2/g'
find $SOURCES_DIR -type f -name *.scala -print0 | xargs -0 sed -i '' -E -e 's/(trait .*)-(.* extends)/\1\2/g'
