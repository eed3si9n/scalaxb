#!/bin/bash

SCHEMAS_DIR=$1
OUTPUT_DIR=$2
MASTER_PACKAGE="msofficeschema"

rm -r $OUTPUT_DIR/*

# First, map the namespace URIs to raw package names using namespace_to_packages.awk - output of the form "URI package"
# Then, produce command line arguments for scalaxb from these mappings - output of the form "--package:URI=package"
PACKAGES=$(\
  gawk -f namespace_to_packages.awk $SCHEMAS_DIR/*.xsd |\
  gawk -v pkg=$MASTER_PACKAGE '{print "--package:" $1 "=" pkg "." $2}'\
)

# Generate the sources
find $SCHEMAS_DIR -type f -name *.xsd -print0 | xargs -0 \
  ./scalaxb -d $OUTPUT_DIR -v \
  $PACKAGES \
  -p $MASTER_PACKAGE \
  --package-dir \
  --attribute-prefix

# Fix the errors
./fixes.sh $OUTPUT_DIR