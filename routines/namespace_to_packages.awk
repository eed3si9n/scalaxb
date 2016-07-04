#!/usr/local/bin/gawk -f
# Searches for namespace definition patterns in the input files.
# Outputs these namespaces' URIs in one column and their packages in
# the other column. The strategy for pacakge name generation is simply
# to concatenate the two fragments of a namespace URI with a dot.

match($0, /xmlns="(http:\/\/schemas\.openxmlformats\.org\/(.+)\/2006\/(.+))\/?"/, m){
  print m[1] " " m[2] "." m[3]
}