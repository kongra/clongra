#! /bin/bash

ant clean
ant
rm -rf target/
lein compile
