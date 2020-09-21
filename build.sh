#!/bin/bash

rm -fr Makefile.* config* *.in scripts/laco aclocal.m4  autom4te.cache 
hall scan -x
hall guix -x
hall dist -x
guix build -f guix.scm --with-source=../laco
