#!/bin/bash

obuild build
obuild install
#ocamlfind -query clangml-transforms # check, optional
#ocamlfind -remove clangml-transforms # uninstall command
