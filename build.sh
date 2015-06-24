#!/bin/bash

# NOTE could add -dont-catch-errors to have exceptions pass through catches.
ocamlbuild -cflag -g -lflag -g -tag thread -use-ocamlfind -use-menhir \
  -package core \
  -no-hygiene \
  otto.byte
