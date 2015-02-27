#!/bin/bash

ocamlbuild -cflag -g -lflag -g -tag thread -use-ocamlfind -use-menhir \
  -package core \
  -no-hygiene \
  crisp_test.byte

ocamlbuild -cflag -g -lflag -g -tag thread -use-ocamlfind -use-menhir \
  -package core \
  -no-hygiene \
  flick.byte
