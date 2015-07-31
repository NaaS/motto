#!/bin/bash

DEFAULT_TARGET=motto.byte

if [ -z $1 ]
then
  TARGET=${DEFAULT_TARGET}
else
  TARGET="${1}"
fi

echo "building ${TARGET}"

# NOTE could add -dont-catch-errors to have exceptions pass through catches.
ocamlbuild -cflag -g -lflag -g -tag thread -use-ocamlfind -use-menhir \
  -package dynlink \
  -no-hygiene \
  -I front-end \
  -I back-end \
  -I general \
  -I runtime \
  -I syntax \
  -I il \
  -I tests/runtime \
  ${TARGET}
