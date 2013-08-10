#!/bin/bash

set -e
touch continue
while test -f continue
do
  BV-infer/dist/build/BV-infer/BV-infer +RTS -N12 -M30G -RTS
  echo
done
