#!/bin/bash

set -e
touch continue
while test -f continue
do
  BV-infer/dist/build/BV-infer/BV-infer +RTS -N8 -M30G -RTS
  echo
done
