#!/bin/bash

set -e

while true
do
  BV-infer/dist/build/BV-infer/BV-infer +RTS -N8 -M30G -RTS
  echo
done
