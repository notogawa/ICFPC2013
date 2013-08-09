#!/bin/bash

set -e

while true
do
  time BV-infer/dist/build/BV-infer/BV-infer +RTS -N12
  echo
  sleep 20
done
