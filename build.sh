#!/bin/bash

set -e

pushd language-BV
cabal-dev install
popd

pushd BV-infer
rm -rf cabal-dev
cabal-dev add-source ../language-BV
cabal-dev install
popd
