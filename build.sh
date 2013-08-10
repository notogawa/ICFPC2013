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

#pushd graceful
#cabal-dev install
#popd

#pushd BV-proxy
#rm -rf cabal-dev
#cabal-dev add-source ../graceful
#cabal-dev install
#cp dist/build/BV-proxy/BV-proxy /tmp/proxy
#popd
