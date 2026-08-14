#!/usr/bin/env sh

set -x -e

./mill --meta-level 1 mill.scalalib.scalafmt/checkFormatAll
# see format_src.sh
# ./mill __.fix --check
./mill mill.scalalib.scalafmt/checkFormatAll