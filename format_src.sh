#!/usr/bin/env sh

set -x -e

./mill --meta-level 1 mill.scalalib.scalafmt/
# TODO: into syntax is preview, and ScalaFix does not pass the option properly
# ./mill __.fix
./mill mill.scalalib.scalafmt/
