#!/usr/bin/env bash

exec cs launch ammonite:2.0.4 --scala 2.13.1 -- \
  "$(dirname "${BASH_SOURCE[0]}")/generate-index.sc" "$@"
