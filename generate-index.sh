#!/usr/bin/env bash

exec cs launch ammonite:2.4.0 --scala 2.13.6 -- \
  --predef-code 'interp.configureCompiler(_.settings.YtastyReader.value = true)' \
  "$(dirname "${BASH_SOURCE[0]}")/generate-index.sc" "$@"
