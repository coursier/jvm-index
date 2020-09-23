#!/usr/bin/env bash
set -e

cd "$(git rev-parse --show-toplevel)"

./.github/scripts/cs-setup.sh
echo "::add-path::$(pwd)"
