#!/usr/bin/env bash
set -e

cd "$(git rev-parse --show-toplevel)"

./.github/scripts/cs-setup.sh
pwd >> "$GITHUB_PATH"
