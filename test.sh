#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
SUITE="${1:-test/suite}"

cd "$ROOT_DIR"
python3 -m runner.runner "$SUITE"