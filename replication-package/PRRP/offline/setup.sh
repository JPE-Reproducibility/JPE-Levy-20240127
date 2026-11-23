#!/bin/bash
set -euo pipefail
cd -- "$(dirname -- "$0")/.."
if [[ "$(uname -s)" != Darwin || "$(uname -m)" != arm64 ]]; then
  echo 'This offline bundle supports Apple Silicon macOS only.' >&2
  exit 1
fi
if [[ ! -d offline/runtime ]]; then
  tar -xzf offline/R-4.6.1-arm64-runtime.tar.gz -C offline
fi
mkdir -p offline/empty-library
bash offline/Rscript --vanilla offline/setup.R
