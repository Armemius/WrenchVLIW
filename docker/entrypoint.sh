#!/usr/bin/env sh
set -e

STORAGE_PATH="${STORAGE_PATH:-/data}"
EXAMPLE_BUNDLE="/app/example-reports"

mkdir -p "$STORAGE_PATH"

if [ -d "$EXAMPLE_BUNDLE" ]; then
  cp -r "$EXAMPLE_BUNDLE"/. "$STORAGE_PATH"/ 2>/dev/null || true
fi

exec "$@"
