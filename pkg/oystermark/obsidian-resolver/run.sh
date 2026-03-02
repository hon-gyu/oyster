#!/bin/sh
# Usage: ./run.sh <vault-name> <output-path>
# Triggers the link-resolver plugin via Obsidian URI and waits for results.
# Obsidian must be running with the vault already open.

set -e

VAULT="${1:?Usage: run.sh <vault-name> <output-path>}"
OUTPUT="${2:?Usage: run.sh <vault-name> <output-path>}"
TIMEOUT="${3:-30}"

VAULT_ENC=$(node -e "process.stdout.write(encodeURIComponent(process.argv[1]))" -- "$VAULT")
OUTPUT_ENC=$(node -e "process.stdout.write(encodeURIComponent(process.argv[1]))" -- "$OUTPUT")

# Remove stale output
rm -f "$OUTPUT"

open "obsidian://link-resolver?vault=${VAULT_ENC}&output=${OUTPUT_ENC}"

elapsed=0
while [ ! -f "$OUTPUT" ]; do
  sleep 0.5
  elapsed=$((elapsed + 1))
  if [ "$elapsed" -ge "$((TIMEOUT * 2))" ]; then
    echo "error: timed out after ${TIMEOUT}s waiting for $OUTPUT" >&2
    exit 1
  fi
done

echo "$OUTPUT"
