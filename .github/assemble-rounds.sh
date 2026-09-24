#!/usr/bin/env bash
# Assembles the archive of closed rounds under <outdir>/round/, from the
# assets of their GitHub releases, and builds the round index. Test rounds
# are deliberately not listed.
#
# Usage: assemble-rounds.sh <outdir>
set -euo pipefail

out="$1/round"
downloads=$(mktemp -d)
mkdir -p "$out"
# No match before the first round is closed; pipefail would make that an
# error rather than an empty archive
tags=$(gh release list --limit 1000 --json tagName --jq '.[].tagName' \
  | { grep -E '^round-[0-9]{4}-[0-9]{2}$' || true; })
for tag in $tags; do
  round="${tag#round-}"
  prefix="lean-arena-round-$round"
  echo "::group::Round $round"
  gh release download "$tag" --dir "$downloads/$round"
  mkdir -p "$out/$round"
  tar -xzf "$downloads/$round/$prefix-site.tar.gz" \
    -C "$out/$round" --strip-components=1
  # The archived site links to these relatively and under fixed names, so put
  # them back next to it, rather than only offering them under their
  # round-qualified names on the release
  cp "$downloads/$round/$prefix-results.json" "$out/$round/results.json"
  cp "$downloads/$round/$prefix-tests.tar.gz" "$out/$round/lean-arena-tests.tar.gz"
  echo "::endgroup::"
done
./lka.py build-rounds-index --outdir "$out"
