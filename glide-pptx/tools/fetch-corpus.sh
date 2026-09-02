#!/usr/bin/env bash
# Downloads a corpus of real .pptx files to test against.
#
# The files are LibreOffice's own pptx regression suite: several hundred decks,
# each written to exercise one awkward corner of the format. They are far better
# at finding bugs than anything we would write ourselves, and they are not
# committed here -- they belong to that project, and `tests/corpus/` is ignored.
#
# Usage: tools/fetch-corpus.sh [count]
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
out="$here/tests/corpus"
count="${1:-400}"
repo="LibreOffice/core"
path="sd/qa/unit/data/pptx"

mkdir -p "$out"
echo "listing $repo/$path ..."
names=$(gh api "repos/$repo/contents/$path" --paginate \
          --jq '.[] | select(.name|test("[.]pptx$")) | .name' | head -n "$count")
n=$(echo "$names" | wc -l | tr -d ' ')
echo "fetching $n decks into $out"

echo "$names" | xargs -P 12 -I {} sh -c \
  'test -s "'"$out"'/{}" || curl -sSL --retry 2 -o "'"$out"'/{}" \
     "https://raw.githubusercontent.com/'"$repo"'/master/'"$path"'/{}"'

echo "$(ls -1 "$out"/*.pptx 2>/dev/null | wc -l | tr -d ' ') decks present"
