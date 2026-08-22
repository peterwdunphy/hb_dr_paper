#!/usr/bin/env bash
# make_diff.sh -- render a track-changes PDF for one manuscript.
#
# Compares ONE version against ONE other version, never a cumulative diff
# across the whole history.
#
#   ./make_diff.sh main              working tree vs the last commit
#   ./make_diff.sh main HEAD~1       the last commit vs the one before it
#   ./make_diff.sh academic HEAD~1
#   ./make_diff.sh appendix
#
# Output: <name>_diff.pdf  (blue underline = added, red strikethrough = deleted)
#
# Note on floats: table, tabular, figure and tikzpicture environments are
# treated as atomic, so their contents are NOT marked up. Marking up inside
# booktabs tables makes latexdiff emit \noalign and \omit in illegal positions
# and the document will not compile. Changed tables therefore appear in their
# new form only; use `git diff` on the .tex to review those line by line.
set -euo pipefail

NAME="${1:-main}"
REF="${2:-HEAD}"
cd "$(dirname "$0")"

command -v latexdiff >/dev/null || { echo "latexdiff not found (ships with TeX Live)"; exit 1; }
[ -f "${NAME}.tex" ] || { echo "no such file: ${NAME}.tex"; exit 1; }

OLD=$(mktemp -t "${NAME}_old").tex
git show "${REF}:paper/${NAME}.tex" > "$OLD" 2>/dev/null \
  || { echo "could not read ${NAME}.tex at ${REF}"; exit 1; }

echo "Diffing ${NAME}.tex: ${REF} -> working tree"
latexdiff --flatten \
  --config "PICTUREENV=(?:picture|tikzpicture|DIFnomarkup|table|table\*|tabular|figure|figure\*)[\w\d@]*" \
  --math-markup=whole \
  "$OLD" "${NAME}.tex" > "${NAME}_diff.tex" 2>/dev/null

pdflatex -interaction=nonstopmode -halt-on-error "${NAME}_diff.tex" >/dev/null 2>&1 || true
pdflatex -interaction=nonstopmode -halt-on-error "${NAME}_diff.tex" >/dev/null 2>&1 || true

rm -f "${NAME}_diff.aux" "${NAME}_diff.log" "${NAME}_diff.out" "${NAME}_diff.toc" "$OLD"
if [ -f "${NAME}_diff.pdf" ]; then
  echo "Wrote ${NAME}_diff.pdf"
  grep -c 'DIFadd{' "${NAME}_diff.tex" | xargs echo "  text additions marked:"
  grep -c 'DIFdel{' "${NAME}_diff.tex" | xargs echo "  text deletions marked:"
else
  echo "PDF was not produced; inspect ${NAME}_diff.tex"; exit 1
fi
