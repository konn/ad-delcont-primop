#!/usr/bin/env bash

set -euo pipefail
rm -rf bench-results
mkdir -p bench-results
BENCH="${1:-$(cabal list-bin ad-delcont-primop-bench)}"

"${BENCH}" -l | cut -d. -f2 | uniq | while read -r GROUP; do
  GROUP_DIR="bench-results/${GROUP}"
  mkdir -p "${GROUP_DIR}"
  I=0
  "${BENCH}" -l -p "/All.${GROUP}/" | cut -d. -f3 | uniq | while read -r CASE; do
    CASE_LABEL="All.${GROUP}.${CASE}"
    CASE_NUM="$(printf "%02d" "${I}")"
    CASE_BASE="${GROUP_DIR}/${CASE_NUM}"
    echo "Saving ${CASE_LABEL} to ${CASE_BASE}"
    "${BENCH}" -j1 -p "/${CASE_LABEL}/" --csv "${CASE_BASE}.csv" --svg "${CASE_BASE}.svg"
    I=$((I + 1))
  done
done