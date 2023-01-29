#!/usr/bin/env bash

set -euo pipefail
BENCH="${1:-$(cabal list-bin ad-delcont-primop-bench)}"
RESULTS="${2:-bench-results}"
rm -rf "${RESULTS}"
mkdir -p "${RESULTS}"

"${BENCH}" -l | cut -d. -f2 | uniq | while read -r GROUP; do
  GROUP_DIR="${RESULTS}/${GROUP}"
  mkdir -p "${GROUP_DIR}"
  I=0
  "${BENCH}" -l -p "\$2 == \"${GROUP}\"" | cut -d. -f3 | uniq | while read -r CASE; do
    CASE_LABEL="All.${GROUP}.${CASE}"
    CASE_NUM="$(printf "%02d" "${I}")"
    CASE_BASE="${GROUP_DIR}/${CASE_NUM}"
    echo "Saving ${CASE_LABEL} to ${CASE_BASE}"
    set -x
    "${BENCH}" -j1 -p "\$2 == \"${GROUP}\" && \$3 == \"${CASE}\"" --csv "${CASE_BASE}.csv" --svg "${CASE_BASE}.svg"
    I=$((I + 1))
    set +x
  done
done
