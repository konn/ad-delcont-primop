#!/usr/bin/env bash
set -euxo pipefail

DEST=${1:-artifacts.tar.zst}

wget -O- https://github.com/haskell-hvr/cabal-plan/releases/download/v0.6.2.0/cabal-plan-0.6.2.0-x86_64-linux.xz \
  | xz -d - > cabal-plan
chmod +x cabal-plan
BIN_DIR=bins
mkdir -p "${BIN_DIR}/bin"
./cabal-plan list-bins  | \
  grep -e :test: -e :bench: | awk '{ print $2 }' | \
  while read -r FILE; do basename "${FILE}"; done > "${BIN_DIR}/bench.txt"

./cabal-plan list-bins  | grep ad-delcont-primop | awk '{ print $2 }' | while read -r BIN; do
  strip "${BIN}"
  cp "${BIN}" "${BIN_DIR}/bin/"
done

tar -cf "${DEST}" --use-compress-program="zstdmt -9"  "${BIN_DIR}"
