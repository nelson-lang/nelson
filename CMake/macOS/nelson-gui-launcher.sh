#!/bin/bash
# ==============================================================================
# Nelson.app launcher script
# This script is placed inside Nelson.app/Contents/MacOS/ and launches the
# real nelson-gui binary with the correct environment.
# ==============================================================================
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUNDLE_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
NELSON_ROOT="${BUNDLE_DIR}/Contents/Resources"

export NELSON_ROOT_DIR="${NELSON_ROOT}"
export DYLD_LIBRARY_PATH="${NELSON_ROOT}/lib/Nelson:${NELSON_ROOT}/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
export DYLD_FRAMEWORK_PATH="${BUNDLE_DIR}/Contents/Frameworks${DYLD_FRAMEWORK_PATH:+:$DYLD_FRAMEWORK_PATH}"
export PATH="${NELSON_ROOT}/bin:${PATH}"

exec "${NELSON_ROOT}/bin/nelson-gui-exec" "$@"
