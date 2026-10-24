#!/bin/bash
#
# Build a relocatable Dawn installation for Nelson's gpu_engine module.
#
# The default build is host-native, Metal-only and monolithic static.  Dawn is
# therefore linked into Nelson's module and adds no runtime library to the app
# bundle.
#
# Optional environment variables:
#   DAWN_REVISION            Dawn Git revision to build.
#   DAWN_INSTALL_PREFIX      Installation prefix.
#   DAWN_MACOS_DEPLOYMENT_TARGET  Minimum supported macOS version.
#   CMAKE_BUILD_PARALLEL_LEVEL    Parallel build job count.
#

set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
readonly REPOSITORY_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
readonly DAWN_REVISION="${DAWN_REVISION:-ebe71e6527d26611b1b64c498a7fe3d415234f75}"
readonly TARGET_ARCHITECTURE="$(uname -m)"
readonly WORK_ROOT="${REPOSITORY_ROOT}/temp/dawn-macos"
readonly SOURCE_DIR="${WORK_ROOT}/source"
readonly BUILD_DIR="${WORK_ROOT}/build-${TARGET_ARCHITECTURE}"
readonly INSTALL_PREFIX="${DAWN_INSTALL_PREFIX:-${WORK_ROOT}/install-${TARGET_ARCHITECTURE}}"
readonly DEPLOYMENT_TARGET="${DAWN_MACOS_DEPLOYMENT_TARGET:-12.0}"
readonly BUILD_JOBS="${CMAKE_BUILD_PARALLEL_LEVEL:-4}"

print_status() {
    printf '\n===> %s\n' "$1"
}

require_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        printf 'Required command not found: %s\n' "$1" >&2
        exit 1
    fi
}

if [ "$(uname -s)" != "Darwin" ]; then
    printf 'This script only supports macOS.\n' >&2
    exit 1
fi

case "${TARGET_ARCHITECTURE}" in
    arm64|x86_64) ;;
    *)
        printf 'Unsupported macOS architecture: %s\n' "${TARGET_ARCHITECTURE}" >&2
        exit 1
        ;;
esac

case "${BUILD_JOBS}" in
    ''|*[!0-9]*|0)
        printf 'CMAKE_BUILD_PARALLEL_LEVEL must be a positive integer.\n' >&2
        exit 1
        ;;
esac

require_command cmake
require_command git
require_command python3

if command -v ninja >/dev/null 2>&1; then
    readonly CMAKE_GENERATOR="Ninja"
else
    readonly CMAKE_GENERATOR="Unix Makefiles"
fi

print_status "Fetching Dawn ${DAWN_REVISION}"
if [ ! -d "${SOURCE_DIR}/.git" ]; then
    mkdir -p "${SOURCE_DIR}"
    git -C "${SOURCE_DIR}" init
    git -C "${SOURCE_DIR}" remote add origin https://dawn.googlesource.com/dawn
fi
git -C "${SOURCE_DIR}" fetch --depth=1 origin "${DAWN_REVISION}"
git -C "${SOURCE_DIR}" -c advice.detachedHead=false checkout --detach FETCH_HEAD

print_status "Configuring Dawn for ${TARGET_ARCHITECTURE} with Metal"
cmake -S "${SOURCE_DIR}" -B "${BUILD_DIR}" -G "${CMAKE_GENERATOR}" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" \
    -DCMAKE_OSX_ARCHITECTURES="${TARGET_ARCHITECTURE}" \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="${DEPLOYMENT_TARGET}" \
    -DBUILD_SHARED_LIBS=OFF \
    -DDAWN_BUILD_MONOLITHIC_LIBRARY=STATIC \
    -DDAWN_ENABLE_INSTALL=ON \
    -DDAWN_FETCH_DEPENDENCIES=ON \
    -DDAWN_ENABLE_METAL=ON \
    -DDAWN_TARGET_MACOS=ON \
    -DDAWN_ENABLE_D3D11=OFF \
    -DDAWN_ENABLE_D3D12=OFF \
    -DDAWN_ENABLE_DESKTOP_GL=OFF \
    -DDAWN_ENABLE_NULL=OFF \
    -DDAWN_ENABLE_OPENGLES=OFF \
    -DDAWN_ENABLE_VULKAN=OFF \
    -DDAWN_ENABLE_WEBGPU_ON_WEBGPU=OFF \
    -DDAWN_USE_GLFW=OFF \
    -DDAWN_BUILD_BENCHMARKS=OFF \
    -DDAWN_BUILD_NODE_BINDINGS=OFF \
    -DDAWN_BUILD_PROTOBUF=OFF \
    -DDAWN_BUILD_SAMPLES=OFF \
    -DDAWN_BUILD_TESTS=OFF \
    -DTINT_BUILD_BENCHMARKS=OFF \
    -DTINT_BUILD_CMD_TOOLS=OFF \
    -DTINT_BUILD_GLSL_VALIDATOR=OFF \
    -DTINT_BUILD_GLSL_WRITER=OFF \
    -DTINT_BUILD_HLSL_WRITER=OFF \
    -DTINT_BUILD_IR_BINARY=OFF \
    -DTINT_BUILD_SPV_READER=OFF \
    -DTINT_BUILD_SPV_WRITER=OFF \
    -DTINT_BUILD_TESTS=OFF

print_status "Building and installing Dawn"
cmake --build "${BUILD_DIR}" --target webgpu_dawn --parallel "${BUILD_JOBS}"
cmake --install "${BUILD_DIR}"

print_status "Dawn is ready for Nelson"
printf 'Configure Nelson with:\n'
printf '  cmake -S . -B build-macos -DWITH_GPU_ENGINE_MODULE=ON -DDAWN_HOME=%q\n' \
    "${INSTALL_PREFIX}"
