
set shell := ["bash", "-c"]

import 'build_windows.just'

# Build the project
build:
    cmake --build . -- -j$(nproc 2>/dev/null || sysctl -n hw.ncpu)
