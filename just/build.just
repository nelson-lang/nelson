
set shell := ["bash", "-c"]

import 'build_windows.just'

# Build the project
build:
    cd .. && cmake --build . -- -j$(nproc)
