set shell := ["bash", "-c"]

os := if os() == "macos" { "macOS" } else { os() }

import 'just/configure.just'
mod build 'just/build.just'
import 'just/clean.just'
import 'just/install.just'
import 'just/start.just'
import 'just/update_version.just'
import 'just/format.just'
import 'just/lint.just'
import 'just/build_help.just'
import 'just/setup_python.just'
import 'just/setup_julia.just'
import 'just/get_module_skeleton.just'
import 'just/package.just'
import 'just/tests.just'
import 'just/benchmarks.just'
import? 'tools/wasm/wasm.just'
import 'just/update_localization.just'

# About recipes available in this justfile:
help:
    just -f justfile --list --list-submodules --unsorted
