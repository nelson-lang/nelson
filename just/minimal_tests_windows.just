set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Minimal tests for Windows 64-bit version
win64: (minimal-tests-arch "x64")

# Minimal tests for Windows ARM64 version
arm64: (minimal-tests-arch "ARM64")

# Helper to build for a given architecture
[private]
minimal-tests-arch arch:
    @.\bin\{{arch}}\nelson-cli.exe --no-ipc --quiet -f .\tools\tests_minimal\runtests_minimal.m
