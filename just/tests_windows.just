set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Tests for Windows 64-bit version
win64: (tests-arch "x64")

# Tests for Windows ARM64 version
arm64: (tests-arch "ARM64")

# Helper to build for a given architecture
[private]
tests-arch arch:
    @.\bin\{{arch}}\nelson-adv-cli.exe --no-ipc --quiet -f .\tools\tests_all\runtests_all.m
