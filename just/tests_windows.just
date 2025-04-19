set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Tests for Windows 64-bit version
win64: (tests-arch "x64")

# Tests for Windows 32-bit version
win32: (tests-arch "win32")

# Tests for Windows ARM64 version
arm64:
    @echo Not yet supported

# Helper to build for a given architecture
[private]
tests-arch arch:
    @.\bin\{{arch}}\nelson.bat -adv-cli -noipc --quiet -f .\tools\tests_all\runtests_all.m
