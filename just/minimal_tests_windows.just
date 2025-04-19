set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Minimal tests for Windows 64-bit version
win64: (minimal-tests-arch "x64")

# Minimal tests for Windows 32-bit version
win32: (minimal-tests-arch "win32")

# Minimal tests for Windows ARM64 version
arm64:
    @echo Not yet supported

# Helper to build for a given architecture
[private]
minimal-tests-arch arch:
    @.\bin\{{arch}}\nelson.bat -cli -noipc --quiet -f .\tools\tests_minimal\runtests_minimal.m
