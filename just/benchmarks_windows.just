set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Benchmarks for Windows 64-bit version
win64: (benchmarks-arch "x64")

# Benchmarks for Windows 32-bit version
win32: (benchmarks-arch "win32")

# Benchmarks for Windows ARM64 version
arm64:
    @echo Not yet supported

# Helper to build for a given architecture
[private]
benchmarks-arch arch:
    @.\bin\{{arch}}\nelson.bat -adv-cli -noipc --quiet -f .\tools\benchmark_all\runbenchmark_all.m
