set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Benchmarks for Windows 64-bit version
win64: (benchmarks-arch "x64")

# Benchmarks for Windows ARM64 version
arm64: (benchmarks-arch "ARM64")

# Helper to build for a given architecture
[private]
benchmarks-arch arch:
    @.\bin\{{arch}}\nelson-adv-cli.exe --no-ipc --quiet -f .\tools\benchmark_all\runbenchmark_all.m
