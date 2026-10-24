set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Get module skeleton for Windows 64-bit
win64: (get-skeleton "x64")

# Get module skeleton for Windows ARM64
arm64: (get-skeleton "ARM64")

# Helper command to get module skeleton for given architecture
[private]
get-skeleton arch:
    @.\bin\{{arch}}\nelson-cli --no-ipc --quiet -f .\tools\clone_module_skeleton\clone.m
