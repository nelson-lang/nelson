set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Build Windows 64-bit version
win64: (build-arch "win64")

# Build Windows 32-bit version
win32: (build-arch "win32")

# Build Windows ARM64 version
arm64: (build-arch "ARM64")

# Helper to build for a given architecture
[private]
build-arch arch:
    @.\just\build_windows.bat {{arch}}

