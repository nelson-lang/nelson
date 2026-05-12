set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Configure Windows 64-bit CMake build
win64 *OPTS:
    @call win64-environment.bat env-only && set "CMAKE_GENERATOR=Visual Studio 17 2022" && .\tools\cmake\bin\cmake.exe -S . -B build\cmake-windows-x64 -A x64 {{OPTS}}

# Backward-compatible alias for x64 naming
x64 *OPTS:
    @just win64 {{OPTS}}

# Configure Windows 32-bit CMake build
win32 *OPTS:
    @call win32-environment.bat env-only && set "CMAKE_GENERATOR=Visual Studio 17 2022" && .\tools\cmake\bin\cmake.exe -S . -B build\cmake-windows-win32 -A Win32 {{OPTS}}

# Configure Windows ARM64 CMake build
arm64 *OPTS:
    @call ARM64-environment.bat env-only && set "CMAKE_GENERATOR=Visual Studio 17 2022" && .\tools\cmake\bin\cmake.exe -S . -B build\cmake-windows-arm64 -A ARM64 {{OPTS}}
