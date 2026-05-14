set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Build Windows 64-bit CMake build (all targets)
win64:
    @call win64-environment.bat env-only && .\tools\cmake\bin\cmake.exe --build build\cmake-windows-x64 --config Release --target ALL_BUILD -- /m:8 /p:PreferredToolArchitecture=x64

# Build Windows 32-bit CMake build (all targets)
win32:
    @call win32-environment.bat env-only && .\tools\cmake\bin\cmake.exe --build build\cmake-windows-win32 --config Release --target ALL_BUILD -- /m:8 /p:PreferredToolArchitecture=x64

# Build Windows ARM64 CMake build (all targets)
arm64:
    @call ARM64-environment.bat env-only && .\tools\cmake\bin\cmake.exe --build build\cmake-windows-arm64 --config Release --target ALL_BUILD -- /m:8 /p:PreferredToolArchitecture=x64
