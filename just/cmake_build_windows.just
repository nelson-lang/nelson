set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Build Windows 64-bit CMake build (all targets)
win64:
    @call .\win64-environment.bat env-only && .\tools\cmake\bin\cmake.exe --build build\cmake-windows-x64-vs18 --config Release --target ALL_BUILD -- /m /p:UseMultiToolTask=true /p:EnforceProcessCountAcrossBuilds=true /p:PreferredToolArchitecture=x64

# Build Windows ARM64 CMake build (all targets)
arm64:
    @call .\ARM64-environment.bat env-only && .\tools\cmake\bin\cmake.exe --build build\cmake-windows-arm64-vs18 --config Release --target ALL_BUILD -- /m /p:UseMultiToolTask=true /p:EnforceProcessCountAcrossBuilds=true /p:PreferredToolArchitecture=x64
