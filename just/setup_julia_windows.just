set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Setup Julia environment for Windows 64-bit
win64: (setup-julia-env "x64")

# Setup Julia environment for Windows 32-bit
win32: (setup-julia-env "win32")

# Setup Julia environment for Windows ARM64
arm64: (setup-julia-env "ARM64")

# Helper command to setup Julia environment for a given architecture
[private]
setup-julia-env arch:
    .\bin\{{arch}}\nelson-cli.exe --noipc --quiet -f .\tools\julia_environment_CI\configureJuliaEnvironment.m
