set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Setup Python environment for Windows 64-bit
win64: (setup-python-env "x64")

# Setup Python environment for Windows 32-bit
win32: (setup-python-env "win32")

# Setup Python environment for Windows ARM64
arm64:
    @echo Not yet supported

# Helper command to setup Python environment for a given architecture
[private]
setup-python-env arch:
    .\bin\{{arch}}\nelson-cli.exe --noipc --quiet -f .\tools\python_environment_CI\configurePythonEnvironment.m
