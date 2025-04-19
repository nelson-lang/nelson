set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Install dependencies for Windows 64-bit
win64 *OPTS:
    .\tools\install_dependencies\install-windows-64.bat {{OPTS}}

# Install dependencies for Windows 32-bit
win32 *OPTS:
    .\tools\install_dependencies\install-windows-32.bat {{OPTS}}

# Install dependencies for Windows ARM64
arm64 *OPTS:
    @echo Not yet supported
