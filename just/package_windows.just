set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Package Windows 64-bit version
win64: (package "x64")

# Package Windows ARM64 version
arm64: (package "ARM64")

# Helper to package Nelson for a given architecture
[private]
package arch:
    @.\bin\{{arch}}\nelson-cli --no-ipc --quiet -e run('.\tools\innosetup\innosetup.m');exit
    @iscc .\tools\innosetup\Nelson.iss
