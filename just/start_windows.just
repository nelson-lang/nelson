set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Launch for Windows 64-bit
win64 *args:
    @.\bin\x64\nelson.bat {{args}}

# Launch for Windows 32-bit
win32 *args: 
    @.\bin\win32\nelson.bat {{args}}

# Launch for Windows ARM64
arm64 *args:
    @.\bin\ARM64\nelson.bat {{args}}
