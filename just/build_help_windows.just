set windows-shell := ["cmd.exe", "/c"]
set quiet := true
set working-directory := ".."

# Build help/documentation for Windows 64-bit version
win64:  (build-help "x64")

# Build help/documentation for Windows 32-bit version
win32:  (build-help "win32")

# Build help/documentation for Windows arm64
arm64:
    @echo Not yet supported

[private]
build-help arch:
    @.\bin\{{arch}}\nelson-cli --noipc --quiet -f .\tools\buildhelp\buildhelp.m
