set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Update localization for Windows 64-bit
win64: (update-localization "x64")

# Update localization for Windows 32-bit
win32: (update-localization "win32")

# Update localization for Windows ARM64
arm64: (update-localization "ARM64")

# Helper command to update localization for a given architecture
[private]
update-localization arch:
    .\bin\{{arch}}\nelson-cli.exe --noipc --quiet -f  .\tools\gettext\i18nExtractor.m
