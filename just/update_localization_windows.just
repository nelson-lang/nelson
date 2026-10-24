set windows-shell := ["cmd.exe", "/c"]

set quiet := true
set working-directory := ".."

# Update localization for Windows 64-bit
win64: (update-localization "x64")

# Update localization for Windows ARM64
arm64: (update-localization "ARM64")

# Helper command to update localization for a given architecture
[private]
update-localization arch:
    .\bin\{{arch}}\nelson-cli.exe --no-ipc --quiet -f  .\tools\i18n\i18nExtractor.m
