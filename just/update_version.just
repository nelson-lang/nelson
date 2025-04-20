python := if os_family() == "windows" { "python.exe" } else { "python3" }

# Update Nelson's version number (CI)
update_version *args:
    {{python}} ./tools/update_version/update_version.py {{args}}
