# Run Nelson code analyzer
lint:
    bin\nelson-lint.exe --include-subfolders true .

# Run typo checker
typos:
    cargo typos
