# Run Nelson code analyzer
lint:
    ./bin/nelson-lint --include-subfolders true .

# Run typo checker
typos:
    cargo typos
