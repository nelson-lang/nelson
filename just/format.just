# Format code
format:
    cargo format

# Check code formatting (exit 1 if any file is unformatted)
format-check:
    cargo format-check

# Format only files tracked by git (skip untracked files)
format-tracked:
    cargo format-tracked

# Check formatting of git-tracked files only (exit 1 if any is unformatted)
format-tracked-check:
    cargo format-tracked-check
