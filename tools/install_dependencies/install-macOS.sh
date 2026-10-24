#!/bin/bash
#
# Nelson - Installation script for macOS Sequoia/Sonoma dependencies
# This script installs all required dependencies to build Nelson from source
#

set -e  # Exit immediately if a command exits with a non-zero status
set -o pipefail  # Return value of a pipeline is the value of the last command

# Function to print messages
print_status() {
    echo -e "\n===> $1"
}

print_status "Nelson - Installation script for macOS Tahoe/Sequoia/Sonoma dependencies"

# Check if Homebrew is installed
if ! command -v brew &>/dev/null; then
    print_status "Homebrew not found. Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
    print_status "Homebrew is already installed. Updating Homebrew..."
    brew update
fi

# Clean up outdated versions
print_status "Cleaning up outdated Homebrew packages"
brew cleanup

# Install required dependencies
dependencies=(
    coreutils
    zlib
    libtool
    automake
    libffi
    icu4c
    openmpi
    pkgconf
    libxml2
    libxslt
    cmake
    fftw
    portaudio
    libsndfile
    taglib
    libgit2
    pcre2
    hdf5
    libmatio
    eigen
    libomp
    openblas
    arpack
    qt
    giflib
    jpeg-turbo
    libpng
    webp
    numpy
    tbb
    julia
    libtiff
    just
    rust
    qhull
    netcdf
    apache-arrow
)

# Optional packages: the related modules are simply disabled when missing.
# - SUNDIALS >= 7 (sundials_core) is required by the ode_solvers backend
# - HiGHS is used by the optimization module (linprog, intlinprog)
optional_dependencies=(
    sundials
    highs
)

for package in "${dependencies[@]}"; do
    if ! brew list "$package" &>/dev/null; then
        print_status "Installing $package"
        brew install "$package"
    else
        print_status "$package is already installed. Skipping..."
    fi
done

for package in "${optional_dependencies[@]}"; do
    if ! brew list "$package" &>/dev/null; then
        print_status "Installing optional package $package"
        brew install "$package" || echo "Optional package '$package' not available: skipped."
    else
        print_status "$package is already installed. Skipping..."
    fi
done

print_status "Installing Node.js with nvm"
bash "$(dirname "$0")/install-node-nvm.sh" || \
    print_status "WARNING: Node.js/nvm setup failed; continuing (only the GUI JS build is affected)."

# Link necessary packages
print_status "Linking necessary packages"
brew link libomp --force
brew link giflib --force
brew link icu4c --force

# Install static clang-format-20 on arm64, fallback to brew for others
arch=$(uname -m)
if [ "$arch" = "arm64" ]; then
    print_status "Installing clang-format-20 (static binary)"
    curl -fsSL -o /usr/local/bin/clang-format-20 \
        https://github.com/muttleyxd/clang-tools-static-binaries/releases/download/master-796e77c/clang-format-20_macos-arm-arm64
    chmod +x /usr/local/bin/clang-format-20
    ln -sf /usr/local/bin/clang-format-20 /usr/local/bin/clang-format
    ln -sf /usr/local/bin/clang-format-20 "$(brew --prefix)/bin/clang-format"
    ln -sf /usr/local/bin/clang-format-20 "$(brew --prefix)/bin/clang-format-20"
else
    print_status "Installing clang-format from brew"
    brew install clang-format
fi

print_status "All dependencies have been installed successfully!"

# Install Xcode Command Line Tools
sudo rm -r /Library/Developer/CommandLineTools
# Reset xcode-select
sudo xcode-select -r
# Then install. You will be asked to click a button 
sudo xcode-select --install
