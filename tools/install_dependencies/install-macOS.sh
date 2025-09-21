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

# sudo rm -r /Library/Developer/CommandLineTools

# # Reset xcode-select
# sudo xcode-select -r

# # Then install. You will be asked to click a button 
# sudo xcode-select --install

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
    gettext
    boost
    libxml2
    libxslt
    cmake
    fftw
    portaudio
    libsndfile
    taglib
    libgit2
    hdf5
    libmatio
    eigen
    libomp
    openblas
    qt
    giflib
    numpy
    tbb
    julia
    libtiff
    just
)

for package in "${dependencies[@]}"; do
    if ! brew list "$package" &>/dev/null; then
        print_status "Installing $package"
        brew install "$package"
    else
        print_status "$package is already installed. Skipping..."
    fi
done

# Link necessary packages
print_status "Linking necessary packages"
brew link gettext --force
brew link libomp --force
brew link giflib --force
brew link icu4c --force

# Update PATH for gettext
if ! grep -q '/usr/local/opt/gettext/bin' ~/.zshrc; then
    echo 'export PATH="/usr/local/opt/gettext/bin:$PATH"' >> ~/.zshrc
    source ~/.zshrc
fi

print_status "All dependencies have been installed successfully!"
