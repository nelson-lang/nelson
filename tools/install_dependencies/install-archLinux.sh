#!/bin/bash
#
# Nelson - Installation script for Arch Linux dependencies
# This script installs all required dependencies to build Nelson from source
#

set -e  # Exit immediately if a command exits with a non-zero status
set -o pipefail  # Return value of a pipeline is the value of the last command

# Function to print messages
print_status() {
    echo -e "\n===> $1"
}

# Function to install packages with error handling
install_packages() {
    print_status "Installing: $*"
    if ! pacman -S --needed --noconfirm "$@"; then
        echo "Failed to install packages: $*"
        exit 1
    fi
}

# Check if running as root
if [ "$(id -u)" -ne 0 ]; then
    echo "This script must be run as root or with sudo privileges."
    exit 1
fi

# Update system
print_status "Updating system packages"
pacman -Syu --noconfirm || { echo "System update failed"; exit 1; }

# Install package groups
print_status "Installing development tools"
install_packages base-devel git gcc binutils glibc inetutils gawk m4 pkg-config cmake make just

# Install libraries and dependencies by category
print_status "Installing core libraries"
install_packages boost-libs boost libffi icu zlib curl libgit2

# Install Qt dependencies
print_status "Installing Qt dependencies"
install_packages qt5-base qt5-svg qt5-tools qt5-quickcontrols

# Install math libraries
print_status "Installing math libraries"
install_packages blas lapack lapacke fftw eigen

# Install HPC libraries
print_status "Installing HPC libraries"
install_packages openmpi onetbb

# Install data format libraries
print_status "Installing data format libraries"
install_packages libxml2 hdf5 libmatio

# Install multimedia libraries
print_status "Installing multimedia libraries"
install_packages taglib portaudio libsndfile giflib libtiff

# Install Python dependencies
print_status "Installing Python dependencies"
install_packages python3 python-numpy python-pip

print_status "All dependencies have been installed successfully!"
