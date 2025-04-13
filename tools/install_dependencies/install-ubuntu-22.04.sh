#!/bin/bash
#
# Nelson - Installation script for Ubuntu 22.04 dependencies
# This script installs all required dependencies to build Nelson from source
#

set -e  # Exit immediately if a command exits with a non-zero status
set -o pipefail  # Return value of a pipeline is the value of the last command

# Function to print messages
print_status() {
    echo -e "\n===> $1"
}

# Check if running as root
if [ "$(id -u)" -ne 0 ]; then
    echo "This script must be run as root or with privileges."
    exit 1
fi

# Update package lists
print_status "Updating package lists"
apt-get update --fix-missing

# Remove existing Firefox installation (temporary workaround, CI issue)
print_status "Removing existing Firefox installation"
snap remove firefox || true  # Ignore errors if Firefox is not installed via snap
apt-get remove --purge -y firefox || true

# Hold grub-efi-amd64-signed to prevent accidental upgrades
print_status "Holding grub-efi-amd64-signed package"
apt-mark hold grub-efi-amd64-signed

# Upgrade existing packages
print_status "Upgrading existing packages"
apt-get upgrade -y

# Install required dependencies
print_status "Installing required dependencies"
apt-get install -y \
    build-essential \
    apt-transport-https \
    ca-certificates \
    gnupg \
    software-properties-common \
    wget \
    ninja-build \
    xvfb \
    libopenmpi-dev \
    autotools-dev \
    libtool \
    automake \
    openmpi-bin \
    gettext \
    pkg-config \
    libffi-dev \
    libicu-dev \
    libxml2-dev \
    libopenblas-openmp-dev \
    liblapack-dev \
    liblapacke-dev \
    fftw3 \
    fftw3-dev \
    libasound-dev \
    portaudio19-dev \
    libsndfile1-dev \
    libtag1-dev \
    alsa-utils \
    libsqlite3-dev \
    libgl-dev \
    hdf5-tools \
    zlib1g-dev \
    libcurl4-openssl-dev \
    libgit2-dev \
    libboost-all-dev \
    libeigen3-dev \
    libhdf5-dev \
    libmatio-dev \
    qt6-base-dev \
    libqt6svg6-dev \
    qt6-declarative-dev \
    qt6-documentation-tools \
    qml6-module-qtquick \
    qml6-module-qtquick-templates \
    qml6-module-qtquick-controls \
    qml6-module-qtquick-window \
    qml6-module-qtquick-dialogs \
    qml6-module-qtqml-workerscript \
    qml6-module-qtquick-layouts \
    qt6-tools-dev \
    python3 \
    python3-numpy \
    libtbb-dev \
    libgif-dev \
    libtiff-dev

# Install 'just' build tool
print_status "Installing 'just' build tool"
snap install just --classic

# Clean up
print_status "Cleaning up"
apt-get autoremove -y
apt-get clean

print_status "All dependencies have been installed successfully!"
