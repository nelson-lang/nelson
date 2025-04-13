#!/bin/bash
#
# Nelson - Installation script for Fedora 41 dependencies
# This script installs all required dependencies to build Nelson from source
#

set -euo pipefail  # Safe Bash scripting: exit on error, unset vars, and handle pipeline fails

# Function to print status messages
print_status() {
    echo -e "\n===> $1"
}

# Ensure script is run as root
if [ "$(id -u)" -ne 0 ]; then
    echo "This script must be run as root or with sudo privileges."
    exit 1
fi

print_status "Nelson - Installation script for Fedora 41 dependencies"

print_status "Updating system packages"

dnf update -y && dnf upgrade -y

print_status "Installing dependencies by domain"

# --- System Utilities ---
system_utils=(
    which hostname git just
)

# --- Build Tools ---
build_tools=(
    make libtool gcc gcc-c++ autoconf automake
    cmake gettext pkg-config
)

# --- Scientific / Math Libraries ---
math_libs=(
    openmpi-devel lapack-devel fftw3-devel
    eigen3-devel hdf5-devel matio-devel
    tbb-devel
)

# --- Audio Libraries ---
audio_libs=(
    portaudio-devel libsndfile-devel
    jack-audio-connection-kit-devel taglib-devel
)

# --- GUI / Qt / Graphics ---
qt_gui_libs=(
    qt6-qtbase-devel qt6-qtdeclarative-devel
    qt6-qtbase-mysql qt6-qtbase-odbc qt6-qtbase-postgresql
    qt6-doctools qt6-qtsvg-devel qt6-qtquickcontrols2
    qt6-qttools qt6-qttools-libs-help qt6-qttools-devel
    giflib-devel libtiff-devel
)

# --- Python ---
python_libs=(
    python3-devel python3-numpy
)

# --- Other Libraries ---
other_libs=(
    libffi-devel libicu-devel libxml2-devel
    boost-devel zlib-devel curl-devel libgit2-devel
)

# Install all groups
dnf install -y "${system_utils[@]}"
dnf install -y "${build_tools[@]}"
dnf install -y "${math_libs[@]}"
dnf install -y "${audio_libs[@]}"
dnf install -y "${qt_gui_libs[@]}"
dnf install -y "${python_libs[@]}"
dnf install -y "${other_libs[@]}"

print_status "All dependencies have been installed successfully!"
