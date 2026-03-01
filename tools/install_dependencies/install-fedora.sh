#!/bin/bash
#
# Nelson - Installation script for Fedora 41-42 dependencies
# This script installs all required dependencies to build Nelson from source
#

set -euo pipefail  # Safe Bash scripting: exit on error, unset vars, and handle pipeline fails

# Function to print status messages
print_status() {
    echo -e "\n===> $1"
}

print_status "Nelson - Installation script for Fedora 41-43 dependencies"
print_status "This script uses sudo for privileged operations; run as normal user."

print_status "Updating system packages"

sudo dnf update -y && sudo dnf upgrade -y

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

# --- Rust Toolchain ---
rust_install() {
    if ! command -v rustup &>/dev/null; then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        # Ensure cargo is available in this session
        if [ -f "$HOME/.cargo/env" ]; then
            # shellcheck disable=SC1090
            . "$HOME/.cargo/env"
        else
            export PATH="$HOME/.cargo/bin:$PATH"
        fi
    else
        rustup update stable
    fi
}

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
    libffi-devel libicu-devel libxml2-devel libxslt-devel
    boost-devel zlib-devel curl-devel libgit2-devel
)

# Install all groups
sudo dnf install -y "${system_utils[@]}"
sudo dnf install -y "${build_tools[@]}"

# Install clang-format-20 (static binary on amd64, fallback to dnf otherwise)
arch=$(uname -m)
if [ "$arch" = "x86_64" ]; then
    print_status "Installing clang-format-20 (static binary)"
    curl -fsSL https://github.com/muttleyxd/clang-tools-static-binaries/releases/download/master-796e77c/clang-format-20_linux-amd64 \
        | sudo tee /usr/local/bin/clang-format-20 >/dev/null
    sudo chmod +x /usr/local/bin/clang-format-20
    sudo ln -sf /usr/local/bin/clang-format-20 /usr/local/bin/clang-format
else
  print_status "Installing clang-format from dnf"
    sudo dnf install -y clang-tools-extra
fi

print_status "Installing Rust toolchain"
rust_install

# If running inside GitHub Actions, add the cargo bin directory to the runner PATH
# so subsequent workflow steps can find `cargo`/`rustc`.
cargo_bin="$HOME/.cargo/bin"
if [ -d "$cargo_bin" ]; then
    export PATH="$cargo_bin:$PATH"
    if [ -n "${GITHUB_PATH:-}" ]; then
        echo "$cargo_bin" >> "$GITHUB_PATH"
    fi
fi

sudo dnf install -y "${math_libs[@]}"
sudo dnf install -y "${audio_libs[@]}"
sudo dnf install -y "${qt_gui_libs[@]}"
sudo dnf install -y "${python_libs[@]}"
sudo dnf install -y "${other_libs[@]}"

print_status "All dependencies have been installed successfully!"
