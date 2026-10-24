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
install_packages base-devel git gcc binutils glibc inetutils gawk m4 pkg-config cmake make ninja just clang rust curl

arch=$(uname -m)
if [ "$arch" = "x86_64" ]; then
  print_status "Installing clang-format-20 (static binary)"
  curl -fsSL -o /usr/local/bin/clang-format-20 \
    https://github.com/muttleyxd/clang-tools-static-binaries/releases/download/master-796e77c/clang-format-20_linux-amd64
  chmod +x /usr/local/bin/clang-format-20
  ln -sf /usr/local/bin/clang-format-20 /usr/local/bin/clang-format
else
  print_status "Installing clang-format from pacman"
  pacman -S --noconfirm clang
fi

print_status "Installing Node.js with nvm"
# This installer runs as root (for pacman), but nvm installs Node.js per user.
# When invoked through sudo, install Node.js for the invoking user so a later
# cmake run (as that user) finds ~/.nvm instead of root's home.
if [ -n "${SUDO_USER:-}" ] && [ "$SUDO_USER" != "root" ]; then
    sudo -u "$SUDO_USER" -H bash "$(dirname "$0")/install-node-nvm.sh" || \
        print_status "WARNING: Node.js/nvm setup failed; continuing (only the GUI JS build is affected)."
else
    bash "$(dirname "$0")/install-node-nvm.sh" || \
        print_status "WARNING: Node.js/nvm setup failed; continuing (only the GUI JS build is affected)."
fi

# Install libraries and dependencies by category
print_status "Installing core libraries"
install_packages libffi icu zlib curl libgit2 pcre2

# Install Qt dependencies
print_status "Installing Qt dependencies"
install_packages qt6-base qt6-svg qt6-declarative

# Install math libraries
print_status "Installing math libraries"
install_packages blas lapack fftw eigen

# Install HPC libraries
print_status "Installing HPC libraries"
install_packages openmpi onetbb

# Install data format libraries
print_status "Installing data format libraries"
install_packages libxml2 libxslt hdf5 libmatio netcdf arrow

# Install geometry libraries (qhull is required by the geometry module)
print_status "Installing geometry libraries"
install_packages qhull

# Install multimedia libraries
print_status "Installing multimedia libraries"
install_packages taglib portaudio libsndfile giflib libjpeg-turbo libpng libtiff libwebp

# Install Python dependencies
print_status "Installing Python dependencies"
install_packages python3 python-numpy python-pip

# WebView runtime for the HTML viewer (`web()`), the examples gallery and the
# Markdown renderer. WebUI dlopen()s "libgtk-3.so.0" and "libwebkit2gtk-4.1.so.0"
# at run time and never links them. Without them `web()` silently opens a web
# browser instead of a native window.
print_status "Installing WebView runtime (GTK3 + WebKit2GTK)"
for pkg in gtk3 webkit2gtk-4.1; do
    pacman -S --needed --noconfirm "$pkg" || \
        echo "Package '$pkg' not available: the native WebView will not be used."
done

# Optional libraries: the related modules are simply disabled when missing.
# - SUNDIALS >= 7 (sundials_core) is required by the ode_solvers backend
# - HiGHS is used by the optimization module (linprog, intlinprog), AUR only
print_status "Installing optional libraries"
for pkg in sundials highs; do
    print_status "Installing optional package: $pkg"
    pacman -S --needed --noconfirm "$pkg" || echo "Optional package '$pkg' not available: skipped."
done

print_status "All dependencies have been installed successfully!"
