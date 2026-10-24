#!/bin/bash
#
# Nelson - Improved Installation script for Debian trixie dependencies
# This script installs all required dependencies to build Nelson from source


set -e  # Exit immediately if a command exits with a non-zero status
set -o pipefail  # Return value of a pipeline is the value of the last command

# Function to print messages
print_status() {
    echo -e "\n===> $1"
}

# Check if running as root
if [ "$(id -u)" -ne 0 ]; then
    echo "This script must be run as root or with sudo privileges."
    exit 1
fi

print_status "Nelson - Installation script for Debian trixies dependencies"

print_status "Updating package lists"
apt-get update --fix-missing -y

print_status "Upgrading existing packages"
apt-get upgrade -y

print_status "Installing build tools"
apt-get install -y build-essential cmake autotools-dev libtool automake \
                   pkg-config just curl ca-certificates

# Install clang-format-20 (static binary on amd64, fallback to apt otherwise)
arch=$(uname -m)
if [ "$arch" = "x86_64" ]; then
  print_status "Installing clang-format-20 (static binary)"
  curl -fsSL -o /usr/local/bin/clang-format-20 \
    https://github.com/muttleyxd/clang-tools-static-binaries/releases/download/master-796e77c/clang-format-20_linux-amd64
  chmod +x /usr/local/bin/clang-format-20
  ln -sf /usr/local/bin/clang-format-20 /usr/local/bin/clang-format
else
  print_status "Installing clang-format-20 from apt"
  apt-get install -y clang-format-20
fi

print_status "Installing Rust toolchain"
if ! command -v rustup &>/dev/null; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    export PATH="$HOME/.cargo/bin:$PATH"
else
    rustup update stable
fi

print_status "Installing Node.js with nvm"
# This installer runs as root (for apt), but nvm installs Node.js per user.
# When invoked through sudo, install Node.js for the invoking user so a later
# cmake run (as that user) finds ~/.nvm instead of root's home.
if [ -n "${SUDO_USER:-}" ] && [ "$SUDO_USER" != "root" ]; then
    sudo -u "$SUDO_USER" -H bash "$(dirname "$0")/install-node-nvm.sh" || \
        print_status "WARNING: Node.js/nvm setup failed; continuing (only the GUI JS build is affected)."
else
    bash "$(dirname "$0")/install-node-nvm.sh" || \
        print_status "WARNING: Node.js/nvm setup failed; continuing (only the GUI JS build is affected)."
fi

print_status "Installing system dependencies"
apt-get install -y apt-transport-https ca-certificates gnupg wget \
                   xvfb zlib1g-dev libffi-dev libicu-dev libxml2-dev libxslt1-dev libsqlite3-dev \
                   libgl-dev libcurl4-openssl-dev libgit2-dev libtbb-dev libpcre2-dev

print_status "Installing math libraries"
apt-get install -y libopenblas-openmp-dev liblapack-dev libfftw3-dev \
                   libeigen3-dev

print_status "Installing HDF5 and MATIO libraries"
apt-get install -y libhdf5-dev libmatio-dev

print_status "Installing audio libraries"
apt-get install -y libasound-dev portaudio19-dev libsndfile1-dev libtag1-dev alsa-utils

print_status "Installing image libraries"
apt-get install -y libgif-dev libjpeg62-turbo-dev libpng-dev libtiff-dev libwebp-dev

print_status "Installing MPI libraries"
apt-get install -y libopenmpi-dev openmpi-bin

print_status "Installing Qt6 dependencies"
apt-get install -y qt6-base-dev libqt6svg6-dev qt6-declarative-dev \
                   qml6-module-qtquick qml6-module-qtquick-templates qml6-module-qtquick-controls \
                   qml6-module-qtquick-window qml6-module-qtquick-dialogs \
                   qml6-module-qtqml-workerscript qml6-module-qtquick-layouts

print_status "Installing Python dependencies"
apt-get install -y python3 python3-numpy

print_status "Installing geometry libraries (qhull, required by the geometry module)"
apt-get install -y libqhull-dev

print_status "Installing netCDF libraries"
apt-get install -y libnetcdf-dev

print_status "Installing Apache Arrow and Parquet libraries"
if ! apt-get install -y libarrow-dev libparquet-dev; then
    print_status "Falling back to the official Apache Arrow repository"
    distro_id=$(. /etc/os-release && echo "$ID")
    distro_codename=$(. /etc/os-release && echo "$VERSION_CODENAME")
    wget -q -O /tmp/apache-arrow-apt-source.deb \
        "https://packages.apache.org/artifactory/arrow/${distro_id}/apache-arrow-apt-source-latest-${distro_codename}.deb"
    apt-get install -y /tmp/apache-arrow-apt-source.deb
    rm -f /tmp/apache-arrow-apt-source.deb
    apt-get update -y
    apt-get install -y libarrow-dev libparquet-dev
fi

# WebView runtime for the HTML viewer (`web()`), the examples gallery and the
# Markdown renderer. WebUI dlopen()s "libgtk-3.so.0" and "libwebkit2gtk-4.1.so.0"
# (falling back to "libwebkit2gtk-4.0.so.37") at run time and never links them,
# so only the runtime libraries are needed here, not the -dev packages. Without
# them `web()` silently opens a web browser instead of a native window.
install_first_available() {
    for candidate in "$@"; do
        if apt-get install -y "$candidate"; then
            return 0
        fi
    done
    echo "None of '$*' is available: the native WebView will not be used."
}

print_status "Installing WebView runtime (GTK3 + WebKit2GTK)"
install_first_available libgtk-3-0t64 libgtk-3-0
install_first_available libwebkit2gtk-4.1-0 libwebkit2gtk-4.0-37

# Optional libraries: the related modules are simply disabled when missing.
# - SUNDIALS >= 7 (sundials_core) is required by the ode_solvers backend
# - HiGHS is used by the optimization module (linprog, intlinprog)
for pkg in libsundials-dev highs libhighs1 libhighs-dev; do
    print_status "Installing optional package: $pkg"
    apt-get install -y "$pkg" || echo "Optional package '$pkg' not available: skipped."
done

print_status "Cleaning up"
apt-get autoremove -y
apt-get clean

print_status "All dependencies have been installed successfully!"
