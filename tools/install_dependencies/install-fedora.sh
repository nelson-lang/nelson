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
    which hostname git just curl
)

# --- Build Tools ---
build_tools=(
    make libtool gcc gcc-c++ autoconf automake
    cmake pkg-config
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
    qt6-qtsvg-devel qt6-qtquickcontrols2
    giflib-devel libjpeg-turbo-devel libpng-devel libtiff-devel libwebp-devel
)

# --- Python ---
python_libs=(
    python3-devel python3-numpy
)

# --- Geometry (qhull is required by the geometry module) ---
geometry_libs=(
    qhull-devel
)

# --- Data formats: netCDF and Apache Arrow / Parquet ---
data_format_libs=(
    netcdf-devel libarrow-devel parquet-libs-devel
)

# --- WebView runtime for the HTML viewer (`web()`), the examples gallery and
# the Markdown renderer. WebUI dlopen()s "libgtk-3.so.0" and
# "libwebkit2gtk-4.1.so.0" at run time and never links them, so the runtime
# packages are enough. Without them `web()` silently opens a web browser
# instead of a native window. ---
webview_libs=(
    gtk3
    webkit2gtk4.1
)

# --- ODE solvers (SUNDIALS >= 7, optional: module degrades if absent) ---
optional_libs=(
    sundials-devel
    highs-devel
)

# --- Other Libraries ---
other_libs=(
    libffi-devel libicu-devel libxml2-devel libxslt-devel
    zlib-devel curl-devel libgit2-devel pcre2-devel
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

print_status "Installing Node.js with nvm"
# nvm installs Node.js per user. When this installer is invoked through sudo,
# install Node.js for the invoking user so a later cmake run (as that user)
# finds ~/.nvm instead of root's home.
if [ -n "${SUDO_USER:-}" ] && [ "$SUDO_USER" != "root" ]; then
    sudo -u "$SUDO_USER" -H bash "$(dirname "$0")/install-node-nvm.sh" || \
        print_status "WARNING: Node.js/nvm setup failed; continuing (only the GUI JS build is affected)."
else
    bash "$(dirname "$0")/install-node-nvm.sh" || \
        print_status "WARNING: Node.js/nvm setup failed; continuing (only the GUI JS build is affected)."
fi

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
sudo dnf install -y "${geometry_libs[@]}"
sudo dnf install -y "${data_format_libs[@]}"

print_status "Installing WebView runtime (GTK3 + WebKit2GTK)"
for pkg in "${webview_libs[@]}"; do
    sudo dnf install -y "$pkg" || \
        echo "Package '$pkg' not available: the native WebView will not be used."
done

# Optional packages: not available in every Fedora release, install one by one
# and keep going when a package is missing (the related module is then disabled).
for pkg in "${optional_libs[@]}"; do
    print_status "Installing optional package: $pkg"
    sudo dnf install -y "$pkg" || echo "Optional package '$pkg' not available: skipped."
done

print_status "All dependencies have been installed successfully!"
