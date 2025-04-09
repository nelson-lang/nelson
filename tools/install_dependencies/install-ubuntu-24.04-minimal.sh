#!/bin/bash
#
# Nelson - Installation script for Ubuntu 24.04 dependencies
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
    echo "This script must be run as root or with sudo privileges."
    exit 1
fi

print_status "Nelson - Installation script for Ubuntu 24.04 dependencies (minimal build)"

print_status "Updating package lists"
apt-get update --fix-missing -y

print_status "Upgrading existing packages"
apt-get upgrade -y
   

print_status "Installing build tools"
apt-get install -y build-essential ninja-build autotools-dev libtool automake \
                   pkg-config just

print_status "Installing system dependencies"
apt-get install -y apt-transport-https ca-certificates gnupg software-properties-common wget 

print_status "Installing math libraries"
apt-get install -y  liblapack-dev liblapacke-dev libeigen3-dev

print_status "Installing Boost libraries"
apt-get install -y libboost-all-dev

print_status "Cleaning up"
apt-get autoremove -y
apt-get clean

print_status "All dependencies have been installed successfully!"
