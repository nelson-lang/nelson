#!/bin/bash
#
# Nelson - Install the Node.js version required by nflow_gui with nvm
#

set -e
set -o pipefail

print_status() {
    echo -e "\n===> $1"
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NELSON_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
NODE_VERSION_FILE="$NELSON_ROOT/.nvmrc"

if [ ! -f "$NODE_VERSION_FILE" ]; then
    echo "Missing Node.js version file: $NODE_VERSION_FILE"
    exit 1
fi

NODE_VERSION="$(tr -d '[:space:]' < "$NODE_VERSION_FILE")"
if [ -z "$NODE_VERSION" ]; then
    echo "Empty Node.js version file: $NODE_VERSION_FILE"
    exit 1
fi

export NVM_DIR="${NVM_DIR:-$HOME/.nvm}"
if [ ! -s "$NVM_DIR/nvm.sh" ]; then
    print_status "Installing nvm"
    curl -fsSL https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
fi

# shellcheck disable=SC1091
. "$NVM_DIR/nvm.sh"

print_status "Installing Node.js $NODE_VERSION with nvm"
nvm install "$NODE_VERSION"
nvm alias default "$NODE_VERSION"
nvm use "$NODE_VERSION"

print_status "Node.js and npm versions"
node --version
npm --version
