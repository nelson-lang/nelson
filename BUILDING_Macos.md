# Building on macOS

To build this project on macOS, follow the steps below.

## Nix

Nelson provides a [Nix](https://nix.dev/manual/nix/2.17/command-ref/new-cli/nix3-develop) development environment for working on the project. This is the recommended setup for Linux and macOS users.

### [Install Nix](https://nixos.org/download/)

```bash
curl \
  --proto '=https' \
  --tlsv1.2 \
  -sSf \
  -L https://install.determinate.systems/nix \
  | sh -s -- install

```

restart your terminal

### Development Workflow

Once Nix is installed, enter the development environment with:

```bash
nix develop
```

Then, use the following commands (in this order):

`just config` — Configure the build environment

`just build` — Build the project

`just start` — Run Nelson

## 🛠️ Other Setup Instructions

### 📋 Prerequisites

1. **Install Xcode** 🧰
   Required for compiling code on macOS.
   Install it via terminal:

   ```bash
   xcode-select --install
   ```

---

### 🍺 Install Homebrew

[Homebrew](https://brew.sh) is a package manager for macOS that simplifies software installation.

To install Homebrew:

```bash
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew doctor
```

---

### 🐍 Install Micromamba

[Micromamba](https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html) is a fast and lightweight environment manager.

You have two options to install it:

🔹 **Via Homebrew** (recommended):

```bash
brew install micromamba
```

🔹 **Manual Installation**:
Follow the [official Micromamba guide](https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html) for advanced options.

---

## 🧱 Building the Project

### 🐍 Using Micromamba

1. **Navigate to the project root directory** 📁

   ```bash
   cd /path/to/nelson/
   ```

2. **Create and activate the environment** 🌱

   ```bash
   micromamba env create -f environment-macos-dev.yml
   micromamba activate nelson
   ```

3. **Configure CMake and build** 🏗️

   ```bash
   just config
   just build
   just build_help
   ```

4. **Run the application** 🚀

   ```bash
   just start
   ```

---

### 🍺 Using Homebrew Only

1. **Install required dependencies** 🧱

   ```bash
   cd nelson
   ./tools/install_dependencies/install-macOS.sh
   ```

2. **Configure and build the project** 🛠️

   ```bash
   just config
   just build
   just build_help
   ```

3. **Run the application** 🚀

   ```bash
   just start
   ```

## Additional Information

- See [ccpp.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) for an up-to-date list of dependencies and build instructions across platforms.
- Refer to [Building with Linux](BUILDING_Linux.md) for further information on compilation options.
- By default on Apple Silicon, build use native optimized cpu instructions set.
  [Previous: Building](BUILDING.md)
