# 🚀 Windows Build Instructions

🔗 **Dependencies:** Refer to the [`ccpp_windows.yml`](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp_windows.yml) file for the most up-to-date build instructions and required dependencies.

---

## ✅ Prerequisites

Before building **Nelson** on Windows, make sure the following tools are installed:

- 🛠 **[Git for Windows](https://git-scm.com/downloads/win)**
- 🎯 **[Visual Studio 2022 (C++)](https://learn.microsoft.com/en-us/visualstudio/install/install-visual-studio?view=vs-2022)**
- ⚙️ **[Just Command Runner](https://just.systems/)**
  Install via [winget](https://learn.microsoft.com/en-us/windows/package-manager/winget/):

  ```bash
  winget install --id Casey.Just --exact
  ```

---

## 📁 Set Up the Project Directory

```bash
mkdir nelson-lang
cd nelson-lang
```

---

## 🔄 Clone the Source Repository

```bash
git clone https://github.com/nelson-lang/nelson.git
cd nelson
```

---

## 📥 Install Third-Party Dependencies

- ✅ **64-bit Architecture** _(Recommended)_:

  ```bash
  just -f justfile.win install_dependencies::win64
  ```

- ⚠️ **32-bit Architecture** _(Deprecated, not officially maintained)_:

  ```bash
  just -f justfile.win install_dependencies::win32
  ```

---

## 🏗️ Build Nelson

### Using `just` (Recommended)

1. 📂 Navigate to the cloned `nelson` directory (if not already there).
2. 🛠 Run the build command:

   - For 64-bit:

     ```bash
     just -f justfile.win build::win64
     ```

   - For 32-bit:
     ```bash
     just -f justfile.win build::win32
     ```

### Using `just` + CMake (All Targets)

Use the CMake recipes exposed by `justfile.win`:

```bash
# Configure x64 CMake build (disable MPI when wrappers are unavailable)
just -f justfile.win cmake_config::win64 -- -DWITHOUT_MPI_MODULE=ON

# Build all targets
just -f justfile.win cmake_build::win64
```

For other architectures:

```bash
just -f justfile.win cmake_config::win32
just -f justfile.win cmake_build::win32

just -f justfile.win cmake_config::arm64
just -f justfile.win cmake_build::arm64
```

**Note:** MPI is automatically disabled by default on Windows ARM64 builds. Use `-DNELSON_ALLOW_MPI_ON_WINDOWS_ARM64=ON` to allow enabling MPI manually.

### Visual Studio IDE

You can also launch the Visual Studio 2022 IDE with the appropriate environment variables by running either `win64-environment.bat` or `win32-environment.bat`.

---

## ▶️ Start Nelson

- 64-bit:

  ```bash
  just -f justfile.win start::win64
  ```

- 32-bit:
  ```bash
  just -f justfile.win start::win32
  ```

---

🔙 [← Back to: Building](BUILDING.md)
