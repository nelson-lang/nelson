# 🚀 Windows Build Instructions

🔗 **Dependencies:** Refer to the [`ccpp_windows.yml`](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp_windows.yml) file for the most up-to-date build instructions and required dependencies.

---

## ✅ Prerequisites

Before building **Nelson** on Windows, make sure the following tools are installed:

- 🛠 **[Git for Windows](https://git-scm.com/downloads/win)**
- 🎯 **[Visual Studio 2026 (C++)](https://learn.microsoft.com/en-us/visualstudio/install/install-visual-studio?view=visualstudio)**
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
  just install_dependencies::win64
  ```

- ✅ **ARM64-bit Architecture**:

  ```bash
  just install_dependencies::arm64
  ```

---

## 🏗️ Build Nelson

### Using `just` (Recommended)

1. 📂 Navigate to the cloned `nelson` directory (if not already there).
2. 🛠 Run the build command:

   - For 64-bit:

     ```bash
     just build::win64
     ```

   - For ARM64-bit:
     ```bash
     just build::arm64
     ```

### Using `just` + CMake (All Targets)

Use the CMake recipes exposed by `justfile.win`:

```bash
# Configure x64 CMake build (disable MPI when wrappers are unavailable)
just cmake_config::win64 -- -DWITHOUT_MPI_MODULE=ON

# Build all targets
just cmake_build::win64
```

For other architectures:

```bash
just cmake_config::arm64
just cmake_build::arm64
```

**Note:** MPI is automatically disabled by default on Windows ARM64 builds. Use `-DNELSON_ALLOW_MPI_ON_WINDOWS_ARM64=ON` to allow enabling MPI manually.

### Visual Studio IDE

You can launch the Visual Studio 2026 IDE with the appropriate environment variables:

```bash
win64-environment.bat          # Open nelson.sln (all projects)
win64-environment.bat core     # Open nelson-core.slnf (95 projects)
win64-environment.bat minimal  # Open nelson-minimal.slnf (29 projects)
win64-environment.bat nflow    # Open nelson-nflow.slnf (111 projects)
win64-environment.bat env-only # Set environment variables only
win64-environment.bat help     # Show all options
```

Replace `win64` with `ARM64` for ARM64 builds.

### Solution Filters (`.slnf`)

The full `nelson.sln` contains 195+ projects. For faster IDE loading and targeted rebuilds, three solution filters are provided:

| Filter | Projects | Use case |
|--------|----------|----------|
| `nelson-minimal.slnf` | 29 (15%) | Interpreter, type system, operators |
| `nelson-core.slnf` | 95 (49%) | Full CLI build and basic tests |
| `nelson-nflow.slnf` | 111 (57%) | NFlow simulation framework |

**Workflow:**

1. Run a **full build first** (`just build::win64` or `msbuild nelson.sln`). This produces all DLLs in `bin/x64/`.
2. Open a `.slnf` filter in Visual Studio for daily work. The IDE loads only the listed projects and rebuilds only what you change.
3. At runtime, Nelson still loads all modules via `modules.m`. The DLLs for modules outside the filter remain in `bin/x64/` from the full build.
4. After a `git pull` that changes files outside your filter, run a full build again to update those DLLs.

The filters do not change `modules.m` or `nlsBuildConfig.h`. They are purely a Visual Studio IDE convenience for working on a subset of the codebase.

---

## ▶️ Start Nelson

- 64-bit:

  ```bash
  just start::win64
  ```

- ARM64-bit:
  ```bash
  just start::arm64
  ```

🔙 [← Back to: Building](BUILDING.md)
