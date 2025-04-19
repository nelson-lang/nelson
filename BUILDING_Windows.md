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
