# 🚀 Windows Build Instructions

🔗 **Dependencies:** Check the [ccpp.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) file for up-to-date build instructions and required dependencies.

## ✅ Prerequisites

Before building Nelson on Windows, ensure you have the following installed:

- 🛠 **Git for Windows**
- 🎯 **Visual Studio 2022 (C++)**

## 📁 Create the Nelson Main Directory

```bash
mkdir nelson-lang
cd nelson-lang
```

## 🔄 Get the Source Code

```bash
git clone https://github.com/nelson-lang/nelson.git
cd nelson
```

## 📥 Download and Install Third-Party Dependencies

- **For Windows 64-bit architecture** ✅ _(Recommended)_:
  ```bash
  .\tools\install_dependencies\install-windows-64.bat
  ```
- **For Windows 32-bit architecture** ⚠️ _(No longer officially maintained)_:
  ```bash
  .\tools\install_dependencies\install-windows-32.bat
  ```

## 🏗️ Start the Build Process

1. 📂 Navigate to the **Nelson** directory.
2. 🖥️ Run:
   - `win32-environment.bat` _(for a 32-bit build)_
   - `win64-environment.bat` _(for a 64-bit build)_
3. 🏗️ Start the build using **Visual Studio 2022**.

🔙 [Previous: Building](BUILDING.md)
