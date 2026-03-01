# 🏰️ Building Nelson

Depending on the platform or features required, the build process may vary slightly. After successfully compiling a binary, running the test suite is recommended to validate its functionality.

If you consistently encounter a test failure, check the [Nelson issue tracker](https://github.com/nelson-lang/nelson/issues) or file a new issue.

---

## 🚀 How to Build Nelson

To compile Nelson, you will need:

- **C++17-compatible compiler**
- **Qt 5.15 or later** (required for the GUI and various components)

Platform-specific build instructions are available here:

- **[Linux](BUILDING_Linux.md)**
- **[macOS](BUILDING_Macos.md)**
- **[Windows](BUILDING_Windows.md)**

For the most up-to-date list of dependencies and build configurations, refer to the [`ccpp.yml`](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) file in the GitHub repository.

---

## 🛠️ Code Formatting and Style

To maintain consistent formatting across the codebase, we use **[clang-format](https://clang.llvm.org/docs/ClangFormat.html)** for `.xml`, `.json`, and `.md` files.

Continuous integration (CI) ensures all files are properly formatted. To manually check and fix formatting, follow these steps:

```bash
just format
just format-check
```

Make sure **[Rust](https://rust-lang.org/)** is installed on your system before running these commands.

---

## 🔒 Code Quality, Security, and Maintainability

To ensure high code quality, security, and maintainability, Nelson leverages the following tools:

### ✅ **Static Code Analysis**

- **[CodeQL](https://codeql.github.com/)** – Performs automated security analysis and helps detect vulnerabilities.
- **[Cppcheck](https://cppcheck.sourceforge.io/)** – Identifies potential errors, memory leaks, and undefined behaviors in C/C++ code.
- **[PVS-Studio](https://pvs-studio.com/)** – Advanced static analysis tool for detecting security issues and code defects.

### 💡 **Dynamic Analysis and Testing**

- **[Valgrind](https://valgrind.org/)** – Used for memory leak detection and profiling.
- **[AddressSanitizer](https://clang.llvm.org/docs/AddressSanitizer.html) (ASan)** – Helps detect memory corruption bugs.
- **[ThreadSanitizer](https://clang.llvm.org/docs/ThreadSanitizer.html) (TSan)** – Identifies data race conditions in multithreaded code.

### 🔄 **Continuous Integration (CI)**

- **[GitHub Actions](https://github.com/features/actions)** – Automates build and test pipelines to detect issues early.
- **[Automated Test Suite](https://github.com/nelson-lang/nelson/actions)** – Runs extensive tests on every commit and pull request.

### 🔒 **Security Best Practices**

- **Dependency Monitoring** – Regular updates and security checks for third-party dependencies.
- **Memory Safety Measures** – Detection of memory leaks and buffer overflows during testing.
- **Secure Code Guidelines** – Development follows best practices to prevent common security flaws.

By following these practices, we ensure Nelson remains a reliable, high-performance numerical computation language.

---

For additional information, visit the [official documentation](https://nelson-lang.github.io/nelson-website/).
