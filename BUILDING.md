# 🏰️ Building Nelson

Depending on the platform or features required, the build process may vary slightly. After successfully compiling a binary, running the test suite is recommended to validate its functionality.

If you consistently encounter a test failure, check the [Nelson issue tracker](https://github.com/nelson-lang/nelson/issues) or file a new issue.

---

## 🚀 How to Build Nelson

To compile Nelson, you will need:

- **C++17-compatible compiler**
- **Qt 6.2 or later** (required for the GUI and various components)

Platform-specific build instructions are available here:

- **[Linux](BUILDING_Linux.md)**
- **[macOS](BUILDING_Macos.md)**
- **[Windows](BUILDING_Windows.md)**

For the most up-to-date list of dependencies and build configurations, refer to the [`ccpp.yml`](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) file in the GitHub repository.

---

## 🔁 Continuous Integration

GitHub Actions builds and tests Nelson on every push and pull request. The main pipeline is [`ccpp.yml`](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml): it builds Windows x64, macOS arm64 and Ubuntu x64, then runs the test suite on the installed packages. Other workflows run next to it:

- `cpp-tests.yml` builds and runs the C++ unit tests (doctest) of each module.
- `frontend.yml` runs the TypeScript unit and e2e tests. It runs on master, or when a frontend directory changes.
- `prettier.yml` checks the formatting and the spelling.

Some builds are not part of the push pipeline. They are started by hand, from the **Run workflow** button in the Actions tab, or with the [GitHub CLI](https://cli.github.com/):

| Workflow                 | What it builds                                  | Command                                                                 |
| ------------------------ | ----------------------------------------------- | ----------------------------------------------------------------------- |
| `ccpp_fedora.yml`        | Fedora 44 build and tests (no display)          | `gh workflow run ccpp_fedora.yml --ref <branch> --repo nelson-lang/nelson`        |
| `ccpp_windows_cmake.yml` | Windows x64 with CMake instead of `nelson.sln`  | `gh workflow run ccpp_windows_cmake.yml --ref <branch> --repo nelson-lang/nelson` |
| `minimal.yml`            | Ubuntu 24.04 with `MINIMAL_BUILD=ON`            | `gh workflow run minimal.yml --ref <branch> --repo nelson-lang/nelson`            |

Replace `<branch>` with the branch to build. GitHub only lists a manual workflow once its file exists on the default branch (master). To follow a run:

```bash
gh run list --workflow ccpp_fedora.yml --repo nelson-lang/nelson --limit 3
gh run watch <run-id> --repo nelson-lang/nelson
```

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

To ensure code quality, security, and maintainability, Nelson uses the following tools:

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
- **[Automated Test Suite](https://github.com/nelson-lang/nelson/actions)** – Runs broad test coverage on every commit and pull request.

### 🔒 **Security Best Practices**

- **Dependency Monitoring** – Regular updates and security checks for third-party dependencies.
- **Memory Safety Measures** – Detection of memory leaks and buffer overflows during testing.
- **Secure Code Guidelines** – Development follows best practices to prevent common security flaws.

By following these practices, we ensure Nelson remains a reliable, high-performance numerical computation language.

---

For additional information, visit the [official documentation](https://nelson-lang.github.io/nelson-website/).
