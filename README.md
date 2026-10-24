![Visitor Badge](https://visitor-badge.laobi.icu/badge?page_id=nelson-lang.nelson)

<div align="center">
  <a href="https://nelson-lang.github.io/nelson-website/" target="_blank">
    <img src="https://github.com/nelson-lang/nelson/blob/master/resources/banner_nelson_small.png?raw=true" alt="Nelson Logo" width="210" height="142"></img>
  </a>
</div>
<br>
<br>

<table>
  <tr>
    <td>
      <a href="https://github.com/nelson-lang/nelson/actions/workflows/ccpp.yml"><img src='https://github.com/nelson-lang/nelson/workflows/C%2FC%2B%2B%20CI/badge.svg'/></a>
    </td>
    <td>
      <a href="https://github.com/nelson-lang/nelson/actions/workflows/codeql-analysis-cpp.yml"><img src='https://github.com/nelson-lang/nelson/actions/workflows/codeql-analysis-cpp.yml/badge.svg'/></a>
    </td>
    <td>
      <a href="https://bestpractices.coreinfrastructure.org/projects/602"><img src='https://bestpractices.coreinfrastructure.org/projects/602/badge'/></a>
    </td>
  </tr>
</table>

# The Nelson Language

The main homepage for Nelson can be found at [https://nelson-lang.github.io/nelson-website/](https://nelson-lang.github.io/nelson-website/).

## **Starring the repository**

If this project is useful to you, consider [starring it on GitHub](https://github.com/nelson-lang/nelson/stargazers). Repository stars provide a simple signal of usage and help prioritize ongoing maintenance.

## **Introduction**

Nelson is an open-source numerical computing language for engineering, scientific computing, and education. It provides more than 3,000 functions, native or written in Nelson, covering array computation, linear algebra, signal processing, data analysis, visualization, and external language interfaces.

Nelson follows the array-oriented conventions used by established numerical computing environments and GNU Octave, while keeping its own module system, file formats, engine interfaces, and execution model.

![Nelson environment](https://github.com/nelson-lang/nelson-website/blob/master/images/Nelson-windows.png?raw=true)

## **Features**

### Data Types Managed by Nelson

- **Double and Complex Double**: Supports scalars, vectors, 2D matrices, N-dimensional arrays, and sparse matrices.
- **Single and Complex Single**: Includes scalars, vectors, 2D matrices, N-dimensional arrays, and sparse matrices.
- **Logical**: Handles scalars, vectors, 2D matrices, N-dimensional arrays, and sparse matrices.
- **Character Arrays**: Supports UNICODE characters.
- **String Arrays**: Fully supports UNICODE.
- **Integers**: 8, 16, 32, and 64-bit signed and unsigned scalars, vectors, 2D matrices, and N-dimensional arrays.
- **Handle Objects**: For object-oriented functionality.
- **Anonymous Functions**: Allows creation and manipulation of functions without names.
- **Data Structures**: Supports dictionaries and tables.
- **Overloading**: All types can be overloaded for custom behavior.

### Performance Enhancements

- **OpenMP and SIMD**: Uses parallel processing and vectorization for faster computations.

### Visualization & Interface

- **2D and 3D Plotting**: High-level commands for visualizations.
- **User Interface Controls**: Built-in controls for creating custom interfaces.
- **Desktop Environment**: Comes with history tracking, a file explorer, and workspace browser.

### Advanced Modules

- **Parallel Computing**: Enables efficient use of multi-core processors.
- **Fast Fourier Transform (FFT)**: High-performance FFT functions based on FFTW and MKL.
- **SLICOT Interface**: Optional support for the Systems and Control Theory subroutine library.
- **Control System Module**: Tools for control theory and system design.
- **MPI (Message Passing Interface)**: Functions for distributed parallel computing.

### Data Formats & Interfacing

- **JSON Support**: Read and write JSON files.
- **HDF5 Functions**: High-level I/O functions, with HDF5 as the default file format for `.nh5` workspaces.
- **MAT-File Compatibility**: Load and save workspaces in MAT-file format.
- **Foreign Function Interface (FFI)**: Build and load C/Fortran code on the fly.
- **MEX C API Compatibility**: Interfacing with MEX-compatible C APIs.
- **Nelson Engine API**: Use Nelson as a backend engine within C code, compatible with the MEX Engine API.
- **Julia Interfacing**: Call Julia scripts and functions from Nelson.
- **Python Interfacing**: Call Python scripts and functions from Nelson.
- **RESTful API**: Enables Nelson to interact with web services.

### Additional Capabilities

- **Inter-Process Communication**: Communicate between Nelson processes.
- **QML Engine**: Use Qt’s QML framework to display and manipulate graphical content.
- **Component Object Model (COM)**: Interface with COM components, especially on Windows.
- **Excel File Support**: Write and read `.xlsx` files using COM on Windows.
- **Embedded Code Editor**: Integrated editor for Nelson scripts.

### Help & Testing Tools

- **Help Engine**: Generate and view help files in various formats like HTML, Markdown, PDF, or GitBook.
- **Test Engine**: Validate algorithms using built-in functions, with support for xUnit report export.

### Debugging

- **Debugger**: Interactive debugger with breakpoints, step-in/step-over/step-out, variable inspection, call stack navigation.

### Profiling & Code Coverage

- **Profiler**: Built-in profiler to analyze and optimize code performance.
- **Code Coverage**: Measure the coverage of your tests to ensure thorough validation.

### Cloud & Extensibility

- **Nelson Cloud**: Instant access to Nelson from any web browser via [Nelson Cloud](https://www.npmjs.com/package/nelson-cloud).
- **Module Skeleton**: Templates for extending Nelson:
  - [Template with Macros and Builtins](https://github.com/nelson-lang/module_skeleton).
  - [Basic Macros Template](https://github.com/nelson-lang/module_skeleton_basic).
- **Nelson Modules Manager (nmm)**: A package manager to install and manage extensions for Nelson.

## **Download**

[![Get it from the Snap Store](https://snapcraft.io/static/images/badges/en/snap-store-black.svg)](https://snapcraft.io/nelson)

<a href='https://flathub.org/apps/io.github.nelson_lang.Nelson'>
<img width='240' alt='Get it on Flathub' src='https://flathub.org/api/badge?locale=en'/>
</a>

[Latest Windows Installer](https://github.com/nelson-lang/nelson/releases)

[Get it from the Windows Package Manager](https://winstall.app/apps/NelsonNumericalSoftware.Nelson)

[Get it from the Chocolatey Package Manager](https://community.chocolatey.org/packages/nelson)

[Get it from Scoop](https://scoop.sh/#/apps?q=nelson)

## **Resources**

- **Homepage:** <https://nelson-lang.github.io/nelson-website/>
- **GitBook:** <https://nelson-lang.github.io/nelson-gitbook/>
- **Documentation:** <https://nelson-lang.github.io/nelson-gitbook/releases/en_US/latest/index.html>
- **Debugger Reference:** <https://nelson-lang.github.io/nelson-gitbook/releases/en_US/latest/index.html>
- **Source code:** <https://github.com/nelson-lang/nelson>
- **Binaries:** <https://github.com/nelson-lang/nelson/releases>
- **Docker:** <https://hub.docker.com/r/nelsonsoftware/nelson/>
- **Gitter:** <https://gitter.im/nelson-lang/Lobby>
- **YouTube:** <https://www.youtube.com/channel/UCdZMnH0HC9XflNGAFFiRX9g>
- **Twitter:** <https://twitter.com/Nelson_software>
- **LinkedIn:** <https://www.linkedin.com/groups/13576150>
- **DeepWiki** [![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/nelson-lang/nelson)

## **License**

Nelson is delivered under dual-license:

- [![License (GNU Lesser General Public License (LGPL) v3.0)](<https://img.shields.io/badge/License-GNU%20Lesser%20General%20Public%20License%20(LGPL)%20v3.0-blue.svg?style=flat-square>)](https://opensource.org/licenses/LGPL-3.0)

  or

- [![License (GNU General Public License (GPL) v3.0)](<https://img.shields.io/badge/license-GNU%20General%20Public%20License%20(GPL)%20v3.0-blue.svg?style=flat-square>)](https://opensource.org/licenses/GPL-3.0)

Third-party components redistributed with Nelson are listed in [THIRDPARTY.md](THIRDPARTY.md).

## **Trademarks**

Nelson is an independent free software project, not affiliated with, endorsed by, or sponsored by any third party. See [TRADEMARKS.md](TRADEMARKS.md) for the full notice.

## **Supported Platforms**

Nelson is tested on the platforms listed below. **[Download](https://github.com/nelson-lang/nelson/releases)** options and **[continuous integration (CI)](https://github.com/nelson-lang/nelson/actions/workflows/ccpp.yml)** status are shown for each supported target.

| **Operating System** |  **Architecture**   | **CI** |   **Binaries**    |
| :------------------: | :-----------------: | :----: | :---------------: |
|  **Windows 10, 11**  | x86-64 (\*1), arm64 |   ✓    |         ✓         |
|   **macOS Tahoe**    |        arm64        |   ✓    |                   |
|  **macOS Sequoia**   |        arm64        |   ✓    |                   |
|   **macOS Sonoma**   |        arm64        |   ✓    |                   |
|   **macOS Sonoma**   |   x86-64 (64-bit)   |   ✓    |                   |
|   **Ubuntu 26.04**   |       x86-64        |   ✓    | ✓ (Snap, Flatpak) |
|   **Ubuntu 26.04**   | arm64 (cobalt 100)  |   ✓    | ✓ (Snap, Flatpak) |
|   **Ubuntu 24.04**   |       x86-64        |   ✓    | ✓ (Snap, Flatpak) |
|   **Ubuntu 24.04**   | arm64 (cobalt 100)  |   ✓    | ✓ (Snap, Flatpak) |
|    **Fedora 43**     |       x86-64        |   ✓    | ✓ (Snap, Flatpak) |
|    **ArchLinux**     |       x86-64        |   ✓    | ✓ (Snap, Flatpak) |

_Note: The continued support of some platforms may depend on community contributions._

(\*1) On Windows x64, the processor must be an Intel or AMD x86-64 CPU that supports the [AVX2](https://en.wikipedia.org/wiki/Advanced_Vector_Extensions#CPUs_with_AVX2) instruction set.

Official distribution of 32-bit Windows binary versions has been discontinued.

Allan CORNET (nelson.numerical.computation@gmail.com)
