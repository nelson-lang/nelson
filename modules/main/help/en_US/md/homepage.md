![banner](banner_homepage.png)

### Nelson 1.10.0.0

Nelson is a powerful, open-source numerical computational language, developed to provide a comprehensive and intuitive environment for engineers, scientists, and students. With over 1,200 built-in functions, Nelson supports a wide range of tasks, from basic algebra to advanced numerical simulations.

Originally inspired by languages like MATLAB© and GNU Octave, Nelson offers users a lightweight yet feature-rich experience. It is designed to be easy to learn and use, with an emphasis on performance and flexibility.

**Try it now!**

[Site Web](https://nelson-lang.github.io/nelson-website/)

## Features

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

- **OpenMP and SIMD**: Utilizes parallel processing and vectorization for faster computations.

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

### Profiling & Code Coverage

- **Profiler**: Built-in profiler to analyze and optimize code performance.
- **Code Coverage**: Measure the coverage of your tests to ensure thorough validation.

### Cloud & Extensibility

- **Nelson Cloud**: Instant access to Nelson from any web browser via [Nelson Cloud](https://www.npmjs.com/package/nelson-cloud).
- **Module Skeleton**: Templates for extending Nelson:
  - [Template with Macros and Builtins](https://github.com/nelson-lang/module_skeleton).
  - [Basic Macros Template](https://github.com/nelson-lang/module_skeleton_basic).
- **Nelson Modules Manager (nmm)**: A package manager to install and manage extensions for Nelson.

---

- [Changelog](CHANGELOG.md)
- [Changelog v0.7.x](CHANGELOG-0.7.x.md)
- [Changelog v0.6.x](CHANGELOG-0.6.x.md)
- [Changelog v0.5.x](CHANGELOG-0.5.x.md)
- [Changelog v0.4.x](CHANGELOG-0.4.x.md)
- [Changelog v0.3.x](CHANGELOG-0.3.x.md)
- [Changelog v0.2.x](CHANGELOG-0.2.x.md)
- [Changelog v0.1.x](CHANGELOG-0.1.x.md)
- [Nelson license](license.md)
