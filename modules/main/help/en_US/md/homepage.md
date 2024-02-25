![banner](banner_homepage.png)

### Nelson 1.2.0.0

Nelson is an interactive, fully functional environment for engineering and scientific applications. It implements a matrix-driven language (which is largely compatible with MATLAB and GNU Octave), with advanced features such as 2-D 3-D plotting, image manipulation and viewing, a codeless interface to external C/C++/FORTRAN libraries, native support for various C types, and a host of other features.

Try it !

[Web site](https://nelson-lang.github.io/nelson-website/)

## Features

- Types managed by Nelson:

  - double and double complex: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - single and single complex: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - logical: scalar, vector, matrix 2D, N dimensions array, sparse matrix.
  - character array (UNICODE supported).
  - string array (UNICODE supported).
  - integers 8, 16, 32, 64 signed and unsigned: scalar, vector, matrix 2D, N dimensions array.
  - handle objects.
  - anonymous functions,
  - all types can be overloaded.

- `OpenMP` and `SIMD` extensions used.

- 2D and 3D plotting with high-level plot commands.

- Desktop environment with history, file and workspace browser.

- Parallel Computing Module.

- Fast Fourrier Transformation functions based on FFTW and MKL wrapper.

- SLICOT (Subroutine Library in Systems and Control Theory) interfaces (optional).

- Control System module.

- Message Passing Interface (MPI): functions for parallel computing.

- JSON decode/encode data support.

- HDF5 high-level functions I/O,

- HDF5 used as default data file format (.nh5) load/save workspace,

- MAT-file compatible load/save workspace,

- Foreign Function Interface C/Fortran.

- Interfacing C/C++ or Fortran with Nelson (build and load external code on the fly).

- MEX C API compatibility.

- Nelson Engine API for C (compatible with MEX Engine). Call Nelson from your C code as engine.

- RESTful API web service.

- Inter-process communication between Nelson's process.

- The QML engine enables nelson programs to display and manipulate graphical content using Qt's QML framework.

- Component Object Model (COM) client interface: binary-interface standard for software components on Windows.

- Write/Read xlsx files on Windows using COM.

- Embedded Nelson code editor.

- Help engine:

  Generate help files using Nelson dedicated functions.
  View your generated help files as html, markdown, pdf, gitbook or directly in Nelson help viewer.

- Tests engine:

  Validate your algorithm using Nelson dedicated functions.
  Export the test results under the xUnit reports format.

- Profiling and Code coverage tools for Nelson's language:

  Nelson has a built-in profiler that is very useful to profile your code and find out what script or function is taking the most time.

- [Nelson cloud](https://www.npmjs.com/package/nelson-cloud):
  Instant access to Nelson anywhere from an web browser.

- Module skeleton to extend Nelson available here:

  - [template macros and builtin](https://github.com/nelson-lang/module_skeleton),
  - [basic template macros only](https://github.com/nelson-lang/module_skeleton_basic).

- Nelson Modules Manager (nmm) : package manager for Nelson

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
