<img src="banner_homepage.png" alt="banner" style="display:block; margin-left:auto; margin-right:auto;" />

# Nelson 2.0.0

**Nelson 2.0 is a major release.** It includes a faster language engine based on bytecode, object-oriented programming with `classdef`, NFlow (a visual block diagram editor with simulation and C/Rust code generation), a preview of the web desktop (including a WebAssembly build that runs in the browser), and many new data types, graphics functions, statistics functions, and developer tools.

The main homepage for Nelson can be found at [https://nelson-lang.github.io/nelson-website/](https://nelson-lang.github.io/nelson-website/). The complete list of changes is available in the [Changelog v2.x.x](CHANGELOG.md).

## Introduction

Nelson is an open-source numerical computing language for engineering, scientific computing, and education. It provides more than 3,000 functions, native or written in Nelson, covering array computation, linear algebra, signal processing, data analysis, visualization, and external language interfaces.

Nelson follows the array-oriented conventions used by established numerical computing environments and GNU Octave, while keeping its own module system, file formats, engine interfaces, and execution model.

## What's New in Nelson 2.0

### Language Engine

- **Bytecode virtual machine**: code is compiled to bytecode instead of being interpreted line by line. Loops and recursive functions run much faster.
- **Project structure**: packages, `import`, nested functions, local functions in scripts, and `localfunctions`.
- **Argument validation**: `arguments` blocks with `Repeating` sections, name-value arguments, and N-D size specifications, plus `validateattributes`, `validatestring`, and `inputParser`.
- **Shorter expressions**: a call can be followed directly by field access or indexing, for example `f().Field`, `f()(1)`, `f(){1}`.
- **Stricter syntax**: `end` is required to close blocks, and a mismatch between a file name and its function name is reported as an error.
- **Faster functions**: `diff`, `interp1`, `interp2`, `interp3`, `filter`, and `upfirdn` are now implemented in C++, and binary operators support compatible-size implicit expansion on N-D, sparse, and empty arrays.

### Object-Oriented Programming with `classdef`

- **Value and handle classes** with single and multiple inheritance, packages, abstract and sealed members, static members, constants, enumerations, and an event/listener system.
- **Property validation** with type checks and validation rules, plus `Dependent`, `Observable`, `Transient`, `AbortSet`, and `SetAccess = immutable`.
- **Dynamic properties** (`addprop`/`rmprop`), weak handles, and mixin base classes (`nelson.mixin.SetGet`, `Heterogeneous`, `Copyable`, `CustomDisplay`).
- **Custom indexing and object arrays**: build containers or proxy objects, index and concatenate object arrays, and subclass built-in numeric, logical, and char types.
- **Introspection and persistence**: `metaclass` and `?ClassName` return typed metadata, and custom objects can be saved and reloaded without loss.

### NFlow: Visual Block Diagram Editor (1.0.0-beta.1)

- **Diagram editor**: drag and connect blocks; wires are routed automatically. Diagrams are saved as plain text files that work with any version control system.
- **Block library**: sources, math, continuous and discrete dynamics, logic, lookup tables, buses, data stores, and sinks. All blocks accept vector, matrix, and typed signals.
- **Acausal physical modeling**: electrical, translational, rotational, thermal, and planar 2-D multibody components connected through physical pins, solved as a differential-algebraic system.
- **Conditional and iterated subsystems**: enabled, triggered, resettable, action, if/switch-case, function-call, for-each, and For/While iterator subsystems.
- **Simulation**: fixed-step and adaptive solvers, an optional CVODES/IDAS backend for stiff systems, zero-crossing localization, algebraic-loop solving, and multi-rate sample times; `sim()`, `linmod`, and `trim` run models from scripts.
- **Code generation and FMI**: standalone C or Rust generation from the same diagram, FMI 2.0/3.0 import, FMI 3.0 Co-Simulation export, and SSP import/export.
- **Scripted model API**: `new_system`, `add_block`, `add_line`, `get_param`/`set_param`, `find_system`, and related helpers; changes made from scripts appear immediately in the editor.

### Web Desktop and Browser Interface (preview)

- **The whole desktop in a browser**: Command Window, Workspace browser, Command History, file browser, Variable Editor, code editor, and figures in dockable panels with light and dark themes.
- **Two ways to run it**: the `nelson-webview` launcher opens a native webview window by default, or serves the same desktop over HTTP with `--web`.
- **Code editor**: an editor based on Monaco, with autocompletion, hover help, Run Section for `%%` blocks, live debugging, and a Problems panel with quick fixes.
- **Figures and user interfaces without Qt**: a display-list renderer with WebGL2 for 3-D, interactive rotation, pan and zoom, `uicontrol` and ui components as HTML widgets, and standard dialogs as modals.
- **Other panels**: profiler, documentation browser, system terminal, import data dialog, examples gallery, and package manager.
- **WebAssembly build**: Nelson also compiles to WebAssembly. The same web desktop then runs entirely in the browser, with no server and no installation, and the engine can be used from Node.js. This build is single-threaded and excludes native extensions, process creation, and code generation; `iswasm` reports the platform.

### New Data Types

- **Dates and times**: `datetime`, `duration`, and `calendarDuration` with calendar-aware arithmetic, time zones and daylight saving time, and `NaT` missing values.
- **Tables, timetables, and time series**: SQL-style joins, pivoting, grouped summaries, missing-data handling, `timetable` with `retime` and synchronization, `timeseries`, and direct math on tabular data with unit propagation.
- **Categorical arrays**: a native `categorical` type with full category management, ordinal categories, set operations, and table integration.
- **Text functions**: `compose`, `extractBetween`, `insertAfter`, `pad`, `split`, `strjoin`, and related functions, plus `nelson.lang.makeValidName` and `makeUniqueStrings`.
- **Sparse matrices**: `single` and single-complex support, iterative solvers (`gmres`, `pcg`, `bicgstab`, `lsqr`, and more) with preconditioners, `ilu`/`ichol`, and `eigs`/`svds` for sparse single inputs.

### Numerical Methods

- **ODE and DAE solvers**: a common `ode_solvers` interface, an optional SUNDIALS backend, automatic stiff/non-stiff switching, delay differential equations, sensitivity analysis, and sparse Jacobians.
- **Optimization**: linear, mixed-integer linear, quadratic, nonlinear, least-squares, and problem-based workflows; `fsolve`, `lsqnonlin`, and `fmincon` implement the standard algorithm families and output structures.
- **Statistics**: descriptive statistics, distribution fitting for more than fifteen families, correlation and regression (linear, generalized linear, robust, ridge, lasso), hypothesis tests, clustering (`kmeans`, `kmedoids`, hierarchical, spectral, Gaussian mixtures), and classification models with `predict`.
- **Image processing**: 2-D and 3-D functions for color conversions, filtering, edge detection, morphology, connected components and region measurements, geometric transforms, registration, segmentation, and feature detection. The heavy operations are parallelized with OpenMP.

### Graphics and User Interfaces

- **New chart types**: pie and donut charts, bubble charts, polar plots, box plots, volume visualization (`isosurface`), figure annotations, and surface lighting (`light`, `camlight`, `material`, `shading`, `fsurf`).
- **Graphics object utilities**: `gobjects`, `gco`, `gcbo`, `gcbf`, `allchild`, `findall`, `findfigs`, `copyobj`, `reset`, and many more properties on all graphics objects.
- **App components for `uifigure`**: panels, button groups, tab groups, grid layouts, check boxes, radio and toggle buttons, dropdowns, list boxes, edit fields, spinners, sliders, knobs, gauges, lamps, switches, date pickers, `uitable`, `uitree`, and `uiaxes`.
- **More predictable rendering**: aligned figure geometry, camera-space lighting, batched line rendering for dense 3-D scenes, and native paths for `patch`, `fill`, `fill3`, and `area`.

### Interoperability and AI

- **File formats**: Open XML `.xlsx` read/write on every platform, an optional `netcdf` module, optional Parquet support, and XML document APIs (`xmlread`, `xmlwrite`, `xslt`, `readstruct`, `writestruct`).
- **Python**: the `nelson` Python package drives a Nelson session from Python, NumPy arrays and pandas DataFrames convert in both directions, and `pyfunction` lets Python code call back into Nelson.
- **Embedding**: a new C/C++ API embeds a full Nelson interpreter inside other applications.
- **AI assistants**: an MCP server is built in, so AI assistants and agents can call Nelson directly.

### Code Quality, Testing, and Packaging

- **Static analysis**: `checkcode` and `codeIssues` detect complexity, unused variables and imports, shadowing, unreachable code, and style problems, with inline editor diagnostics and one-click fixes.
- **Command-line tools**: `nelson-lint` for CI/CD (SARIF, JSON, plain text) and `nelson-lsp`, a language server with live diagnostics, formatting, hover, and quick fixes.
- **Testing**: the `nelson.unittest` runner with HTML, JSON, JUnit XML, and TAP13 reports, the `mocking` module (`nelson.mock`), and a method-style `asserts.*` assertion API.
- **Package manager**: `nmm` adds registry search, recursive dependency resolution with semantic-version constraints, lockfiles with SHA-256 checksums, Ed25519-signed registries, transactional installs with rollback, and publishing workflows.
- **Faster startup**: dynamic libraries are loaded on first use, thanks to a cache of gateway descriptors.

## Core Features

### Data Types

- **Double, single, and complex**: scalars, vectors, 2-D matrices, N-dimensional arrays, and sparse matrices.
- **Logical and integers**: 8, 16, 32, and 64-bit signed and unsigned types.
- **Character and string arrays** with full UNICODE support.
- **Structures, cells, dictionaries, tables, timetables, categorical arrays, dates and durations**.
- **Handle objects, `classdef` classes, and anonymous functions**; all types can be overloaded.

### Performance

- **OpenMP and SIMD** parallel processing and vectorization.
- **Parallel computing** on multi-core processors, **MPI** for distributed computing, and an optional **`gpu_engine`** module for GPU compute through WebGPU.
- **High-performance FFT** based on FFTW and MKL.

### Visualization and Interface

- **2-D and 3-D plotting** with high-level commands and two rendering backends (Qt and web).
- **User interface controls** and `uifigure` components for building custom applications.
- **Desktop environment** with command history, file explorer, workspace browser, variable editor, embedded code editor, and terminal panel.

### Scientific Modules

- **Control system** tools and the optional **SLICOT** interface.
- **Signal processing**, **polynomials**, **geometry**, **special functions**, **symbolic** computation, **image processing**, **statistics**, **optimization**, and **ODE solvers**.

### Data Formats and Interfacing

- **JSON, XML, HDF5** (default `.nh5` workspace format), **MAT-file**, **`.xlsx`**, **NetCDF**, and **Parquet**.
- **Foreign Function Interface (FFI)**: build and load C/Fortran code on the fly.
- **MEX C API** and **Nelson Engine API** compatibility.
- **Julia** and **Python** interfacing, **RESTful API**, **inter-process communication**, **QML engine**, and **COM** components on Windows.

### Help, Testing, and Profiling

- **Help engine** generating HTML, Markdown, PDF, or GitBook documentation.
- **Test engine** with xUnit, JUnit, TAP13, and HTML report export.
- **Profiler** and **code coverage** tools.

## Resources

- **Website**: [https://nelson-lang.github.io/nelson-website/](https://nelson-lang.github.io/nelson-website/)
- **Source code and issues**: [https://github.com/nelson-lang/nelson](https://github.com/nelson-lang/nelson)
- **Nelson Cloud**: use Nelson from a web browser with [Nelson Cloud](https://www.npmjs.com/package/nelson-cloud).
- **Nelson Modules Manager (nmm)**: install and manage extensions for Nelson.
- **Module skeletons** to extend Nelson:
  - [Template with Macros and Builtins](https://github.com/nelson-lang/module_skeleton).
  - [Basic Macros Template](https://github.com/nelson-lang/module_skeleton_basic).

---

## Changelogs

- [Changelog v2.x.x](CHANGELOG.md)
- [Changelog v1.x.x](CHANGELOG-1.x.x.md)
- [Changelog v0.7.x](CHANGELOG-0.7.x.md)
- [Changelog v0.6.x](CHANGELOG-0.6.x.md)
- [Changelog v0.5.x](CHANGELOG-0.5.x.md)
- [Changelog v0.4.x](CHANGELOG-0.4.x.md)
- [Changelog v0.3.x](CHANGELOG-0.3.x.md)
- [Changelog v0.2.x](CHANGELOG-0.2.x.md)
- [Changelog v0.1.x](CHANGELOG-0.1.x.md)

---

## License

- [Nelson license](license.md)
