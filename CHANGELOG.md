# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 1.14.0 - UNRELEASED

### Added

- `scatter3`: 3-D scatter plot.
- `WindowState` property to Figure objects.
- `nebula` colormap.
- `colormaplist` List available colormaps.
- `arrayfun` apply function to each element of array.
- Support for `nix develop`, enabling a reproducible Bash shell preconfigured with Nelson’s build environment.
  Refer to [BUILDING.md](./BUILDING.md) for detailed usage instructions.
- Introduced a [`justfile`](https://just.systems/man/en/) to streamline and standardize the build process across all supported platforms.

### Changed

- Scatter symbols have been refined for greater pixel-perfect accuracy.
- `scatter` now returns a scatter graphic object instead of a line graphic object.
- `scatter` performance has been improved.
- `spy` uses scatter function and no more plot.
- Improved error message for parsing invalid anonymous functions.
- [#1378](http://github.com/nelson-lang/nelson/issues/1378) Boost 1.88 support.
- Qt 6.9.0 used on Windows x64.
- Minimum boost version was fixed to 1.71.
- JSON for Modern C++ version 3.12.0

### Technical Improvements

- Updated GitHub CI to use Windows 2025 for Windows builds.
- Significant rework of the GitHub CI workflow for improved reliability and maintainability.
- Updated Prettier to the latest version.
- Use shared library suffix CMAKE macro.
- Include the CPU target name in the package (on linux).
- Minimal dependencies on SLICOT used .

## 1.13.0 - (2025-03-29)

This release introduces performance improvements and new graphical capabilities while deprecating support for 32-bit Windows versions.

### Changed

- **Windows x64 Compatibility**: Now requires the [AVX2](https://en.wikipedia.org/wiki/Advanced_Vector_Extensions#CPUs_with_AVX2) instruction set.
- **Windows 32-bit Support**: Official distribution of 32-bit Windows binary versions has been discontinued.
- **macOS Optimization**: Builds for macOS with M-series chips now leverage native optimizations for improved performance.
- **Plot Performance**: Optimized `plot` and `plot3` functions for increased speed. Example:
  ```matlab
  tic(); plot(rand(300,300), rand(300,300)); toc();
  ```
- **Dependencies Updated**:
  - Upgraded `fmtlib` to version 11.1.3.
  - Intel Math Kernel Library (MKL) updated to 2025.0.1 on Windows.
- **Internal Enhancements**:

  - OpenMP multithreading macros have been reworked for better efficiency.

- `SLICOT` module incorporates SLICOT library 5.9, which is distributed under the BSD-3-Clause license.

  - `SLICOT` module available on all platforms by default.

- python 3.13.2 embedded on Windows

### Added

- **Double Buffering for Plots**:

  - Implemented double buffering to enhance the smoothness and responsiveness of graphical plots.
  - Significantly reduces flickering during graphical updates.

- **New Graphics Functions**:

  - `getframe`: Captures an axes or figure as a movie frame.
  - `movie`: Plays recorded movie frames.
  - `im2frame`: Converts an image to a movie frame.
  - `frame2im`: Returns image data associated with a movie frame.
  - `DevicePixelRatio`: New figure property to handle display scaling.

- **Graphics IO** module:

  - `imwrite`: create gif animations.
  - `imwrite`, `imread`: pcx, tiff file formats managed.
  - `imformats`: Manage image file format registry.

- **New Example**:

  - Added an example for connecting `ollama` with Nelson:
    ```matlab
    edit([modulepath('webtools'), '/examples/ollama/readme.md'])
    ```

- **CMake Enhancement**:

  - Introduced `ENABLE_AVX2` CMake option for systems that support AVX2.
  - CMake dependencies reworked.

### Fixed

- MacOs: Default terminal did not use monospaced font.

- Some warnings detected with PVS-studio

## 1.12.0 (2025-02-16)

### Added

- Julia interface (part 1):

  - `jlenv`: Change default environment of Julia interpreter.
  - `jlrun`: Run Julia statements from Nelson.
  - `jlrunfile`: Run Julia file from Nelson.
  - Major types conversions are available.
  - CMake: Optional Julia engine detection.

- `bar`, `scatter` manage color name and short colorname.
- Github CI Ubuntu 24.04 arm64 (Cobalt 100 processor).
- Github CI Snapcraft build amd64 and arm64.
- Snapcraft arm64.

### Changed

- Completion .m files allows execution without extension.
- [#1342](http://github.com/nelson-lang/nelson/issues/1342) Github CI - Ubuntu-20.04 hosted runner image removed.

### Fixed

- [#1346](http://github.com/nelson-lang/nelson/issues/1346) [display] integer in cell are displayed as double and not as integer.

## 1.11.0 (2025-01-11)

### Added

- [#1321](http://github.com/nelson-lang/nelson/issues/1321) `mustBeSparse` validator function.
- [#1322](http://github.com/nelson-lang/nelson/issues/1322) `cmdsep`: Command separator for current operating system.
- `urlencode`: Replace special characters in URLs with escape characters.
- `docroot`: Utility to retrieve or define the root directory of Nelson Help.
- `ismodule`: second input argument `isprotected` added.
- `editor('editor_command', cmd)` allows to change text editor in Nelson (for example: VS Code).
- `NELSON_RUNTIME_PATH` environment variable added by installer on Windows.
- `--vscode` command line argument added.
- NixOS 24.11 packaging (see [BUILDING_Linux.md](https://github.com/nelson-lang/nelson/blob/master/BUILDING_Linux.md)).

### Changed

- Help Center: Access documentation in your system's web browser. Previously, the documentation was opened in the embedded Help browser.
- CA certificate store update.
- fmt library dependency updated.
- BS::threadpool library dependency updated.
- Advanced terminal updated (common for all platforms without GUI, auto completion, search history).
- Python 3.13.1 supported.

### Fixed

- [#1324](http://github.com/nelson-lang/nelson/issues/1324) Cell display could not be interrupted.

## 1.10.0 (2024-12-14)

### Added

- `detectImportOptions`: Generate import options from the file's content.
- `readtable`: Read table from file.
- `writetable`: Write table to file.
- `readcell`: Read cell array from file.
- `writecell`: write cell array to file.
- `readmatrix`: read matrix from file.
- `writematrix`: write matrix to file.
- `csvread`: Read comma-separated value (CSV) file.
- `csvwrite`: Write comma-separated value (CSV) file.
- `dlmread`: Read ASCII-delimited file of numeric data into matrix.
- `realmin`: Smallest normalized floating-point number.
- [#1288](http://github.com/nelson-lang/nelson/issues/1288) `mustBeMatrix`, `mustBeRow`, `mustBeColumn` validator functions.
- `join`: Combine strings.
- [#1292](http://github.com/nelson-lang/nelson/issues/1292) Large Table Display.
- [#1290](http://github.com/nelson-lang/nelson/issues/1290) `VariableTypes` property for table: Specify the data types of table in Nelson.
- `hour`, `minute`, `second` component of input date and time.

### Changed

- `narginchk`, `nargoutchk` support for check only minimun arguments `narginchk(3, Inf)`.
- Fedora 41 CI
- `title`: `Visible` property is inherited from the parent if not explicitly defined.
- i18n: migration PO files to JSON.
- `dlmwrite`: rework the function to be more fast and robust.
- `strjust`: rework the function to be more fast and robust.
- `datenum`: support '' as format for compatibility.

### Fixed

- [#1303](http://github.com/nelson-lang/nelson/issues/1303) `datevec` result must be normalized.
- [#1297](http://github.com/nelson-lang/nelson/issues/1297) some features have no help files.
- [#1276](http://github.com/nelson-lang/nelson/issues/1276) micromamba macos build.

## 1.9.0 (2024-10-26)

### Added

- Table direct computation:

  - unary functions: `abs`, `acos`, `acosh`, `acot`, `acotd`, `acoth`,
    `acsc`, `acscd`, `acsch`, `asec`, `asecd`, `asech`,
    `asin`, `asind`, `asinh`, `atan`, `atand`, `atanh`,
    `ceil`, `cosd`, `cosh`, `cospi`, `cot`, `cotd`,
    `coth`, `csc`, `cscd`, `csch`, `exp`, `fix`,
    `floor`, `log`, `log10`, `log1p`, `log2`, `nextpow2`,
    `round`, `sec`, `secd`, `sech`, `sin`, `sind`,
    `sinh`, `sinpi`, `sqrt`, `tan`, `tand`, `tanh`,
    `var`, `acosd`, `not`.
  - binary functions: `plus`, `minus`, `times`, `eq`, `ge`, `gt`, `le`,
    `ne`, `lt`, `rdivide`, `rem`, `power`, `pow2`, `or`, `mod`, `ldivide`.

- `end` magic keyword can be overloaded for classes (applied to `table` class).
- [#1250](http://github.com/nelson-lang/nelson/issues/1250) `head`, `tail` functions for table and array.
- [#1248](http://github.com/nelson-lang/nelson/issues/1248) `removevars`, `renamevars` functions for table.

### Changed

- [#1259](http://github.com/nelson-lang/nelson/issues/1259) Add macOS Sequoia and remove macOS Monterey CI support.
- Qt 6.8 LTS support (used on Windows 64 bits binary).
- Python 3.13.0 on Windows.
- Boost 1.86 on Windows.

## 1.8.0 (2024-10-04)

### Added

- **`table` Data Type**:

  - Introduced the `table` data type, offering enhanced functionality for structured data manipulation.

  - Overloaded methods specific to the `table` data type:

    - `disp`, `display` for table display.
    - `horzcat`, `vertcat` for horizontal and vertical concatenation.
    - `isempty` to check if the table is empty.
    - `isequal`, `isequalto` for table comparison.
    - `properties` for accessing table metadata.
    - `subsasgn` for subscripted assignment.
    - `subsref` for subscripted referencing.

  - Conversion functions added:

    - `array2table`: Convert an array to a table.
    - `cell2table`: Convert a cell array to a table.
    - `struct2table`: Convert a structure to a table.
    - `table2array`: Convert a table to an array.
    - `table2cell`: Convert a table to a cell array.
    - `table2struct`: Convert a table to a structure.

  - Utility functions introduced:
    - `width`: Retrieve the number of columns in the table
    - `height`: Retrieve the number of rows in the table
    - `istable`: Check if a variable is of the `table` data type

- `Resize` - Resize figure property.
- [#36](http://github.com/nelson-lang/nelson/issues/36) `datenum` format compatibility extended.
- [#37](http://github.com/nelson-lang/nelson/issues/37) `datestr` Convert date and time to string format.

### Changed

- CodeQL Github action updated.

### Fixed

- fix 'units' refresh for 'axes' object.

## 1.7.0 (2024-08-28)

### Added

- `uicontrol` Create user interface control (button, slider, edit, list box, etc.).
- `waitfor` Block execution and wait for condition.
- `waitforbuttonpress` — Wait for click or key press.
- `im2double` — Convert image to double.
- `CloseRequestFcn` — Close request callback for `figure`.
- `CreateFcn` — Create callback for all graphic objects.
- `DeleteFcn` — Delete callback for all graphic objects.
- `BusyAction` — Busy action for all graphic objects.
- `Interruptible` — Interruptible property for all graphic objects.
- `BeingDeleted` — Being deleted property for all graphic objects.
- `KeyPressFcn`, `KeyReleaseFcn`, `ButtonDownFcn` properties for `figure`.

### Changed

- Refactor the internal implementation of the 'system' built-in function.

- Python 3.12.5 on Windows.

## 1.6.0 (2024-06-29)

### Added

- `unique`: Unique values.
- `ndgrid`: Rectangular grid in N-D space.
- `nthroot`: Real nth root of real numbers.
- `allfinite`: Check if all array elements are finite.
- `j` as imaginary unit number is also supported. example `3+2j` equivalent to `3+2i`.
- `FollowLocation` option for `weboptions`
- oneAPI Threading Building Blocks optional dependency.
- Ubuntu 24.04 debian package.
- Ubuntu 24.04 CI

### Changed

- `sort`: speed optimization.

- Windows dependencies updated and rebuild with minimal dependencies:

  - Qt 6.7.1,
  - Visual C++ 2022 Redistributable v14.40.33810.00,
  - boost 1.85,
  - Python 3.12.4,
  - Intel Math Kernel Library 2024.1.1,
  - Intel runtime,
  - SLICOT,
  - gettext 0.22.5,
  - cmake 3.30.0 rc3,
  - libsndfile 1.2.2,
  - portaudio 19.7.5,
  - taglib 2.0,
  - libzip1 1.3.1,
  - libcurl 8.8.0,
  - icu4c 74.2,
  - libffi 3.4.6,
  - libxml2 2.11.7

- Unicode® Standard, Version 15.1 support

- simdutf 5.2.8
- fast_float 6.1.1
- dtl 1.2.0

### Fixed

- [#1210](http://github.com/nelson-lang/nelson/issues/1210) `bode` did not unwrap phase.
- [#1206](http://github.com/nelson-lang/nelson/issues/1206) `balance` yields wrong Transformation Matrix.
- [#1205](http://github.com/nelson-lang/nelson/issues/1205) `diag` may return wrong sub-diagonals.
- [#1202](http://github.com/nelson-lang/nelson/issues/1202) buildhelpmd does not generate SUMMARY as expected.
- [#1201](http://github.com/nelson-lang/nelson/issues/1201) Matrix Exponential `expm` might give wrong results.
- [#1200](http://github.com/nelson-lang/nelson/issues/1200) Matrix Parsing/Evaluation trouble.

## 1.5.0 (2024-05-31)

### Added

- `dictionary` data type.

  - `dictionary`: Object that maps unique keys to values.
  - `configureDictionary`: Create dictionary with specified key and value types.
  - `insert`: Add entries to a dictionary.
  - `lookup`: Find value in dictionary by key.
  - `remove`: Remove dictionary entries.
  - `entries`: Key-value pairs of dictionary.
  - `keys`: Keys of dictionary.
  - `values`: Values of dictionary.
  - `types`: Types of dictionary keys and values.
  - `numEntries`: Number of key-value pairs in dictionary.
  - `isConfigured`: Determine if dictionary has types assigned to keys and values.
  - `isKey`: Determine if dictionary contains key.
  - `keyHash`: Generate hash code for dictionary key.
  - `keyMatch`: Determine if two dictionary keys are the same.

- `bernsteinMatrix`: Bernstein matrix.

- `orderedfields`: Order fields of structure array.

- Python interface (part 3):

  - [#1160](http://github.com/nelson-lang/nelson/issues/1160) Python operators in Nelson.
  - `keyHash`, `keyMatch` for python objects.
  - `isa` builtin support python types.
  - python dictionary to Nelson dictionary `dictionary(pyDict)`
  - conversion dictionary to python dictionary.

### Changed

- help files generated sorted by name on all platforms.
- on windows, Qt libraries used are in debug mode.

### Fixed

- [#1195](http://github.com/nelson-lang/nelson/issues/1195) `strcmp({'a'},["a"])` did not return expected value.

## 1.4.0 (2024-04-27)

### Added

- Python interface (part 2):

  - [#1168](http://github.com/nelson-lang/nelson/issues/1168) Run Python script file from Nelson.
  - [#1141](http://github.com/nelson-lang/nelson/issues/1141) Help about Managing Data between Python and Nelson.
  - [#1149](https://github.com/nelson-lang/nelson/issues/1149) python bytes, and bytearray types were not managed.
  - [#1163](https://github.com/nelson-lang/nelson/issues/1163) pyenv searchs python by version on Windows.
  - [#1164](https://github.com/nelson-lang/nelson/issues/1164) Embed python distribution on Windows.
  - [#1167](https://github.com/nelson-lang/nelson/issues/1167) Help about how to install Python package from Nelson.
  - numpy types support if numpy available.
  - `pyenv`: can use environment variables to set values.

- `getenv`: Retrieve the values of several environment variables.
- `pyrun`: Python code object allowed as first input argument.
- `nelson --without_python` starts nelson without python engine.
- `skip_testsuite`: allows to skip test suite dynamically on condition.

### Changed

- Allow to call method of a variable of CLASS/HANDLE type like a function (currently, only plugged for python subtype).
- [#1142](http://github.com/nelson-lang/nelson/issues/1142) Github Actions updated.
- [#1157](http://github.com/nelson-lang/nelson/issues/1157) Qt 6.7 support (used on Windows 64 bits binary).
- `copyfile`, `isfile`, `isdir`, `mkdir` allow string array type as input.
- warning about 'Matrix is singular to working precision' for inv matrix.
- tests webtools skipped if connection fails or not available.

### Fixed

- [#1144](http://github.com/nelson-lang/nelson/issues/1144) test_run markdown help file had a typo.
- [#1143](http://github.com/nelson-lang/nelson/issues/1143) Linux Snapcraft version did not allow to use python.
- [#1148](http://github.com/nelson-lang/nelson/issues/1148) pyrun('print(A)','A','A',string(NaN)) did not return expected value.
- `single(int64([1 2; 3 4]))` returned a wrong value.
- `py.tuple`, `py.list` compatibility increased.
- `pyenv` did not manage python's path with space on Windows.
- Matio 1.5.27 compatibility on ArchLinux.
- Ubuntu 24.04 LTS support.
- [#1178](http://github.com/nelson-lang/nelson/issues/1178) Fedora 40 support (CI).
- [#1134](http://github.com/nelson-lang/nelson/issues/1134) [CI] MacOS X Ventura restored.

## 1.3.0 (2024-03-30)

### Added

- Python interface (part 1):

  - CMake: Optional Python3 detection.
  - `pyenv` Change default environment of Python interpreter.
  - `pyrun` Run Python statements from Nelson.
  - Major types conversions are compatible (numpy in the next upcoming version).

- ArchLinux packaging (https://aur.archlinux.org/packages/nelson-git).
- `contour` Contour plot of matrix.
- `contour3` 3-D contour plot.
- `shiftdim` Shift array dimensions.
- `xcorr2` 2-D cross-correlation.
- `deconv` Deconvolution and polynomial division.
- `vecnorm` Vector-wise norm.
- `normpdf` Normal probability density function.
- [#310](http://github.com/nelson-lang/nelson/issues/310) `gammaln` Logarithm of gamma function.
- [#1112](http://github.com/nelson-lang/nelson/issues/1112) `gradient` Numerical gradient.
- [#1126](http://github.com/nelson-lang/nelson/issues/1126) `isspace` Determine which characters are space characters.

### Changed

- [#1110](http://github.com/nelson-lang/nelson/issues/1110) Eigen master branch (352ede96e4c331daae4e1be9a5f3f50fff951b8d) ready to use.
- [#1134](http://github.com/nelson-lang/nelson/issues/1134) [CI] MacOS X Ventura disabled (Install dependencies fails)
- `struct` supports scalar string array as field name.

### Fixed

- [#1110](http://github.com/nelson-lang/nelson/issues/1110) add help about build and use C/C++ on fly.
- [#1124](http://github.com/nelson-lang/nelson/issues/1124) unexpected result from long statements on Multiple Lines.
- [#1127](http://github.com/nelson-lang/nelson/issues/1127) Nelson could crash if an mxn characters is displayed in the variable browser.
- [#1125](http://github.com/nelson-lang/nelson/issues/1125) Unsupported colon operator with char operands.
- Missing 'zoom in', 'zoom out' icons for help viewer in linux package.
- `gcd` without argument returned wrong error message.
- [#1133](http://github.com/nelson-lang/nelson/issues/1133) [CI] [ARCH LINUX] Warning about MPI.

## 1.2.0 (2024-02-25)

### Added

- Recursive completion on Graphic handle, struct, handle, class (properties, methods).
- Adding links between documents about mex and supported compilers.
- GitHub CI for macOS Sonoma (Apple Silicon) support.
- `Export to ...` context menu for console and text editor as pdf.
- `CTRL + Mouse wheel` or `CTRL + +/-` to zoom in/out on console, editor, help.
- Toolbar for figure with print, zoom in, zoom out, rotation, pan, restore axes.
- `zoom `, `pan `, `rotate3d ` functions.
- `MenuBar`, `ToolBar` figure properties.
- Window menu on graphic window, list all others available windows.
- `feature` builtin (undocument features, debug, tests, ...) content can change with next releases.
- `GridAlpha`, `GridColor`, `View` properties for Axes.
- CTRL+C in help viewer, copy selected text.
- `checkupdate` function and check update menu.
- `isScalarStringArray` iinternal API C++ method.

### Changed

- Clicking on an axis automatically sets it as the current axes object.
- Clicking on an figure automatically sets it as the current figure object.
- `saveas` exports the figure as a PDF page with centered alignment.
- Default color of grid for axes.
- Default figure size updated.
- Default `MarkerFaceColor` value for compatibility.
- view function returns azimuth and elevation values.
- Camera view reworked.
- Minimal screen resolution supported 800x600.

### Fixed

- Change directory with file browser line editor did not work as expected.
- Template to create a function with file browser was wrong.
- Do not allow to select multiple variable in workspace browser.
- File browser checks if files with the extension ".m" have a valid name before enable 'run' context menu.
- Paste in editor with multiple tab.
- Starting the Nelson desktop was taking longer than necessary.

## 1.1.0 (2024-01-29)

### Added

- Nelson Desktop environment: file browser, command history, workspace browser, desktop layout.
- [#1074](http://github.com/nelson-lang/nelson/issues/1074) Roadmap v2.0.0
- [#1044](http://github.com/nelson-lang/nelson/issues/1044): LU matrix factorization.
- [#1080](http://github.com/nelson-lang/nelson/issues/1080) `LineStyle`, `LineWidth` properties were not implemented for surface objects.
- `sky`, `abyss` colormaps.

## 1.0.0 (2024-01-04)

Nelson 1.0.0 has been released.

Nelson is an interactive, fully functional environment for engineering and scientific applications. It implements a matrix-driven language (which is largely compatible with MATLAB and GNU Octave), with advanced features such as 2-D 3-D plotting, image manipulation and viewing, a codeless interface to external C/C++/FORTRAN libraries, native support for various C types, and a host of other features.

### Features

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

## Previous changelog

[Changelog v0.7.x](CHANGELOG-0.7.x.md)

[Changelog v0.6.x](CHANGELOG-0.6.x.md)

[Changelog v0.5.x](CHANGELOG-0.5.x.md)

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
