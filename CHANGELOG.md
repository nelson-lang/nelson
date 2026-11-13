# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.0.0 - (UNRELEASED)

Nelson 2.0 is a major release. Existing scripts run faster, larger projects can use packages and classes, and Nelson runs in a web browser. This release also includes NFlow, a block diagram editor for modeling and simulation.

### Added

#### Language

- Bytecode engine: loops and recursive functions run much faster, without changes to existing scripts.
- Object-oriented programming with `classdef`: value and handle classes, inheritance, enumerations, events and listeners, property validation, custom indexing, save and load.
- Packages, `import`, nested functions, local functions in scripts, and `arguments` blocks for input validation.
- Direct indexing of a call result, for example `f().Field` or `f()(1)`.

#### NFlow (1.0.0-beta.1)

- Block diagram editor for continuous, discrete, and physical (electrical, mechanical, thermal) models.
- Simulation from the editor or from scripts with `sim`, `linmod`, and `trim`, with fixed-step and adaptive solvers.
- C and Rust code generation, FMI 2.0/3.0 and SSP import and export.

#### Web (preview)

- Web desktop: Command Window, editor, workspace, and figures in a browser or in the `nelson-webview` window.
- WebAssembly build: Nelson runs entirely in a browser, with no installation and no server.

#### Desktop

- Debugger in the editor on the Qt and web desktops: breakpoints, stepping, and variable inspection.
- Notebooks (`.nlx`) with code, results, and text.
- Gallery of animated demos and a documentation window.

#### Data analysis

- `datetime`, `duration`, and `calendarDuration` with time zones and calendar arithmetic.
- `table`, `timetable`, and `timeseries` with joins, grouping, and missing data handling.
- `categorical` arrays.
- Statistics: distribution fitting, regression, hypothesis tests, clustering, and classification.
- 2-D and 3-D image processing.

#### Scientific computing

- ODE solvers (`ode45`, `ode15s`, ...), delay differential equations (`dde23`), and boundary value problems (`bvp4c`).
- Optimization: `fminsearch`, `fmincon`, `fsolve`, `linprog`, `quadprog`, and problem-based modeling.
- Optional GPU computing with `gpuArray` on Windows, Linux, and macOS, without CUDA.

#### Graphics and user interfaces

- Pie, donut, bubble, polar, and box charts, `isosurface`, annotations, and lighting.
- `uifigure` components: panels, tabs, grids, sliders, gauges, tables, and trees.

#### Interoperability

- `.xlsx` read and write on every platform, and optional NetCDF and Parquet support.
- `readlines`, `writelines`, and partial loading and saving of MAT-files with `matfile`.
- `nelson.io.zip`, `nelson.io.zstd`, `nelson.io.gzip` and `nelson.io.tar`: read and write ZIP entries, tar archives (plain, gzip or Zstandard), gzip and Zstandard data without extracting files, with `fopen` file identifiers on entries and compressed data, password-protected ZIP archives (WinZip AES and traditional encryption) and http/https sources.
- Python package to drive Nelson from Python, with NumPy and pandas conversion.
- Built-in MCP server for AI agents.
- Optional `compiler` module to build standalone applications and installers from `.m` files.

#### Developer tools

- `checkcode`, `nelson-lint`, and `nelson-lsp`.
- `nelson.unittest` reports in HTML, JSON, JUnit XML, and TAP13.
- `nmm`: dependency resolution, lockfiles, and publishing.

### Changed

- Breaking: `end` is required to close blocks, and a file whose name differs from its function name raises an error.
- Breaking: `isapprox` compares element by element and takes a tolerance level or `AbsoluteTolerance`/`RelativeTolerance`; the previous behavior is `isapproxnorm`.
- Linear algebra (`\`, `/`, `lu`, `det`, `chol`) is faster, handles `NaN`/`Inf` and edge cases more consistently, and gives the same results on every platform for small matrices.
- More options for existing functions: `'like'` for `eps`, `realmax`, `intmax`, ..., `save -struct`, full matrices in `svds`, unsorted points in `interp1` and `spline`, `hgtransform` parents for `plot`, `surf`, `patch`, and `text`.
- A function file on the path always takes precedence over a built-in of the same name.
- `filewrite` is about 6 times faster on large text.
- `patch` no longer widens manual axes limits (`XLim`, `YLim`, `ZLim`) to include the new patch.
- `xlim`, `ylim` and `zlim` accept and return `datetime` limits on datetime axes; `image`, `imagesc` and `imshow` fix manual limits to the image and leave a held axes unchanged.

### Removed

- COM support on Windows (`com_engine` module). Excel files are read and written with the native `.xlsx` functions.
- Nelson in the cloud (`sio_client` module), replaced by the web desktop and the WebAssembly build.
- Gateway generator (`nig` module).

## Previous changelog

[Changelog v1.x.x](CHANGELOG-1.x.x.md)

[Changelog v0.7.x](CHANGELOG-0.7.x.md)

[Changelog v0.6.x](CHANGELOG-0.6.x.md)

[Changelog v0.5.x](CHANGELOG-0.5.x.md)

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
