# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## UNRELEASED

### Fixed

- [#674](http://github.com/nelson-lang/nelson/issues/674) Migrate sources to nelson-lang github organization.
- `cellfun` did not check type of second input argument.

### Added

- `filter` 1-D digital filter.

- control system module (part 2):
  - `evalfr` Evaluate system response at specific frequency.
  - `nyquist` Nyquist plot of frequency response.
  - `ord2` Generate continuous second-order systems.
  - `append` Group models by appending their inputs and outputs.
  - `feedback` Feedback connection of multiple models.
  - `series` Series connection of two models.
  - `ssdelete` Remove inputs, outputs and states from state-space system.
  - `ssselect` Extract subsystem from larger system.
  - `tzero` Invariant zeros of linear system.
  - `tf2ss` Convert transfer function filter parameters to state-space form.
  - `ss2tf` Convert state-space representation to transfer function.
  - `minreal` Minimal realization or pole-zero cancellation.
  - `ssdata` Access state-space model data.
  - `tfdata` Access transfer function data.
  - `gram` Controllability and observability Gramians.
  - `hsvd` Hankel singular values of a state-space or transfer function model.
  - `damp` Natural frequency and damping ratio.
  - `balreal` Gramian-based balancing of state-space realizations.
  - `lqry` Form linear-quadratic (LQ) state-feedback regulator with output weighting.
  - [#957](http://github.com/nelson-lang/nelson/issues/957) `dlqr` Linear-quadratic (LQ) state-feedback regulator for discrete-time state-space system.
  - [#961](http://github.com/nelson-lang/nelson/issues/961) `lqed` Discrete Kalman estimator design from continuous cost function.
  - [#960](http://github.com/nelson-lang/nelson/issues/960) `lqe` Kalman estimator design for continuous-time systems.
  - [#955](http://github.com/nelson-lang/nelson/issues/955) `lqr` Linear-Quadratic Regulator (LQR) design.
  - [#943](http://github.com/nelson-lang/nelson/issues/943) `dare` Solve discrete-time algebraic Riccati equations.
  - [#951](http://github.com/nelson-lang/nelson/issues/951) `care` Continuous-time algebraic Riccati equation solution.
  - [#945](http://github.com/nelson-lang/nelson/issues/945) `ctrbf` Compute controllability staircase form.
  - [#946](http://github.com/nelson-lang/nelson/issues/946) `ctrb` Controllability of state-space model.
  - [#963](http://github.com/nelson-lang/nelson/issues/963) `obsv` Observability matrix.
  - [#964](http://github.com/nelson-lang/nelson/issues/964) `obsvf` Compute observability staircase form.
  - [#949](http://github.com/nelson-lang/nelson/issues/949) `acker` Pole placement gain selection using Ackermann's formula.
  - [#950](http://github.com/nelson-lang/nelson/issues/950) `bdschur` Block-diagonal Schur factorization.
  - [#952](http://github.com/nelson-lang/nelson/issues/952) `cloop` Close unity feedback loops.
  - [#953](http://github.com/nelson-lang/nelson/issues/953) `compreal` Companion realization of transfer functions.
  - [#959](http://github.com/nelson-lang/nelson/issues/959) `gensig` Create periodic signals for simulating system response.

### Changed

- [#674](http://github.com/nelson-lang/nelson/issues/674) Migrate sources to nelson-lang github organization.
- [#775](http://github.com/nelson-lang/nelson/issues/775) `quit`, `exit`, `startup.m`, `finish.m` behavior reworked for compatibility.
- JSON for Modern C++ version 3.11.3 used on all platforms.
- {fmt} 10.1.1 (6f95000) used.

## 0.7.11 (2023-11-29)

### Added

- `hist` Histogram plot.
- `bar` Bar graph.
- `scatter` Scatter plot.
- `stem` Plot discrete sequence data.
- `stairs` Stairstep graph.
- `fill` 2-D patch.
- `pie` legacy pie chart.
- `subsref` Subscripted reference.
- `subsasgn` Redefine subscripted assignment.
- `substruct` Create structure argument for subsasgn or subsref.
- `deal` Distribute inputs to outputs.
- Intel compiler support.

### Changed

- axis limits recalculate with `hggroup`.
- `axes` forces focus on current axe.
- function_handle parenthese precedence.
- `patch` and `fill` manages `FaceAlpha`.
- visibility title and labels.
- object constructor must be in '@' directory and no more in parent directory (compatibility).
- `subsref`, `subsasgn` compatibility with `substruct`.
- To display a percent sign, you need to use a double percent sign (%%) in the format string (compatibility).
- French translation updated (100%, Thanks to weblate contributors)
- [#997](http://github.com/nelson-lang/nelson/issues/997) Macos BigSur Github CI support removed.
- Qt 6.6.1 on win64 CI build.

### Fixed

- `A = []; A(false) = zeros(3, 0)` did not return an empty matrix but an error.

## 0.7.10 (2023-10-27)

### Added

- private functions/folders support (to limit the scope of a function).
- syntax extended to facilitate the creation of literal integers without loss of precision:
  - example: `18446744073709551615u64`, `18446744073709551615i64` (similar to rust syntax)
- `flintmax('like', p)` syntax added.
- `int64`, `uint64` warning about double-precision.
- [#570](http://github.com/nelson-lang/nelson/issues/570) balance: Diagonal scaling to improve eigenvalue accuracy.
- `isobject` Check whether the input is an object.
- `cell2mat` Convert cell array of matrices to single matrix.
- [#948](http://github.com/nelson-lang/nelson/issues/948) `blkdiag` Create a block diagonal matrix from 2D matrices of different sizes.
- `kron` Kronecker tensor product.
- `strjust` Justify strings.

- control system module (part 1):

  - [#967](http://github.com/nelson-lang/nelson/issues/967) control system module template.
  - [#944](http://github.com/nelson-lang/nelson/issues/944) `mag2db`, `db2mag`, `pow2db`, `db2pow` functions.
  - [#968](http://github.com/nelson-lang/nelson/issues/968) `zp2tf`: Zero-pole to transfer function conversion.
  - [#954](http://github.com/nelson-lang/nelson/issues/954) `dcgain`: Low-frequency (DC) gain of LTI system.
  - [#965](http://github.com/nelson-lang/nelson/issues/965) `padecoef`: Padé approximation of time delays.
  - [#958](http://github.com/nelson-lang/nelson/issues/958) `esort`: Sort continuous-time poles by real part.
  - `dsort`: Sort discrete-time poles by magnitude.
  - [#962](http://github.com/nelson-lang/nelson/issues/962) `lyap`: Continuous Lyapunov equation solution.
  - `dlyap`: Discret Lyapunov equation solution.
  - `abcdchk` Verifies the dimensional compatibility of matrices A, B, C, and D.
  - `ss`: State-space model.
  - `tf`: Transfer function model (display, horzcat, vertcat, size).
  - `isct`: checks if dynamic system model is in continuous time.
  - `isdt`: checks if dynamic system model is in discret time.
  - `isstatic`: checks if model is static or dynamic.
  - `islti`: checks if variable is an linear model tf, ss or zpk.
  - `issiso`: checks if dynamic system model is a single input and single output.
  - `zero`: Zeros and gain of SISO dynamic system.
  - `pole`: Poles of dynamic system.
  - `bode`: Bode plot of frequency response, magnitude and phase data.

### Changed

- some modules (nig, modules_manager, help_browser) reworked to use private functions.
- Windows 64 bit CI and release use Qt 6.6.0

### Fixed

- [#940](http://github.com/nelson-lang/nelson/issues/940) title bar on dark theme on Windows.
- help viewer using dark theme.
- adjust position `xlabel` on `figure`.
- [#976](http://github.com/nelson-lang/nelson/issues/976) wrong output when reading a file with fscanf with size argument.
- [#975](http://github.com/nelson-lang/nelson/issues/975) Legend color (and width) is not matching that of curve in figure.
- [#988](http://github.com/nelson-lang/nelson/issues/988) anonymous function serialization '.^' and '^' are inversed.

## 0.7.9 (2023-09-18)

### Changed

- [#488](http://github.com/nelson-lang/nelson/issues/488) overloading functions:

  - all types including basic types can be overloaded.
  - overload is now fully compatible using '@' syntax and precedence.
  - all operators were reworked to support compatible overload.

- `.*` operator optimized.
- `conv2` optimized.
- Boost 1.82 used on Windows.

- Internals:

  - `class`, `function_handle` types reworked.
  - types order updated.
  - rework validator module.
  - functions finder reworked.
  - file watcher reworked.
  - operators reworked.
  - `repmat`, `ones`, `NaN`, `Inf` reworked.

- `function_handle` display is more compatible.

### Added

- [#491](http://github.com/nelson-lang/nelson/issues/491) Anonymous functions
- `--withoutfilewatcher` executable argument. disable file watcher for current session.
- `<--FILE WATCHER REQUIRED -->` test_run option.
- [#853](http://github.com/nelson-lang/nelson/issues/853) MacOs 13 ventura CI

### Fixed

- [#916](http://github.com/nelson-lang/nelson/issues/916) openblas micromamba on macos required to link libgfortran

## 0.7.5 (2023-05-27)

### Changed

- `BS::thread_pool` v3.5.0
- `simdutf` to 3.2.9.
- `{fmt}` to 10.0.0.
- `fast_float` to 4.0.0.
- `cast` reworked to be more compatible.
- `colon` reworked to be more compatible (operator uses unary overload).
- CMake 3.26.3 on Windows

## 0.7.4 (2023-04-27)

### Added

- Qt 6.5 support.
- [#802](http://github.com/nelson-lang/nelson/issues/802): `bitand`, `bitor`, `bitxor` functions.
- `issorted` Determine if array is sorted.
- `num2cell` Convert array to cell array with consistently sized cells.
- `hggroup` Create group object.
- `colorbar('off')` deletes colorbar associated with the current axes.
- `waterfall` Waterfall plot.
- `ribbon` ribbon plot.
- figure property: `GraphicsSmoothing` Axes graphics smoothing.
- text property: `FontSmoothing`.
- surf property: `MeshStyle`.
- chatGPT example.
- graphics examples about 3D polygons:
  - utah teapot example.
  - nefertiti mask example.
  - stanford bunny example.

### Changed

- Windows installer: allow to install for current user (no administrator rights required).
- `figure` without axes has a color.
- `figure` can be created not visible.
- `grid minor` toggles the visibility of the minor grid lines.
- `mesh` reworked.
- extraction on empty matrix for compatibility.
- `ones`, `eye`, `inf`, `nan` allow negative index (replaced by 0) for compatibility.
- Windows 64 bits version embeds Qt 6.5.
- allows `if` with empty statements

### Fixed

- `weboptions` did not manage HeaderFields as expected.
- update `cacert.pem`.
- [#895](http://github.com/nelson-lang/nelson/issues/895): Micromamba linux build fails after packages updates.

## 0.7.3 (2023-03-28)

### Added

- `patch` Create patches of colored polygons.
- `ancestor` Ancestor of graphics object.
- hexadecimal color code managed example: '#DDFF00'.
- `validatecolor` Validate color values.
- [#851](http://github.com/nelson-lang/nelson/issues/851): Build with micromamba environment (linux and macOS)

### Changed

- Figure property `Position` uses position based on bottom left position for compatibility.
- internal: boost no more used to read/write json files.
- internal: taglib library is optional.
- version date updated with each build.

### Fixed

- [#866](http://github.com/nelson-lang/nelson/issues/866): Close menu on figure can crash on linux.
- graphic hierarchy was not fully destroyed after `close` or `delete`.
- labels were not displayed correctly when the logarithmic scale was enabled.
- [#869](http://github.com/nelson-lang/nelson/issues/869): missing help files in linux package.

## 0.7.2 (2023-02-27)

### Changed

- cmake project reworked. It should be easier to package Nelson on linux platforms (Thanks to @JohanMabille)
- Debian package generated (beta - feedback welcome).
- `modulepath` reworked and extended.
- C++ API: `IsCellOfStrings(ArrayOf)` replaced by `ArrayOf::isCellArrayOfCharacterVectors()`
- C++ API: header `CheckHelpers.hpp` replaced by `InputOutputArgumentsCheckers.hpp`
- C++ API: `ToCellStringAsColumn` replaced by `ArrayOf::toCellArrayOfCharacterColumnVectors`
- `api_nelson` methods moved to type modules
- Remove internal circular dependency about error and warning.
- Exports minimum headers in package.

### Fixed

- `disp`, `display` did no more support overloading.
- `image` did not save all values for `XData` and `YData`.
- Github CI Monterey and Ubuntu 22.04 (dependencies install) fixed.
- some warnings.

## 0.7.1 (2023-01-29)

### Added

- `drawnow`: Update figures and process callbacks.
- `DrawLater` property added to `figure` graphics object.
- `interp1` linear interpolation 1D.
- [#736](http://github.com/nelson-lang/nelson/issues/736): `bone`, `cool`, `copper`, `hot`, `jet`, `pink`, `turbo`, `viridis`, `white` colormaps.
- `Visible` property to `figure` graphics object.
- [#809](http://github.com/nelson-lang/nelson/issues/809): `NumberTitle` property to `figure` graphics object.
- `AlphaMap` and `Colormap` properties added to `Axes` graphics object.
- `LineStyleOrder` property of 'axes' used for `plot` and `plot3`.
- `ColorOrderIndex` and `LineStyleOrderIndex` properties added to `axes` graphics object.
- `Interpreter` property added to `text` graphics object.
- tex special characters support for `text` and `ticks` graphics object.
- `delete` for graphics objects.
- `imread` Read image from graphics file.
- `imwrite` Write image to graphics file.
- `imshow` Display image.
- `surface` Primitive surface plot.
- [#808](http://github.com/nelson-lang/nelson/issues/808): `pcolor` Pseudocolor plot.
- `mesh` Mesh surface plot.
- `meshz` Mesh surface plot with curtain.
- [#807](http://github.com/nelson-lang/nelson/issues/807): `loglog` Log-log scale plot.
- `CHANGELOG` 0.7.x family.

### Changed

- Graphics objects property names check is strict (compatibility).
- Some speed optimization with graphics objects.
- `surf` reworked to use `surface`.

### Fixed

- [#823](http://github.com/nelson-lang/nelson/issues/823): default LineStyle for a line was wrong with marker.
- `CTRL+C` was not catched on advanced cli for linux and macos.
- colors in `colorbar` were not in the good order.
- warnings detected by CodeQL.
- [#824](http://github.com/nelson-lang/nelson/issues/824): VariableCompleter was not filtered by prefix.

## Previous changelog

[Changelog v0.6.x](CHANGELOG-0.6.x.md)

[Changelog v0.5.x](CHANGELOG-0.5.x.md)

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
