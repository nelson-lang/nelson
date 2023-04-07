# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## 0.7.4 (UNRELEASED)

### Added

- `hggroup` Create group object.
- chatGPT example.
- utah teapot example.

### Changed

- figure without axes has a color.
- `grid minor` toggles the visibility of the minor grid lines.
- `mesh` reworked.
- extraction on empty matrix for compatibility.

### Fixed

- `weboptions` did not manage HeaderFields as expected.
- update `cacert.pem`.

## 0.7.3 (2023-03-28)

### Added

- `patch` Create patches of colored polygons.
- `ancestor` Ancestor of graphics object.
- hexadecimal color code managed example: '#DDFF00'.
- `validatecolor` Validate color values.
- [851](http://github.com/Nelson-numerical-software/nelson/issues/851): Build with micromamba environment (linux and macOS)

### Changed

- Figure property `Position` uses position based on bottom left position for compatibility.
- internal: boost no more used to read/write json files.
- internal: taglib library is optional.
- version date updated with each build.

### Fixed

- [866](http://github.com/Nelson-numerical-software/nelson/issues/866): Close menu on figure can crash on linux.
- graphic hierarchy was not fully destroyed after `close` or `delete`.
- labels were not displayed correctly when the logarithmic scale was enabled.
- [869](http://github.com/Nelson-numerical-software/nelson/issues/869): missing help files in linux package.

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
- [736](http://github.com/Nelson-numerical-software/nelson/issues/736): `bone`, `cool`, `copper`, `hot`, `jet`, `pink`, `turbo`, `viridis`, `white` colormaps.
- `Visible` property to `figure` graphics object.
- [809](http://github.com/Nelson-numerical-software/nelson/issues/809): `NumberTitle` property to `figure` graphics object.
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
- [808](http://github.com/Nelson-numerical-software/nelson/issues/808): `pcolor` Pseudocolor plot.
- `mesh` Mesh surface plot.
- `meshz` Mesh surface plot with curtain.
- [807](http://github.com/Nelson-numerical-software/nelson/issues/807): `loglog` Log-log scale plot.
- `CHANGELOG` 0.7.x family.

### Changed

- Graphics objects property names check is strict (compatibility).
- Some speed optimization with graphics objects.
- `surf` reworked to use `surface`.

### Fixed

- [#823](http://github.com/Nelson-numerical-software/nelson/issues/823): default LineStyle for a line was wrong with marker.
- `CTRL+C` was not catched on advanced cli for linux and macos.
- colors in `colorbar` were not in the good order.
- warnings detected by CodeQL.
- [#824](http://github.com/Nelson-numerical-software/nelson/issues/824): VariableCompleter was not filtered by prefix.

## Previous changelog

[Changelog v0.6.x](CHANGELOG-0.6.x.md)

[Changelog v0.5.x](CHANGELOG-0.5.x.md)

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
