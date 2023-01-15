# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## 0.7.1 (UNRELEASED)

### Added

- `drawnow`: Update figures and process callbacks
- `DrawLater` property added to `figure` graphics object.
- `interp1` linear interpolation 1D.
- [736](http://github.com/Nelson-numerical-software/nelson/issues/736): `bone`, `cool`, `copper`, `hot`, `jet`, `pink`, `turbo`, `viridis`, `white` colormaps.
- `Visible` property to `figure` graphics object.
- [809](http://github.com/Nelson-numerical-software/nelson/issues/809): `NumberTitle` property to `figure` graphics object.
- `AlphaMap` and `Colormap` properties added to `Axes` graphics object.
- `LineStyleOrder` property of 'axes' used for `plot` and `plot3`.
- `ColorOrderIndex` and `LineStyleOrderIndex` properties added to `axes` graphics object.
- `Interpreter` property added to `text` graphics object.
- tex special characters support for `text` graphics object.
- `delete` for graphics objects.
- `imread` Read image from graphics file.
- `imwrite` Write image to graphics file.
- `imshow` Display image.
- `CHANGELOG` 0.7.x family.

### Changed

- Graphics objects property names check is strict (compatibility).

- Some speed optimization with graphics objects.

### Fixed

- [#823](http://github.com/Nelson-numerical-software/nelson/issues/823): default LineStyle for a line was wrong with marker.

- `CTRL+C` was not catched on advanced cli for linux and macos.

- colors in `colorbar` were not in the good order.

- warnings detected by CodeQL.

## Previous changelog

[Changelog v0.6.x](CHANGELOG-0.6.x.md)

[Changelog v0.5.x](CHANGELOG-0.5.x.md)

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
