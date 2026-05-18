# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.0.0 - (UNRELEASED)

### Added

- Added a bytecode virtual machine execution path for Nelson scripts, functions, and anonymous functions.
- Added bytecode VM coverage for recursive functions, indexing/extraction and insertion, `arguments` blocks, ignored outputs (`~`), `varargin`, and `varargout`.

### Changed

- Improved interpreter performance for loops, recursive calls, and anonymous function dispatch.
- Improved Ctrl+C responsiveness in interpreted loops by polling interrupts at bytecode statement boundaries and loop backedges.

## Previous changelog

[Changelog v1.0.x](CHANGELOG-1.0.x.md)

[Changelog v0.7.x](CHANGELOG-0.7.x.md)

[Changelog v0.6.x](CHANGELOG-0.6.x.md)

[Changelog v0.5.x](CHANGELOG-0.5.x.md)

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
