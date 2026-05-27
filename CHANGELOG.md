# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.0.0 - (UNRELEASED)

### Added

- Added a bytecode virtual machine execution path for Nelson scripts, functions, and anonymous functions.
- Added bytecode VM coverage for recursive functions, indexing/extraction and insertion, `arguments` blocks, ignored outputs (`~`), `varargin`, and `varargout`.
- Added `classdef` value classes, handle classes, properties, methods, inheritance, packages, separate method files, method and property attributes, events/listeners, enumerations, static members, constants, dependent properties, observable properties, object arrays, custom indexing hooks, and metadata introspection.
- Added class-related builtins and compatibility entry points including `metaclass`, `events`, `enumeration`, `addlistener`, `listener`, `notify`, and `completion`.
- Added table functions `join`, `innerjoin`, `outerjoin`, `mergevars`, `splitvars`, `rows2vars`, `stack`, `unstack`, `convertvars`, `vartype`, `varfun`, and `rowfun`.
- Added table overloads for row-based set operations and comparisons through `unique`, `ismember`, `union`, `intersect`, `setdiff`, and `setxor`.
- Added data analysis functions `union`, `intersect`, `setdiff`, `setxor`, `rmmissing`, `fillmissing`, `standardizeMissing`, `groupcounts`, and `groupsummary`, with table-aware behavior where applicable.
- Added thread-safe parser execution using per-parse parser and lexer context objects.
- Added Flex C++ lexer and Bison C++ parser generated sources as the default committed parser/lexer path.
- Added parser support for nested functions, trailing local functions in scripts, EOF-terminated simple function files, and temporary result indexing such as `f().Field`, `f()(1)`, and `f(){1}`.
- Added parser/lexer regression coverage for multiline comments, continuations, transpose disambiguation, virtual commas, command shortcuts, name-value syntax, `arguments` blocks, `global` and `persistent` declarations, nested closures, debugger/profiler integration.
- Added a native Nelson Engine C API in `modules/engine` for external clients, including start, connect, find, eval, workspace get/put, visibility, and close entry points.
- Added the `nelson` Python package and `nelson.engine` API with entry points: `start_nelson`, `connect_nelson`, `find_nelson`, `NelsonEngine`, `FutureResult`, `EngineError`, and `NelsonExecutionError`.
- Added Python engine support for dynamic Nelson function calls, `eval`, `feval`, workspace access, `nargout`, asynchronous calls, stdout/stderr forwarding, owned-session close, connected-session detach, and object-handle pass-through.
- Added Nelson Python array classes for dense numeric, complex, logical, integer, character, sparse, cell, struct, dictionary, and table-compatible values, with NumPy and pandas interoperability when available.

### Changed

- Extended `class`, `isa`, `isobject`, `methods`, `properties`, `fieldnames`, `struct`, `clear`, `save`, `load`, `savemat`, `loadmat`, debugger breakpoints, profiler line tracking, and text completion to understand `classdef` objects while preserving legacy `@class` and `class(s, 'Name')` behavior.
- Improved bytecode VM command queue polling to keep IPC responsiveness without taking a mutex on every empty statement boundary.
- Improved interpreter performance for loops, recursive calls, and anonymous function dispatch.
- Improved Ctrl+C responsiveness in interpreted loops by polling interrupts at bytecode statement boundaries and loop backedges.
- Improved syntax compatibility in the parser/lexer by requiring `end` instead of `endfunction`, rejecting Nelson-only slash keyword call syntax, rejecting invalid command-style `global` and `persistent` declaration operands, and reporting filename/function-name mismatches as parser errors.
- Improved parser behavior for command-form builtins with no returned values, so display-only commands such as `dir` keep display semantics instead of forcing an output value.
- Improved parser/lexer documentation and help files for function syntax, nested functions, script local functions, and temporary result indexing.
- Improved HDF5/NH5 object interoperability so `h5write`/`h5read` can round-trip legacy `@class` objects and `classdef` value/handle objects using Nelson object metadata.

### Fixed

- Fixed bytecode indexed assignment so repeated assignments such as `f(k) = parfeval(...)` preserve existing local handle arrays instead of rebuilding from an undefined global base.
- Fixed bytecode event processing responsiveness for queued IPC commands by processing non-empty command queues at statement boundaries.
- Fixed deletion of pending parallel future handle arrays to avoid invalidating future objects still referenced by background worker threads.
- Fixed completion crashes and invalid-handle behavior for `classdef` object and object-array completions.

## Previous changelog

[Changelog v1.0.x](CHANGELOG-1.0.x.md)

[Changelog v0.7.x](CHANGELOG-0.7.x.md)

[Changelog v0.6.x](CHANGELOG-0.6.x.md)

[Changelog v0.5.x](CHANGELOG-0.5.x.md)

[Changelog v0.4.x](CHANGELOG-0.4.x.md)

[Changelog v0.3.x](CHANGELOG-0.3.x.md)

[Changelog v0.2.x](CHANGELOG-0.2.x.md)

[Changelog v0.1.x](CHANGELOG-0.1.x.md)
