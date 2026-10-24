# FMI 3.0 headers (shipped)

These are the official FMI 3.0 C headers (`fmi3PlatformTypes.h`,
`fmi3FunctionTypes.h`, `fmi3Functions.h`), copied verbatim from the vendored
fmi4c third-party sources (`modules/nflow_fmi/src/fmi4c/3rdparty/fmi/`).

They are shipped here — and therefore installed alongside a distributed Nelson
— because `NFlow.exportfmu` compiles the generated FMU sources into a platform
binary and needs these headers at run time. The `src/` tree is not part of the
installer, so the source-tree copy is unavailable in a distributed build; this
`resources/fmi3/` copy is.

Each header carries its own copyright and the 2-Clause BSD license of the
Modelica Association Project "FMI". Keep this copy in sync with the vendored
fmi4c headers after any fmi4c upgrade.
