# WebAssembly FMU demos

Open **Examples > FMI > FMU Wasm - Adder (C / Rust / WAT)** in NFlow's
WebAssembly browser version. Each demo sends the constants 2 and 3 into a real
FMI 3.0 Co-Simulation component. The scope shows 5 after the first communication
step. The initial output is the FMU's start value, consistent with the existing
Co-Simulation block scheduling.

The three `.fmu` archives contain WebAssembly Component Model binaries under
`binaries/wasm32-wasip2/`, their `modelDescription.xml`, and the upstream license.
They contain no native shared libraries. `build-manifest.json` records their
SHA-256 checksums and the source revision:

<https://github.com/modelica/fmi-ls-wasm/tree/7ee00c5e002c0a4a0bc79a0fd9baf1d32dbf11b4>

The upstream example code is licensed under BSD-2-Clause; see
`LICENSE.fmi-ls-wasm.txt`. Rust and WASI compiler runtime code retains its
respective upstream licenses.

## Reference FMUs

The five archives in the parent directory now contain both their original native
binaries and a `binaries/wasm32-wasip2/<modelIdentifier>.wasm` component:

| Archive | NFlow diagrams |
| --- | --- |
| `BouncingBall.fmu` | BouncingBall CS and ME |
| `Dahlquist.fmu` | Dahlquist CS |
| `Feedthrough.fmu` | Feedthrough CS |
| `StateSpace.fmu` | StateSpace CS |
| `VanDerPol.fmu` | VanDerPol CS, ME, and the acausal RLC demo |

All eight diagrams open from **Examples > FMI** in the browser. They use the
same source equations as the desktop demos. The components compile the C sources
embedded in the Reference FMUs v0.0.40 archives, through an FMI-LS-WASM adapter;
no model equations are substituted. The native binaries and original source
files are preserved byte for byte. The FMI 2 archives remain native only.
`reference-fmus-build-manifest.json` records source hashes, tool versions, WIT
hashes and archive checksums. The Reference FMUs use BSD-2-Clause; see
`LICENSE.Reference-FMUs.txt`. The generated Nelson adapter uses LGPL-3.0-or-later.

## Supported browser interface

The browser adapter supports FMI-LS-WASM 3.0 Co-Simulation and Model Exchange,
numeric and Boolean ports, vector ports, scalar parameters and structural array
dimensions. String, Binary and Clock variables are excluded from signal ports.
Array parameters retain their FMU defaults unless configured by the component;
the inspector's parameter controls accept scalar values.

Model Exchange delegates integration, zero crossings and state reset events to
the NFlow solver. Co-Simulation components own their internal integration and
event handling. CS early return and host-managed CS event mode are not supported.
Native-only archives still require desktop Nelson.

Components are converted to JavaScript bindings and core Wasm modules locally,
then executed inside the Nelson Worker. The transpiler loads with the first FMU.

Uploaded archives belong to the current browser session. After reopening a saved
diagram in a new session, select its archive again with **Browse FMU**. These
bundled demos reload their companion archives automatically.

## Artifact provenance and tests

The bundled artifacts were built with WASI SDK 33, wasm-tools 1.251.0,
wit-bindgen 0.57.1 and Rust 1.97.1 with the `wasm32-wasip2` target.
The build manifests record the upstream revisions and artifact checksums.
The compiled FMUs are included directly; the regular NFlow build uses them
without recompiling their sources.

The browser regression suite is
`modules/nflow_gui/tests/wasm/fmu.spec.ts`. It imports each real archive, runs
each model twice, opens the packaged demos, and exercises the Nelson regression
`test_nflow_wasm_fmu_cs.m` in a Worker with a registered component.

After building `nelson-wasm-browser`, run the browser suite from
`modules/nflow_gui`:

```sh
node node_modules/playwright/cli.js test --config tests/wasm/playwright.config.ts
```

The browser regression suite also runs all eight reference diagrams,
including bouncing-ball events and the acausal circuit.

## Nelson module in the Wasm build

With NFlow enabled, the Wasm runtime includes `nflow_fmi` as a static module:
`ismodule('nflow_fmi')` and `__nflow_fmi_available__()` return true. Its backend
uses the Worker's component registry instead of loading native shared libraries.
`fmiInfo`, `fmiCoSimulate` and `fmiModelExchange` operate on a component already
registered in the current Worker, using the path returned by **Browse FMU**.
Opening a bundled diagram registers its component when simulation starts.

An arbitrary `.fmu` path does not synchronously compile a component: import it
first. FMI 2 native binaries, the external Modelica compiler and FMU state
serialization are outside this portable backend. The native module retains its
existing backend.
