---
name: nflow-create-block
description: Create or extend NFlow simulation blocks, including runtime handlers, signal metadata, C and Rust code generation, palette manifests, icons, bilingual help, and regression tests. Use when adding a built-in NFlow block, implementing a native plugin block, or completing a block that is missing simulation, rendering, or code-generation behavior.
---

# Create an NFlow Block

Add the smallest complete block implementation and keep simulation, generated code, palette metadata, help, and tests consistent.

## Start From the Current Contract

1. Inspect the nearest block in the same category, its tests, and the category's `library.json`.
2. Decide whether the request is for a built-in block or an external native plugin. Use `modules/nflow_gui/examples/libraries/cumprod/` as an executable plugin reference.
3. Confirm parameter names, port counts, dimensions, types, phases, and solver semantics before editing.
4. Keep changes unstaged, do not write Git history, do not edit generated locale files, and do not change the root build configuration.

Do not infer parameter casing from a label. Treat the manifest and neighboring handlers as the contract.

Use these phase semantics:

- `INIT`: initialize state and allocate buffers before the time loop.
- `OUTPUT`: produce source output without reading signal inputs.
- `ALGEBRAIC`: compute feedthrough output from current inputs; tolerate repeated calls while solving loops.
- `DERIVATIVE`: return continuous-state slopes to a shared numerical solver.
- `ZERO_CROSSING`: return discontinuity surfaces used for event localization.
- `AFTER_STEP`: record results after state advancement.
- `UPDATE`: advance discrete state after output evaluation.

## Built-in Workflow

1. Add `modules/nflow_blocks/src/cpp/<category>/<block>.cpp` and declare it in `<category>_blocks.hpp`.
2. Register the handler and phases in `<Category>Handlers.cpp`. Add dimension, type, exact-64, complex, or zero-crossing metadata only when the behavior needs it.
3. Implement C and Rust generation as one `make<Block>Codegen(CodegenLang)` body and register one `BlockCodegenPair`.
4. Add the source to `nlsNflow_blocks.vcxproj` and its filters file. CMake discovers category sources automatically.
5. Add the block descriptor to `modules/nflow_blocks/libraries/<category>/library.json` and add a distinct SVG under `exports/` when it belongs in the palette.
6. Add matching help pages under `modules/nflow_blocks/help/en_US/xml/` and `fr_FR/xml/`.

Keep parsing in the descriptor/metadata layer and numerical behavior in the handler. Reuse `SimCtx`, `BlockDescriptor`, signal-layout, and codegen helpers before adding an abstraction.

For generated state, declare every value through the state pass, initialize it through the init pass, and use the generator's state placeholders instead of hand-building field names. For a continuous block, set `continuousWidth` to the packed-state count and implement all four `emitRk4` modes: gather state, scatter state, compute outputs from scattered state, and compute derivatives. Use `continuousWidth = 0` for an algebraic block that only contributes crossing surfaces. For each discontinuity, set `crossingCount` and emit its signed surface from the current scattered state. If this contract cannot be implemented, reject `ode4` and `ode45` generation explicitly instead of silently downgrading the solver.

## Plugin Workflow

Keep `library.json`, the native shared library, and optional `blocks.js` together. Export the ABI version and handler table required by `modules/nflow_blocks/src/include/BlockPlugin.h`; export phase information or codegen JSON only when needed. Give `native` a basename without a platform extension so the loader can resolve the host-specific filename.

## Tests

Add all applicable coverage:

- A focused simulation test in `modules/nflow_blocks/tests/`.
- Error and edge cases for missing inputs, invalid parameters, dimensions, and types.
- A codegen fixture and expected C output under `modules/nflow_gui/tests/<category>/`, plus the owning `test_nf_codegenerate_<category>.m` test.
- Numeric C and Rust execution through `nflow_codegen_check_numeric` when the block computes numerical results.
- Renderer or manifest tests in `modules/nflow_gui/src/ts/` when UI behavior changes.

Run the focused tests, `test_run('nflow_blocks')`, relevant `nflow_engine` codegen tests, and the NFlow GUI unit tests. Format touched C/C++ with the repository WebKit style. Never update golden output unless the behavior change is intentional and explained.

## Done When

- The block simulates through every declared phase.
- C and Rust support are paired, or unsupported generation fails explicitly.
- The palette descriptor, icon, help, and implementation agree.
- New tests pass on simulation and generated code paths.
- Windows and CMake source lists include every new compiled file.
