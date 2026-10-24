# Example: Custom C++ Block Library - `cumprod`

This directory is an end-to-end example of how to create a **native block library** for nflow.
It implements a single block, **Cumulative Product** (`cumprod`), using the full plugin API —
including the optional `nfGetBlockCodegen` export so the block participates in
`nflow_codegenerate` (C and Rust) exactly like a built-in.

> Full authoring guide: see the `nflow-create-block` AI skill in `modules/ai/skills-catalog/nflow/create-block/SKILL.md`

| File             | Purpose                                                      |
| ---------------- | ------------------------------------------------------------ |
| `library.json`  | Library metadata, block list, phases **and block geometry**  |
| `blocks.js`      | JS module: SVG render functions (`blockDefinitions` export)  |
| `cumprod.cpp`    | C++ native simulation handler (compiled to a shared library) |
| `CMakeLists.txt` | CMake build script for the shared library                    |

---

## Block: Cumulative Product

Computes the running product of all input samples:

```
y(0) = u(0)
y(k) = y(k−1) × u(k)
```

One input, one output. The accumulator resets to 1 when the simulation is initialized.

---

## Directory Layout

```
cumprod/
  library.json          ← required by the library loader
  blocks.js              ← required by the library loader (JS side)
  cumprod.cpp            ← native plugin source
  CMakeLists.txt         ← build script
  nflow_cumprod(.dll, .dylib, .so) ← platform extension automatically appended
```

---

## How to Build the Native Plugin

### Prerequisites

- CMake ≥ 3.16
- A C++17 compiler (MSVC, GCC, Clang)
- A built nflow installation (so `nlsNflow_blocks` import-lib / shared-lib exists)

### Steps

```bash
# From this directory:
cmake -S . -B build \
    -DNFLOW_ROOT=/path/to/nflow

cmake --build build --config Release
cmake --install build
```

On Windows (MSVC), `nflow_cumprod.dll` will be copied here.
On Linux, `nflow_cumprod.so`. On macOS, `nflow_cumprod.dylib`.

> **Note**: the filename must match the `"native"` field in `library.json`.

---

## How to Use This Library in nflow

1. Build the plugin (see above).
2. In nflow's Library Manager panel, click **+** (or add a search path) and point it at the
   parent directory of this folder (i.e., the `libraries/` directory containing `cumprod/`).
3. Tick **Example C++** in the library list.
4. The **Cum. Product** block appears in the palette under the _Example C++_ group.

---

## Plugin API Reference

### `library.json` schema

Block geometry (`width`, `height`, `inputs`, `outputs`, `defaultParams`) lives
here alongside phases - the library-loader derives the palette descriptor
and template skeleton from the manifest automatically.

```json
{
  "id": "<unique-id>",
  "title": "Human-readable name",
  "version": "1.0.0",
  "native": "nflow_mylib",
  "blocks": [
    {
      "type": "myBlock",
      "label": "My Block",
      "phases": ["INIT", "ALGEBRAIC", "UPDATE"],
      "width": 80,
      "height": 60,
      "inputs": [{ "x": 0, "y": 30, "side": "left" }],
      "outputs": [{ "x": 80, "y": 30, "side": "right" }],
      "defaultParams": {}
    }
  ]
}
```

### `blocks.js` exports

Only SVG render functions are needed - everything else is derived from the manifest.

```js
// Primary form – blockDefinitions (type → { render })
export const blockDefinitions = {
  myBlock: {
    render(block, { svgRect, createSvgElement, /* … */ }) {
      // populate block.group with SVG elements
    },
  },
};

// Backward-compatible alias (still supported)
export const renderers = {
  myBlock: (block, helpers) => blockDefinitions.myBlock.render(block, helpers),
};
```

### Native plugin C API (`BlockPlugin.h`)

```c
// Required – ABI handshake (loader refuses mismatched plugins)
int nfGetAbiVersion(void);   // must return NF_PLUGIN_ABI_VERSION

// Required – handler table
const NfBlockHandlerEntry* nfGetBlockHandlers(int* count);

// Optional (phases can come from library.json instead)
const char* nfGetPhaseInfo(void);
```

`NfBlockHandlerFn` signature (typed against `SimEngineTypes.hpp`):

```cpp
// C ABI wrapper receives opaque pointers; cast to the real types:
static int myHandler(void* ctx, const void* block, int phase)
{
    SimCtx&      simCtx = *static_cast<SimCtx*>(ctx);
    const Block& blk    = *static_cast<const Block*>(block);
    Phase        ph     = static_cast<Phase>(phase);
    // 0=INIT  1=OUTPUT  2=ALGEBRAIC  3=AFTER_STEP  4=UPDATE
    // 5=DERIVATIVE  6=ZERO_CROSSING  7=EVENT
    ...
}
```

Available helper functions (declared in `SimEngineTypes.hpp`):

| Helper                                       | Description                                       |
| -------------------------------------------- | ------------------------------------------------- |
| `getInput(ctx, id, portIdx, fallback)`       | Read scalar input at port                         |
| `getState(ctx, id)`                          | Get mutable `BlockState&` for the block           |
| `setOutput(ctx, id, value, port=0)`          | Write a scalar to an output port                  |
| `updated(ctx, id, newVal, port=0)`           | Write output and return convergence flag          |
| `param(block, vars, key, fallback)`          | Read a block parameter with variable substitution |
