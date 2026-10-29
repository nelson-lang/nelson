# Example: custom C++ block library (cumprod)

This directory is a complete example of a native block library for nflow. It
provides one block, Cumulative Product (cumprod), and uses the full plugin API.
It also exports the optional `nfGetBlockCodegen` function, so the block can be
used by `nflow_codegenerate` to produce C and Rust code like a built-in block.

The full authoring guide is the `nflow-create-block` AI skill in
`modules/ai/skills-catalog/nflow/create-block/SKILL.md`.

| File          | Purpose                                                      |
| ------------- | ------------------------------------------------------------ |
| `library.json`| Library metadata, block list, phases, and block geometry     |
| `blocks.js`   | JS module with the SVG render functions (`blockDefinitions`) |
| `cumprod.cpp` | C++ simulation handler, compiled to a shared library         |
| `builder.m`   | Build script, based on the Nelson build tools                |
| `cumprod_demo.nflow` | Ready-to-run demo diagram (constant → cumprod → workspace) |

## The block

The block computes the running product of the input samples:

```
y(0) = u(0)
y(k) = y(k-1) * u(k)
```

It has one input and one output. The accumulator is reset to 1 when the
simulation starts.

## Directory layout

```
cumprod/
  library.json     required by the library loader
  blocks.js        required by the library loader (JS side)
  cumprod.cpp      native plugin source
  builder.m        build script (uses the Nelson dynamic_link tools)
  nflow_cumprod.*  produced by builder.m; the extension depends on the platform
```

## Building the native plugin

The plugin is built with the Nelson build tools, so there is no CMakeLists.txt
to write and no path to set by hand. The `builder.m` script uses the
`dynamic_link` module (`dlgeneratemake` and `dlmake`) together with the two
nflow helper functions `dlgetnflowincludes` and `dlgetnflowlibraries`. The same
script works both from the Nelson source tree and from an installation.

Requirements:

- A Nelson build or installation with the `nflow` and `dynamic_link` modules.
- A C++17 compiler. Test it with `havecompiler()`. On Windows, run
  `configuremsvc()` first if needed. See `doc supported_compiler`.

For an installed Nelson, the nflow headers (from `nflow_blocks`) and the
`nlsNflow_blocks` import library are part of the Dynamic link component. Select
that component when you install Nelson.

To build, run the following in Nelson from this directory:

```matlab
cd([modulepath('nflow_gui'), '/examples/libraries/cumprod']);
builder
```

The shared library is written next to `library.json`, with the name given in
the `native` field of the manifest:

- Windows: `nflow_cumprod.dll`
- Linux: `nflow_cumprod.so`
- macOS: `nflow_cumprod.dylib`

The file name must match the `native` field in `library.json`. The script reads
that field, so the two stay in sync.

### What builder.m does

```matlab
[ok, msg] = dlgeneratemake('dynamic_library', buildDir, 'nflow_cumprod', ...
  {source}, dlgetnflowincludes(), {}, dlgetnflowlibraries());
[ok, msg] = dlmake(buildDir);
```

- `dlgetnflowincludes()` returns the `nflow_blocks` headers and the Nelson SDK
  include directories. The plugin SDK needs no third-party header: the public
  headers keep every third-party type out of their surface, so the plugin builds
  even against an installed Nelson that ships no third-party headers. Defining
  `NFLOW_PUBLIC_SDK` forces that header-free path even inside the Nelson source
  tree.
- `dlgetnflowlibraries()` returns `nlsNflow_blocks`. That library exports the
  simulation helpers (`getInput`, `getState`, `setOutput`, `updated`, and so
  on).
- `dlgeneratemake` writes a CMake project in a temporary directory and `dlmake`
  builds it with the compiler that Nelson is configured to use. The script then
  copies the result into this directory and removes the temporary directory.

## Using this library in nflow

### In the nflow editor

1. Build the plugin (see above).
2. In the nflow Library Manager panel, click `+` (or add a search path) and
   select the parent directory of this folder, that is the `libraries/`
   directory that contains `cumprod/`.
3. Check Example C++ in the library list.
4. The Cum. Product block appears in the palette, in the Example C++ group.

### From Nelson commands

Add the directory that contains the `cumprod/` folder (the `libraries/`
directory) to the nflow search paths with `NFlow.addlibrarypath`. It scans that
directory and loads every sub-directory that holds a `library.json` next to its
native library, so the blocks become available to `sim` and code generation.

```matlab
% Build the plugin, then make it visible to the engine.
builder
NFlow.addlibrarypath([modulepath('nflow_gui'), '/examples/libraries']);

% Run the bundled demo diagram (constant -> cumprod -> workspace).
out = sim([modulepath('nflow_gui'), '/examples/libraries/cumprod/cumprod_demo.nflow']);
```

Related commands: `NFlow.librarypaths()` lists the current search paths and
`NFlow.rmlibrarypath(folder)` removes one. The editor manages the same paths
through the Library Manager panel.

## Plugin API reference

### library.json

The block geometry (`width`, `height`, `inputs`, `outputs`, `defaultParams`) is
defined here, next to the phases. The library loader builds the palette entry
and the template from the manifest.

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

### blocks.js

Only the SVG render functions are needed. The rest is read from the manifest.

```js
// Main form: blockDefinitions (type to { render })
export const blockDefinitions = {
  myBlock: {
    render(block, { svgRect, createSvgElement }) {
      // add SVG elements to block.group
    },
  },
};

// Compatible alias (still supported)
export const renderers = {
  myBlock: (block, helpers) => blockDefinitions.myBlock.render(block, helpers),
};
```

### Native plugin C API (BlockPlugin.h)

```c
// Required. ABI check: the loader rejects a plugin with a wrong version.
int nfGetAbiVersion(void);   // must return NF_PLUGIN_ABI_VERSION

// Required. Handler table.
const NfBlockHandlerEntry* nfGetBlockHandlers(int* count);

// Optional. Phases can also be read from library.json.
const char* nfGetPhaseInfo(void);
```

The `NfBlockHandlerFn` signature is typed against `SimEngineTypes.hpp`:

```cpp
// The C ABI passes opaque pointers. Cast them to the real types.
static int myHandler(void* ctx, const void* block, int phase)
{
    SimCtx&      simCtx = *static_cast<SimCtx*>(ctx);
    const Block& blk    = *static_cast<const Block*>(block);
    Phase        ph     = static_cast<Phase>(phase);
    // 0=INIT  1=OUTPUT  2=ALGEBRAIC  3=AFTER_STEP  4=UPDATE
    // 5=DERIVATIVE  6=ZERO_CROSSING  7=EVENT
}
```

The helper functions are declared in `SimEngineTypes.hpp` and exported by
`nlsNflow_blocks`:

| Helper                                  | Description                               |
| --------------------------------------- | ----------------------------------------- |
| `getInput(ctx, id, portIdx, fallback)`  | Read a scalar input on a port             |
| `getState(ctx, id)`                     | Get the mutable `BlockState&` of a block  |
| `setOutput(ctx, id, value, port=0)`     | Write a scalar to an output port          |
| `updated(ctx, id, newVal, port=0)`      | Write an output and return the converge flag |
| `param(block, vars, key, fallback)`     | Read a block parameter with substitution  |
