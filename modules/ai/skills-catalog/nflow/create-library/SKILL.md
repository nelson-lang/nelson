---
name: nflow-create-library
description: Create and validate reusable NFlow block libraries, including library.json manifests, declarative or custom renderers, SVG assets, native plugin wiring, discovery, packaging, and tests. Use when adding a built-in palette family, assembling a third-party block library, or changing an existing library's manifest and loading behavior.
---

# Create an NFlow Library

Build a discoverable library whose manifest is the single source of truth for palette metadata and block templates.

## Choose the Library Kind

- For a built-in palette family, work under `modules/nflow_blocks/libraries/<library>/` and register the manifest in `modules/nflow_gui/src/ts/app/blocks-registry.ts` when a new family is introduced.
- For an external library, create a self-contained directory with `library.json`, optional SVG assets or `blocks.js`, and an optional native plugin.
- For one new block in an existing family, use `nflow-create-block` instead.

Inspect the closest existing manifest and its loader and renderer tests before editing. Keep changes unstaged, do not write Git history, do not edit generated locale files, and do not change the root build configuration.

## Author the Manifest

1. Give the library a stable, unique `id`, a human-readable `title`, a version, `format: "nflow-2"`, license metadata, and an ordered `blocks` array.
2. For each block, define a unique `type`, label, phases, size, ports, and every default parameter consumed by its handler.
3. Prefer a declarative `render` descriptor. Add a TypeScript/JavaScript renderer only when the generic renderer cannot express the visual.
4. Reference assets by filename and keep each built-in SVG in the library's `exports/` directory.
5. Set `native` only when a shared library supplies simulation handlers. Omit platform extensions so the loader can append them.

Do not duplicate geometry or defaults in renderer code. Keep the manifest usable without loading executable JavaScript.

Use this minimal manifest contract:

```json
{
  "id": "example.controls",
  "title": "Controls",
  "version": "1.0.0",
  "format": "nflow-2",
  "license": "BSD-3-Clause",
  "blocks": [
    {
      "type": "gain",
      "label": "Gain",
      "phases": ["ALGEBRAIC"],
      "width": 100,
      "height": 80,
      "inputs": [{ "x": 0, "y": 40, "side": "left" }],
      "outputs": [{ "x": 100, "y": 40, "side": "right" }],
      "defaultParams": { "Gain": 2 },
      "render": { "type": "math", "formula": "{params.Gain}" }
    }
  ]
}
```

Require a unique library `id` and a unique `type` per block. Match each type to its simulation handler. Interpret port coordinates relative to the block's top-left corner; use `wireX` and `wireY` only when the wire attachment differs from the port hit area. Keep phase order canonical: `INIT`, `OUTPUT`, `ALGEBRAIC`, `DERIVATIVE`, `ZERO_CROSSING`, `AFTER_STEP`, `UPDATE`.

Use a `math` render for parameterized formulas, a `plot` render for SVG paths based on axis geometry, an `image` render for an asset relative to `library.json`, and a `toggle` render for interactive binary state. Parameter interpolation belongs only in `math` formulas. If no render descriptor or custom renderer exists, expect the generic labeled-rectangle fallback.

## Wire Discovery and Packaging

Built-in manifests and assets are copied from `modules/nflow_blocks/libraries/` by `modules/nflow_gui/scripts/prepare-dist.mjs`; the module installers already ship the complete libraries directory. A new built-in family still needs its import and registration in `blocks-registry.ts`, plus any family-specific renderer module.

External libraries are found through Library Manager search paths, or added from a script with `NFlow.addlibrarypath`. Keep the plugin filename beside the manifest and verify that the manifest's `native` value matches the built artifact.

## Validate

- Parse `library.json` as strict JSON.
- Run the NFlow GUI build so `prepare-dist.mjs` copies and validates the library.
- Run manifest, loader, exhaustive renderer, icon, and template tests in `modules/nflow_gui`.
- For a native plugin, build and load the library, simulate a minimal diagram, and test optional C/Rust generation.
- For a built-in family, add or extend Nelson tests in `modules/nflow_blocks/tests/` and confirm installed-data packaging tests still pass.

Use `modules/nflow_gui/examples/libraries/cumprod/` as the complete external native-library example. Build its native plugin with Nelson's own tools by running the bundled `builder.m` (it calls `dlgeneratemake` / `dlmake` with the nflow SDK resolvers `dlgetnflowincludes` / `dlgetnflowlibraries`), rather than a hand-written `CMakeLists.txt`.

## Done When

- The library appears exactly once in discovery and its blocks render from manifest defaults.
- Every asset and optional native binary resolves from the library directory.
- Simulation and code generation either work or fail with an explicit unsupported diagnostic.
- Source, distribution, and installed layouts contain the same manifest and assets.
