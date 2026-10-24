---
name: nflow-create-model
description: Create, save, validate, and test NFlow block-diagram models through the scripted API or the nflow-2 JSON format. Use when building a new .nflow example, generating a model programmatically, wiring blocks and subsystems, selecting a solver, adding physical connections, or preparing a model for simulation and C or Rust generation.
---

# Create an NFlow Model

Create a deterministic model from existing block contracts, then prove that it loads, simulates, and round-trips.

## Select the Authoring Path

Inspect a nearby runnable model under `modules/nflow_blocks/examples/` and the manifests that own its block types.

- Prefer `new_system`, `add_block`, `add_line`, `set_param`, and `save_system` for causal models supported by the scripted API.
- Edit `nflow-2` JSON directly only for schema features the API does not expose conveniently, such as physical nets or specialized nested metadata.
- Represent physical connections with `physicalWires`; keep sensor outputs and signal-driven source inputs in normal `connections`.

Keep changes unstaged, do not write Git history, do not edit generated locale files, and do not change the root build configuration.

Use this minimal persisted shape:

```json
{
  "version": "1.0.0",
  "format": "nflow-2",
  "name": "example",
  "blocks": [
    {
      "id": "src",
      "type": "constant",
      "x": 40,
      "y": 80,
      "params": { "Value": 1 }
    }
  ],
  "connections": [],
  "variables": {},
  "sampleTime": 0.01,
  "runtime": 10,
  "solver": "discrete"
}
```

Give every block a unique string `id`, a manifest-backed `type`, placement coordinates, and exact-case parameters. Represent a signal wire as `{ "from": "sourceId", "to": "sinkId", "fromIndex": 0, "toIndex": 0 }`; use consistent indices throughout a newly authored raw file. Represent an undirected physical net as `{ "pins": ["blockId.pinName", "otherId.pinName"] }` inside the top-level `physicalWires` array. Preserve recognized metadata and ignore unknown optional fields when rewriting a model.

## Build the Model

1. Resolve block paths from manifests under `modules/nflow_blocks/libraries/`; use `nflow/<library>/<type>` with the scripted API.
2. Copy parameter keys and defaults from the owning `library.json`. Preserve their exact case.
3. Give every block a stable, descriptive id or name. Keep layout coordinates clear enough for the editor.
4. Connect scripted ports with 1-based `Block/port` names. Raw JSON connection indices are normalized by readers, but new files should follow the nearest current example consistently.
5. Set model `StopTime`, `SampleTime`, variables, and solver explicitly when numerical behavior depends on them.
6. Add `toWorkspace`, scope, or label endpoints needed to assert results.

Use the default discrete path for legacy fixed-step behavior, `ode4` for global fixed-step RK4, `ode45` for native adaptive integration, and `dae` for acausal residual models. Do not choose CVODES or IDAS when standalone C/Rust generation is required.

Use `ode1` or `ode4` with `sampleTime` as the fixed step. Use `ode45`, `variableNonstiff`, `variableStiff`, `cvodesAdams`, or `cvodesBdf` with optional `relTol`, `absTol`, and `maxStep`. Require the optional solver backend for CVODES and IDAS choices. Use `dae` or `idas` for differential-algebraic physical islands and allow the solver to compute consistent initial conditions.

## Validate Behavior

1. Save the model to a path under the repository or Nelson's temporary directory.
2. Close and reload it to verify serialization.
3. Run `sim(model)` and assert status, time range, signal values, and tolerances rather than only checking that execution returned.
4. Open it in the editor when layout, ports, physical wires, or custom rendering changed.
5. If the model is intended for deployment, run `nflow_codegenerate` for both C and Rust and execute the generated numeric harness where available.

For repository examples, add a focused test under the owning NFlow module. Example sweeps must verify that every connection references a real block and that simulation completes. Keep examples small, named by purpose, and free of machine-specific paths.

## Done When

- The model loads after a save/close round trip.
- Every block type and parameter exists in the current manifests.
- Simulation proves the intended numerical behavior.
- The editor renders a coherent diagram when the model is user-facing.
- C/Rust generation is tested or the model clearly documents why it is runtime-only.
