---
name: nflow-code-generation
audience: user
modules: nflow_engine
description: Generate standalone C or Rust code from an NFlow model. Use when the user wants to export a model to source code for deployment, or asks why generation is rejected.
triggers: generation de code, code generation, generer, generate, codegen, nflow_codegenerate, c, rust, deploiement, deployment, standalone, embarque, embedded, exporter code, source
functions: nflow_codegenerate, sim, set_param
---

# Generate C or Rust from an NFlow Model

Export a model to standalone source for deployment, within the limits of what generation supports.

## When to use
- Turning a working model into C or Rust source for embedding/deployment.

## When NOT to use
- Building or simulating the model in Nelson (use the build/simulate guides).
- FMU/SSP exchange (use the FMU/SSP guide).

## Key functions
`nflow_codegenerate(model, destination, flag, language)` generates the source; validate the model with `sim` first; `set_param` fixes model settings before generating. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Make the model simulate cleanly first; generation only mirrors a model that already runs.
2. Pick the target language explicitly (`'C'` or `'rust'`); generate into a clean destination directory.
3. Keep the solver generation-friendly: a fixed-step solver generates to standalone source; advanced variable-step/implicit backends may not be supported for standalone output.
4. Build and run the generated harness to cross-check a few output values against the Nelson simulation.
5. Keep parameters as scalars/text where generation expects them.

## Gotchas
- Generation can be rejected for an unsupported construct (e.g. certain solver backends, or an "island" the generator cannot lower); read the error and simplify the model.
- A model that does not simulate will not generate; fix the simulation first.

## Done when
`nflow_codegenerate` produces C or Rust source in the destination, and the generated numeric result matches the Nelson simulation within tolerance.
