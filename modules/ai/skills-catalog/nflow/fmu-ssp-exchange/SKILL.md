---
name: nflow-fmu-ssp-exchange
audience: user
modules: nflow_fmi
description: Exchange models with other tools through FMI - import/run an FMU, turn an FMU into a block, and handle SSP system packages. Use when the user works with .fmu or .ssp files or Modelica-to-FMU.
triggers: fmu, fmi, co-simulation, cosimulation, model exchange, ssp, echange, exchange, importer modele, import model, modelica, modelicatofmu, bloc fmu, fmu block, interoperabilite, interoperability
functions: fmiInfo, fmiCoSimulate, fmiModelExchange, fmuToBlock, modelicaToFmu
---

# FMU and SSP Exchange

Bring a model in from, or send one out to, other tools through the FMI/SSP standards.

## When to use
- Inspecting or running an `.fmu` (co-simulation or model-exchange).
- Turning an `.fmu` into an NFlow block, or converting Modelica to an FMU.
- Importing/exporting an `.ssp` system package.

## When NOT to use
- Standalone C/Rust generation (use the code-generation guide).

## Key functions
Inspect/run an FMU: `fmiInfo`, `fmiCoSimulate`, `fmiModelExchange`. Wrap an FMU as a block: `fmuToBlock`. From Modelica: `modelicaToFmu`. For SSP system packages, the `NFlow` class methods `NFlow.sspInfo`, `NFlow.sspImport`, `NFlow.sspExport` and `NFlow.exportfmu` handle inspect/import/export. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Inspect before running: `fmiInfo(fmu)` lists inputs, outputs, and the FMI kind so you wire it correctly.
2. Run an FMU directly with `fmiCoSimulate(fmu, tStop, dt, inputs, relTol)` (co-sim) or `fmiModelExchange` (model exchange).
3. To use an FMU inside a model, wrap it with `fmuToBlock(fmu, ...)` and wire it like any block.
4. Export a model as an FMU with `NFlow.exportfmu(modelFile)`; inspect/import/export SSP with the `NFlow.ssp*` methods.
5. Convert a Modelica component to an FMU with `modelicaToFmu` before importing.

## Gotchas
- Match the FMI kind to the call: a co-simulation FMU and a model-exchange FMU take different entry points.
- Input names and causality come from `fmiInfo`; a wrong connector name fails the run.
- Tolerance and step must suit the FMU's dynamics; too large a step diverges.

## Done when
The FMU/SSP is inspected and run or wrapped (or the model exported) with connectors matched to `fmiInfo`, and a short run cross-checks the expected values.
