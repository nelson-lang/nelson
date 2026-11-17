---
name: nflow-simulate-inspect
audience: user
modules: nflow_engine
description: Run an NFlow model with the scripted API and read its results. Use when the user wants to simulate a model from the Command Window or a script (not build it on the pane canvas) and inspect the output.
triggers: simuler, simulation, simulate, lancer, run, sim, nflow, resultat, results, sortie, output, scope, toworkspace, stoptime, sampletime, temps de simulation, inspecter, inspect
functions: sim, set_param, get_param, new_system, add_block, add_line, save_system, bdclose, nflow_layout
---

# Simulate an NFlow Model and Inspect Results

Run a model through the scripted API and get its signals back for inspection.

## When to use
- Running an existing or scripted NFlow model from a script / the Command Window.
- Setting simulation time and step, then reading the output signals.

## When NOT to use
- Building a model on the live pane canvas (the embedded NFlow assistant builds there).
- Authoring new block types or libraries (that is development work).

## Key functions
`sim` runs the model; `set_param` / `get_param` set and read model and block parameters; `new_system`, `add_block`, `add_line`, `save_system`, `bdclose` manage the model; `nflow_layout` arranges blocks. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Set the run before simulating: `set_param(model, 'StopTime', t, 'SampleTime', dt, 'Solver', s)`.
2. Make outputs observable: wire a `toWorkspace` sink (for data) or a `scope` (for a quick look) before running.
3. Simulate with `sim(model)` and keep its result to read the logged signals.
4. Read or change a block value with `get_param(model, block, param)` / `set_param`.
5. Re-running the same model is fine; `bdclose(model)` first only when you want a clean rebuild.

## Gotchas
- Choose the solver for the model: `discrete`/`ode4`/`ode45` for causal signal models, `dae` for physical/acausal ones. (See the solver guide.)
- Every input port must be wired or the run reports an unconnected input instead of a result.
- Reduce `SampleTime` if a fast transient looks under-sampled.

## Done when
The model runs to `StopTime`, the requested signals are available from a `toWorkspace`/scope output, and the key values match what the user expected.
