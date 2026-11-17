---
name: nflow-physical-acausal
audience: user
modules: nflow_engine
description: Build physical (acausal) NFlow models - electrical, thermal, mechanical - by joining component pins into nodes, with grounds, sensors and the DAE solver. Use when the user models a real physical system rather than a signal block diagram.
triggers: physique, physical, acausal, acausale, electrique, electrical, circuit, rlc, resistance, resistor, condensateur, capacitor, bobine, inductor, thermique, thermal, chaleur, heat, mecanique, mechanical, masse, mass, ressort, spring, amortisseur, damper, pin, broche, noeud, node, ground, masse electrique, dae
functions: new_system, add_block, add_line, set_param, save_system, nflow_layout
---

# Physical (Acausal) Modeling

Model a real physical system as joined components, not a faked causal ODE of signal blocks.

## When to use
- Electrical circuits, thermal networks, translational/rotational mechanics.
- Any model where quantities are set by physical constraints, not a fixed signal direction.

## When NOT to use
- A causal signal block diagram (use the general NFlow build guide).

## Key functions
`new_system`, `add_block`, `add_line`, `set_param`, `save_system`, `nflow_layout`. Pins are joined with `add_line` using NAMED endpoints (not numbered ports). (Signatures in the Verified Nelson API block.)

## How acausal wiring works
A physical pin carries an across variable (voltage, temperature, position) and a through variable (current, heat flow, force). Joining pins makes them share one node, so direction is not fixed - that is what "acausal" means.

## Idioms & workflow
1. Drop acausal blocks and join pins with NAMED endpoints: `add_line(model, 'A/p', 'B/p')` - never fake the physics with a causal ODE of signal blocks.
2. Build a shared node with successive `add_line` calls that share a pin: `add_line(m,'L1/n','C1/p'); add_line(m,'C1/p','VS/p');` merges `{L1.n, C1.p, VS.p}` into one node.
3. Add a `Ground` on every electrical reference node, or the system is singular.
4. A `...Sensor` block has physical pins AND a numbered signal output: join its pins with named `add_line`, then read it with a numbered `add_line('m','Sensor/1','Scope/1')`.
5. Finish with `set_param(model, ..., 'Solver', 'dae')` and `nflow_layout(model)`.

## Gotchas
- Use the block paths and pin names from the live block catalog; a pin or block not listed does not exist.
- Mixing a named pin and a numbered port in one `add_line` is an error - both endpoints must be the same kind.
- Forgetting a `Ground`, or leaving the solver off `dae`, makes a physical model singular.

## Done when
Components are joined into physical nodes with grounds on reference nodes, sensor outputs read through numbered ports, the solver is `dae`, and the model simulates to a coherent result.
