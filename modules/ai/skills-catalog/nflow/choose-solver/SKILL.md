---
name: nflow-choose-solver
audience: user
modules: nflow_engine
description: Choose the right NFlow solver for a model and set it. Use when the user asks which solver to use, why a simulation is slow/unstable/inaccurate, or how to set StopTime and SampleTime.
triggers: solveur, solver, discrete, ode4, ode45, dae, pas, step, sampletime, stoptime, stiff, raide, instable, unstable, precision, accuracy, choisir solveur, choose solver, integrateur, integrator
functions: set_param, sim, get_param
---

# Choose an NFlow Solver

Match the solver to the model so the simulation is accurate, stable, and fast.

## When to use
- Deciding which solver a model needs.
- Diagnosing a run that is slow, unstable, or under-sampled.

## When NOT to use
- Building or wiring the model (use the NFlow build / physical guides).

## Key functions
Set the solver and timing with `set_param(model, 'Solver', s, 'StopTime', t, 'SampleTime', dt)`; run with `sim`; read current settings with `get_param`. (Signatures in the Verified Nelson API block.)

## Choosing the solver
- `discrete` - default fixed step; for purely discrete / sampled signal models.
- `ode4` - fixed-step Runge-Kutta 4; smooth causal continuous models when a fixed step is wanted. Set `SampleTime` as the step.
- `ode45` - variable step; general continuous non-stiff models; it adapts the step for accuracy.
- `dae` - differential-algebraic; REQUIRED for physical / acausal models (electrical, thermal, mechanical) where variables are coupled by constraints.

## Idioms & workflow
1. Physical/acausal model (joined pins, Ground, sensors)? Use `dae` - the others cannot solve the constraints.
2. Smooth continuous dynamics and you want adaptivity/accuracy? `ode45`. Need a fixed step (e.g. for a fixed-rate pipeline)? `ode4`.
3. Discrete/sampled logic only? `discrete`.
4. Set `StopTime` to cover the behaviour of interest and `SampleTime` small enough to resolve the fastest transient.
5. If a run looks jagged, reduce `SampleTime`; if it blows up on a physical model, confirm the solver is `dae` and every reference node has a `Ground`.

## Gotchas
- A physical model on `discrete`/`ode4`/`ode45` is singular or wrong - it needs `dae`.
- Too large a `SampleTime` aliases fast transients; too small wastes time.

## Done when
The solver matches the model class (`dae` for physical, `ode45`/`ode4` for continuous, `discrete` for sampled), with `StopTime`/`SampleTime` set to resolve the dynamics.
