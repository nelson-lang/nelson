---
name: nelson-usage-control-systems
audience: user
modules: control_system
description: Build and analyze linear control models in Nelson - transfer functions and state space, time and frequency response, feedback, and discretization. Use when the user models a plant, plots a response, or designs a controller.
triggers: controle, control, automatique, systeme, system, fonction de transfert, transfer function, tf, etat, state space, ss, zpk, reponse, response, bode, step, indicielle, impulse, nyquist, marge, margin, feedback, retour, boucle, loop, discretiser, c2d, lqr, pole, pid
functions: tf, ss, zpk, bode, step, impulse, nyquist, margin, pole, zero, feedback, series, parallel, c2d, d2c, lsim, pzmap, rlocus, dcgain, bandwidth, lqr, place, ctrb, obsv, damp, pid
---

# Control Systems

Model a linear system, inspect its response, close a loop, and move between continuous and discrete time.

## When to use
- Building a plant as a transfer function or state space.
- Time response (`step`, `impulse`, `lsim`) or frequency response (`bode`, `nyquist`, `margin`).
- Feedback interconnection, discretization, or a basic controller design.

## When NOT to use
- General ODE integration of a nonlinear model (use the optimization/ODE guide).
- Block-diagram simulation on a canvas (use the NFlow guides).

## Key functions
Models: `tf`, `ss`, `zpk`. Time response: `step`, `impulse`, `lsim`. Frequency: `bode`, `nyquist`, `margin`, `bandwidth`, `dcgain`. Structure: `pole`, `zero`, `pzmap`, `rlocus`, `damp`, `ctrb`, `obsv`. Interconnect: `feedback`, `series`, `parallel`. Sample: `c2d`, `d2c`. Design: `lqr`, `place`, `pid`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Build the model first: `sys = tf(num, den)` or `sys = ss(A, B, C, D)`; convert with `ss`/`tf`/`zpk` as needed.
2. Inspect before designing: `step(sys)`, `bode(sys)`, and `margin(sys)` for gain/phase margins.
3. Close a loop with `feedback(plant, controller)`; check the closed-loop `pole` locations for stability.
4. Discretize with `c2d(sys, Ts)` choosing a sample time well below the fastest dynamics.
5. For state-feedback, verify controllability (`ctrb`) before `place`/`lqr`.

## Gotchas
- Mixing continuous and discrete models in one interconnection is an error; discretize consistently.
- A right-half-plane closed-loop `pole` means instability even if the open loop looked fine.
- Model units and sample time must be consistent across `c2d`/`d2c`.

## Done when
The model is built, its response inspected, and (if a controller was asked for) the closed loop is stable with the requested margins.
