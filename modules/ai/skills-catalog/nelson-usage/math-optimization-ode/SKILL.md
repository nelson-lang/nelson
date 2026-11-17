---
name: nelson-usage-math-optimization-ode
audience: user
modules: optimization, ode_solvers, polynomial_functions, special_functions
description: Solve equations, optimize, integrate ODEs, fit and interpolate in Nelson. Use when the user minimizes a function, finds a root, integrates a differential equation, fits a polynomial, or interpolates.
triggers: optimiser, optimize, minimiser, minimize, fminsearch, fminbnd, racine, root, zero, fzero, fsolve, equation, ode, differentielle, differential, ode45, integrer, integrate, integral, trapz, ajuster, fit, polyfit, interpolation, interp1, spline, gradient, derivee
functions: fminsearch, fminbnd, fzero, fsolve, lsqnonlin, optimset, ode45, ode23, ode113, ode15s, odeset, deval, polyfit, polyval, roots, interp1, interp2, spline, pchip, trapz, cumtrapz, integral, gradient, diff
---

# Optimization, ODEs, Fitting and Interpolation

Minimize, find roots, integrate differential equations, and fit or interpolate data.

## When to use
- Minimizing a cost or finding a root of a function.
- Integrating an ODE in time.
- Fitting a polynomial / interpolating between samples; numeric integration or differentiation.

## When NOT to use
- Plain summary statistics (use the analysis guide).
- Linear solves/decompositions (use the linear-algebra guide).

## Key functions
Optimize/roots: `fminsearch`, `fminbnd`, `fzero`, `fsolve`, `lsqnonlin`, with `optimset`. ODEs: `ode45` (default), `ode23`, `ode113`, `ode15s` (stiff), configured by `odeset`, sampled by `deval`. Fit/interp: `polyfit`, `polyval`, `roots`, `interp1`, `interp2`, `spline`, `pchip`. Calculus: `trapz`, `cumtrapz`, `integral`, `gradient`, `diff`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Pass the model as a function handle: `fzero(@(x) f(x), x0)`, `ode45(@(t, y) rhs(t, y), tspan, y0)`.
2. Choose the ODE solver by stiffness: `ode45` for non-stiff, `ode15s` for stiff; tighten tolerances through `odeset` only when needed.
3. Give optimizers a sensible start `x0`; a bad start finds a local minimum or fails to converge.
4. Fit with the lowest polynomial degree that captures the trend (`polyfit`) to avoid overfitting; evaluate with `polyval`.
5. For integration prefer `integral` (adaptive) over a fixed-step `trapz` when accuracy matters.

## Gotchas
- `fminsearch` finds a LOCAL minimum; try several starts for a global one.
- A high-degree `polyfit` oscillates between points; interpolate with `spline`/`pchip` instead.
- An ODE right-hand side must return a column vector matching `y0`.

## Done when
The minimum/root/solution is found and sanity-checked (residual small, converged), or the fit/interpolation reproduces the data within tolerance.
