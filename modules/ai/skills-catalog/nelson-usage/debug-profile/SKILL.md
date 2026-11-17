---
name: nelson-usage-debug-profile
audience: user
modules: debugger, profiler
description: Debug and profile Nelson code - breakpoints and stepping, error handling, and finding slow spots. Use when the user hits an error, wants to step through a script, or needs to speed code up.
triggers: debug, deboguer, debogage, point d arret, breakpoint, dbstop, pas a pas, step, erreur, error, exception, try catch, attraper, catch, pile, stack, dbstack, profiler, profile, performance, lent, slow, optimiser le temps, avertissement, warning
functions: dbstop, dbstep, dbcont, dbquit, dbstack, dbclear, error, warning, assert, rethrow, lasterror, MException, profile
---

# Debugging and Profiling

Find why code fails or where it is slow, with breakpoints, error handling, and the profiler.

## When to use
- An error or wrong result to investigate.
- Stepping through a script, inspecting state.
- Code that is too slow and needs a hotspot found.

## When NOT to use
- Writing new feature code (use the relevant usage guide).

## Key functions
Breakpoints/stepping: `dbstop`, `dbstep`, `dbcont`, `dbquit`, `dbclear`, with the call stack from `dbstack`. Errors: `error`, `warning`, `assert`, `rethrow`, `lasterror`, `MException`. Timing: `profile`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Reproduce first: a minimal snippet that triggers the failure, then set `dbstop` where it goes wrong (or `dbstop if error`).
2. Step with `dbstep`, continue with `dbcont`, inspect variables at each stop; `dbstack` shows how you got there.
3. Guard assumptions with `assert(cond, msg)`; raise clear failures with `error(id, msg)` and handle with `try`/`catch` + `rethrow`.
4. Profile hotspots: `profile('on')`, run the workload, `profile('off')`, then read where time went - optimize the top entry only.
5. Prefer vectorized operations over element loops once the hotspot is known.

## Driving the live GUI (assistant)
When running inside the Nelson assistant, drive the profiler/debugger GUIs directly:
- `profile_current_file` to profile a saved file and surface its slow functions (the Profiler panel shows the full report on Qt and web).
- `debug_nelson_file` with a line to set a breakpoint and open the file there, so the debugger (Call Stack, variables) engages on the next run; `clear_breakpoints` to reset.
The file must be saved first; interactive stepping stays the user's in the GUI.

## Gotchas
- Clear breakpoints with `dbclear` before a clean run, or stepping fires unexpectedly.
- Catch specific errors; a bare `try`/`catch` that swallows everything hides real bugs.
- Profile a representative workload - a tiny input profiles the wrong thing.

## Done when
The failure's root cause is found (or the slow hotspot identified and improved), with the fix confirmed by re-running and the measurement repeated.
