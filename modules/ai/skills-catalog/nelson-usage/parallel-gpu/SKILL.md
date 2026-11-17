---
name: nelson-usage-parallel-gpu
audience: user
modules: parallel, gpu_engine, mpi
description: Speed up Nelson work with background/parallel execution and the GPU. Use when the user wants to run tasks concurrently, keep the UI responsive during long work, or compute on a gpuArray.
triggers: parallele, parallel, concurrent, arriere plan, background, backgroundpool, parfeval, asynchrone, async, tache, task, gpu, gpuarray, carte graphique, accelerer, accelerate, speedup, gather
functions: parfeval, backgroundPool, afterEach, fetchOutputs, gpuArray, gather, gpuDevice, tic, toc
---

# Parallel and GPU Computing

Run work off the main thread or on the GPU, then collect the results.

## When to use
- Running independent tasks concurrently, or keeping the session responsive during long work.
- Moving array math onto the GPU.

## When NOT to use
- Small/fast work where scheduling overhead dominates (just run it inline).

## Key functions
Background/parallel: `backgroundPool`, `parfeval`, `fetchOutputs`, `afterEach`. GPU: `gpuArray`, `gather`, `gpuDevice`. Timing: `tic`, `toc`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Submit async work with `parfeval(backgroundPool, @fn, nOut, args...)`; it returns a future immediately so the session stays free.
2. Collect with `fetchOutputs(future)` when you need the result, or react to each completion with `afterEach`.
3. On the GPU: move data with `gpuArray`, run the math (it stays on the device), then `gather` the result back to the host.
4. Measure before and after with `tic`/`toc` - parallel/GPU only pays off above a problem-size threshold.
5. Keep tasks independent; shared mutable state across background tasks causes races.

## Gotchas
- `parfeval` overhead can exceed the work for tiny tasks - measure.
- Forgetting `gather` leaves the result on the GPU; a host operation then fails or is slow.
- Results from futures arrive out of order; use `fetchOutputs`/`afterEach`, not an assumed order.

## Done when
The work runs concurrently or on the GPU, results are collected (`fetchOutputs`/`gather`), and a `tic`/`toc` check shows the speedup is real.
