# Native Application Performance Tests

Run `bench_nelsonc.ps1` with PowerShell 7 on Windows. `Runtime` is the binary
directory of a built Nelson distribution, containing `nelsonc.exe`,
`nelson-app.exe` and `NelSon-cli.exe`. The distribution must also contain its
module data, startup files and redistribution notices.

```powershell
./modules/main/tests/bench_nelsonc.ps1 -Runtime D:/build/after/bin/x64 -BeforeRuntime D:/build/before/bin/x64 -Rounds 9 -Samples 3 -Affinity 1
```

The script creates an independent directory under the repository's `temp/`.
It does not replace the supplied runtimes. Keep `report.json`,
`measurements.jsonl`, the process logs and the generated executable together.

## Comparisons

- `source`: ordinary source execution using the current runtime.
- `packaged`: an executable built by that runtime from the same source files.
- `before`: ordinary source execution using the optional reference runtime.
- `installed`: optional (`-IncludeInstalled`) executable using the current
  full installation, without a redistributed runtime. The report pairs it
  with `packaged`, using identical numerical libraries and workload sources.

Omitting `BeforeRuntime` measures packaging overhead only. It cannot establish
that ordinary Nelson execution is unchanged by the implementation.

`-IncludeInstalled` measures installed versus bundled deployment in the same
rotating rounds. It records both executable hashes and the selected installation
root. Startup includes installed-runtime discovery, fingerprinting and the full
installation's module startup; warmed samples exclude all of that startup work.
This comparison is not a before/after implementation regression test.

Use `-InstalledRuntimeRoot` with `-IncludeInstalled` to select a compatible
external installation explicitly. For a same-runtime control, point it to a
previous benchmark's `distribution/performance.runtime`. If the inventories
match, the harness verifies every installed file hash and the exact file list,
then records `InstalledRuntimeMatchesBundled=true`. This isolates external
runtime selection from differences in the startup modules of a full installation.

Use the same compiler, toolset, architecture, configuration, optimization
flags, numerical backend and module-data layout for before/after builds.
Build all changed native libraries in both runtimes; do not compare a debug
binary, an older unrelated installed distribution or different startup data.
The report records source and DLL hashes and includes `build-provenance.json`
when present at `Runtime/../../../build-provenance.json` (the common parent
of paired `before/bin/x64` and `after/bin/x64` trees). Recorded provenance is evidence
to inspect, not an automatic proof that the builds are comparable.

Before/after DLL inventories are checked in both directions. Unchanged libraries,
including numerical backends, must have identical SHA-256 digests. Differences
are permitted only for the nine implementation targets: interpreter, core,
interpreter gateway, dynamic-link library/gateway, archive library/gateway,
Web GUI and engine. Each differing DLL requires one matching output record in
both builds, the same build-tool version, and Release/x64 arguments. The record
contains Variant (before/after), Project, Output (DLL filename), SHA256 and
Arguments. Rebuild older pairs whose provenance does not include output hashes.
The two build-only compiler libraries may be added to the after runtime with
their matching output proof; other additions or removals are rejected.

VerifiedRuntimeDifferences records these checked changes. This validates binary
identity against recorded outputs, not the correctness of the build commands or
equivalence of every source input. Inspect those commands and source snapshots
as well. A same-runtime control with identical DLLs needs no differing-output
proof. Unit coverage is test_performance_runtime_provenance in the compiler
module and test_nelsonc_performance_provenance.ps1 beside this harness.
All measured runtimes, including the bundled runtime, are hashed again after
the measurements. A DLL addition, removal or digest change aborts the report;
the existing process logs and measurements remain available for diagnosis.

## Measurement Protocol

Each workload runs in a fresh process, performs a full warmup and then measures
`Samples` repetitions. Variant order rotates between rounds. Workloads cover
scalar arithmetic, cross-file calls, builtin calls and dense matrix products.
The numerical results must agree across all variants.

The default affinity mask pins the child to one logical processor. A value
of `4` selects bit 2; `0` disables affinity pinning. Computational threads and
the supported numerical-backend environment settings default to one. These
default results do not validate multithreaded scaling.

`-Threads` explicitly sets the computation-thread count for every variant and
the OMP, MKL and OpenBLAS environment settings. Dynamic thread adjustment is
disabled for OMP and MKL. A nonzero affinity mask must allow at least that many
logical processors; invalid combinations fail before creating benchmark files.
The fixture restores its previous thread setting on return or error. The wider
1024-by-1024 matrix workload is available separately as `matrixwide`:

```powershell
./modules/main/tests/bench_nelsonc.ps1 -Runtime D:/build/after/bin/x64 -BeforeRuntime D:/build/before/bin/x64 -Kinds matrixwide -Threads 4 -Affinity 0 -Rounds 9 -Samples 3
```

Compare variants at the same thread count; a one-thread comparison is not a
multithreaded baseline. `Parallelism` in the report gives total warmed process
CPU time divided by total warmed wall time for each workload and variant.
Values above one provide evidence of concurrent CPU use, whereas the configured
thread count alone does not prove that the backend used all those threads.
CPU timing includes every process thread; this diagnostic is not a hardware
utilization counter or a speedup claim. Small matrix workloads may use fewer
threads, so inspect this diagnostic before claiming multithreaded coverage.

Wall time and process CPU time are reported separately. Repetitions within
a process are summarized by their median; each round supplies one paired
ratio. The report includes the geometric mean ratio and a percentile 95%
interval from 10,000 paired bootstrap resamples. A ratio above one means the
numerator is slower. No measurements are discarded as outliers.
With one exploratory round, the bootstrap bounds collapse to the only ratio
and provide no useful uncertainty estimate. Such a run validates the harness,
not performance acceptance.

The startup case measures process creation through normal exit, separately
from warmed computation. Round zero includes first gateway-cache creation.
Subsequent rounds are new processes with warmed filesystem and gateway caches.
Neither case is an operating-system cold-disk benchmark; no caches are purged.
Preference files and temporary files are isolated under the test directory.
Startup diagnostics are checked in addition to exit status.

## Controls And Interpretation

Repeat on another affinity mask to expose processor-specific interference.
An identical-runtime control estimates the protocol's repeatability:

```powershell
./modules/main/tests/bench_nelsonc.ps1 -Runtime D:/build/after/bin/x64 -BeforeRuntime D:/build/after/bin/x64 -Kinds scalar -Rounds 9 -Samples 3 -Affinity 4
```

The report marks this as `IdenticalRuntimeControl`. Do not combine its ratios
with true before/after results. A control centered on one does not disprove a
regression observed in another comparison.

`-IncludeControl` adds a second execution of the current source runtime, named
`control`, to the same rotating rounds as `source`, `before` and the deployed
variants. It uses the same executable, libraries, workload and arguments, with
an independent preference directory. The report marks `InterleavedSourceControl`
and includes raw `control/source` ratios for startup, wall time and CPU time.
This avoids comparing repeatability only in a separate earlier time period.
The control never rescales or subtracts from the real before/after ratios.
With both `-IncludeInstalled` and `-IncludeControl`, ten rounds give each of the
five variants two turns in every execution position.

For targeted Windows relink experiments, keep the embedded PDB path constant
with `/PDBALTPATH`, while directing the actual `/PDB` output to the isolated
build directory. A changed embedded path length can shift data and resolved
code references even when the compiled object's instructions and relocations
are identical. Verify the relinked control's code section before attributing
timing differences to the selected source change. This does not imply that the
complete DLLs, their debug information or their loaded addresses are identical.
See the [linker option documentation](https://learn.microsoft.com/en-us/cpp/build/reference/pdbaltpath-use-alternate-pdb-path).

Confidence intervals, machine load, build provenance, effect size and repeated
runs all matter. An interval containing one is not proof of equivalence, and
a faster startup does not compensate for slower warmed execution. There is
no built-in arbitrary percentage that turns this diagnostic into a pass.
The user's nonregression requirement remains a separate acceptance gate.
