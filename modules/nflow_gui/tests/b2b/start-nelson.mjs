#!/usr/bin/env node
// ============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================
// Launches a real Nelson instance hosting NFlow for back-to-back tests.
// NFLOW_GUI_DEBUGGING_PORT pins the WebUI port, enables multi-client so
// Playwright can connect alongside the locally opened window, and puts the
// WebUI server in serve-forever mode (the port stays up even when every
// client disconnects between tests or the auto-opened window is closed).
//
// Robustness (the process-liveness watch alone is not enough):
//  - stale-host reaping: a Nelson process left over from a previous session
//    can still own the port and serve stale state; it is killed before
//    launching (run `node start-nelson.mjs --reap-only` to only do that).
//  - health supervision: the WebUI server can die while Nelson itself keeps
//    running (historically: window closed / client-gap timeout), and after a
//    relaunch Nelson can land on the wrong port if the pinned one is still
//    lingering. The supervisor probes the HTTP endpoint and recycles the
//    child whenever the port stops answering.
// ============================================================================
import { spawn, execFileSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(scriptDir, '..', '..', '..', '..');

const port = Number(process.env.NFLOW_B2B_PORT || '17973');
const healthUrl = `http://localhost:${port}/nflow.html`;
const reapOnly = process.argv.includes('--reap-only');

// ---------------------------------------------------------------------------
// Stale-host reaping
// ---------------------------------------------------------------------------
function listeningPids() {
  const pids = new Set();
  try {
    if (process.platform === 'win32') {
      const out = execFileSync('netstat', ['-ano', '-p', 'TCP'], { encoding: 'utf8' });
      for (const line of out.split(/\r?\n/)) {
        const m = line.match(/^\s*TCP\s+\S+:(\d+)\s+\S+\s+LISTENING\s+(\d+)\s*$/);
        if (m && Number(m[1]) === port) {
          pids.add(Number(m[2]));
        }
      }
    } else {
      const out = execFileSync('lsof', ['-nP', `-iTCP:${port}`, '-sTCP:LISTEN', '-t'], {
        encoding: 'utf8',
      });
      for (const token of out.split(/\s+/)) {
        if (/^\d+$/.test(token)) {
          pids.add(Number(token));
        }
      }
    }
  } catch {
    // netstat/lsof unavailable or no listener: treat as free.
  }
  return [...pids];
}

function processImage(pid) {
  try {
    if (process.platform === 'win32') {
      const out = execFileSync('tasklist', ['/FI', `PID eq ${pid}`, '/FO', 'CSV', '/NH'], {
        encoding: 'utf8',
      });
      const m = out.match(/^"([^"]+)"/m);
      return m ? m[1] : '';
    }
    return execFileSync('ps', ['-p', String(pid), '-o', 'comm='], { encoding: 'utf8' }).trim();
  } catch {
    return '';
  }
}

async function reapStaleHost() {
  const pids = listeningPids();
  for (const pid of pids) {
    const image = processImage(pid);
    if (!/nelson/i.test(image)) {
      console.error(
        `start-nelson: port ${port} is held by pid ${pid} (${image || 'unknown'}), ` +
          'which is not a Nelson process. Free the port or set NFLOW_B2B_PORT.'
      );
      process.exit(1);
    }
    console.log(`start-nelson: killing stale Nelson host pid ${pid} (${image}) on port ${port}`);
    try {
      process.kill(pid, 'SIGKILL');
    } catch {
      // already gone
    }
  }
  if (pids.length > 0) {
    // Wait for the OS to actually release the listener before binding again.
    const deadline = Date.now() + 5000;
    while (Date.now() < deadline && listeningPids().length > 0) {
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
  }
}

// ---------------------------------------------------------------------------
// Nelson CLI discovery
// ---------------------------------------------------------------------------
const platformCandidates = {
  darwin: [
    path.join(repoRoot, 'bin', 'macOS', 'nelson-cli'),
    path.join(repoRoot, 'bin', 'macOS', 'nelson-cli-exec'),
  ],
  linux: [
    path.join(repoRoot, 'bin', 'linux', 'nelson-cli'),
    path.join(repoRoot, 'bin', 'linux', 'nelson-cli-exec'),
  ],
  win32: [
    path.join(repoRoot, 'bin', 'x64', 'Nelson-cli.exe'),
    path.join(repoRoot, 'bin', 'ARM64', 'Nelson-cli.exe'),
  ],
};

const fallbackCandidates = Object.entries(platformCandidates)
  .filter(([platform]) => platform !== process.platform)
  .flatMap(([, paths]) => paths);

const candidates = [
  process.env.NELSON_CLI_PATH,
  ...(platformCandidates[process.platform] || []),
  ...fallbackCandidates,
].filter(Boolean);

// ---------------------------------------------------------------------------
// Launch + supervision
// ---------------------------------------------------------------------------
const MAX_RESTARTS = 50;
const HEALTH_INTERVAL_MS = 3000;
const HEALTH_TIMEOUT_MS = 2500;
const HEALTH_MAX_FAILURES = 3; // consecutive probe failures before recycling
const BOOT_GRACE_MS = 60_000; // allow nflow() this long to bind after launch

let stopping = false;
let child = null;
let restarts = 0;
let launchedAt = 0;
let sawHealthy = false;
let consecutiveFailures = 0;

function launch(nelsonCli) {
  launchedAt = Date.now();
  sawHealthy = false;
  consecutiveFailures = 0;
  console.log(`start-nelson: launching ${nelsonCli} (NFlow on port ${port})`);
  // The host must never time out on its own — Playwright (or the supervisor)
  // owns its lifetime. A finite sleep turned long suites and reused hosts into
  // scheduled mid-run deaths. And it must idle in a Nelson-level LOOP, not one
  // long sleep(inf): cross-thread interpreter work (MainThreadTaskQueue —
  // dashboard callback buttons, workspace blocks, SSP/FMU export) is only
  // drained at interpreter statement boundaries, so inside a single sleep()
  // call every such RPC stalls for the full 30 s bridge timeout. The loop
  // reaches a statement boundary every ~100 ms.
  child = spawn(nelsonCli, ['-e', 'nflow(); while true; sleep(0.1); end'], {
    env: { ...process.env, NFLOW_GUI_DEBUGGING_PORT: String(port) },
    stdio: 'inherit',
  });
  child.on('exit', (code) => {
    if (stopping) {
      process.exit(code ?? 0);
    }
    if (restarts >= MAX_RESTARTS) {
      console.error('start-nelson: too many restarts, giving up.');
      process.exit(code ?? 1);
    }
    restarts += 1;
    console.log(`start-nelson: Nelson exited (code ${code}); restarting (${restarts})...`);
    setTimeout(() => launch(nelsonCli), 1000);
  });
}

async function probeHealth() {
  try {
    const response = await fetch(healthUrl, { signal: AbortSignal.timeout(HEALTH_TIMEOUT_MS) });
    return response.ok;
  } catch {
    return false;
  }
}

function superviseHealth() {
  setInterval(async () => {
    if (stopping || !child || child.exitCode !== null) {
      return;
    }
    const healthy = await probeHealth();
    if (healthy) {
      sawHealthy = true;
      consecutiveFailures = 0;
      return;
    }
    // Nelson boots + nflow() binds within the grace window; don't count
    // probe failures until it was healthy once (or the grace expired, which
    // catches a host stranded on the wrong port after a bind failure).
    if (!sawHealthy && Date.now() - launchedAt < BOOT_GRACE_MS) {
      return;
    }
    consecutiveFailures += 1;
    if (consecutiveFailures >= HEALTH_MAX_FAILURES) {
      console.error(
        `start-nelson: ${healthUrl} stopped answering (${consecutiveFailures} probes); recycling Nelson...`
      );
      consecutiveFailures = 0;
      try {
        child.kill();
      } catch {
        // exit handler relaunches
      }
    }
  }, HEALTH_INTERVAL_MS).unref();
}

const stop = () => {
  stopping = true;
  try {
    child?.kill();
  } catch {
    // ignore
  }
};

async function main() {
  if (process.env.NFLOW_B2B_REUSE) {
    // The user explicitly wants to run against their own host: leave it alone.
    if (reapOnly) {
      return;
    }
  } else {
    await reapStaleHost();
  }
  if (reapOnly) {
    return;
  }
  const nelsonCli = candidates.find((p) => fs.existsSync(p));
  if (!nelsonCli) {
    console.error(
      'start-nelson: Nelson CLI not found. Build Nelson first or set NELSON_CLI_PATH.'
    );
    process.exit(1);
  }
  process.on('SIGTERM', stop);
  process.on('SIGINT', stop);
  process.on('exit', stop);
  launch(nelsonCli);
  superviseHealth();
}

main();
