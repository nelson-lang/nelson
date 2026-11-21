// Phase 3a NFlow proof: drive nflow.* over the notebook kernel stdio channel and
// prove workspace interop. One process runs a notebook cell that sets kv_in, then
// an async NFlow simulation whose From Workspace block reads kv_in; a gain of 2
// makes 42, To Workspace writes kv_out, and a final cell reads kv_out back.
// No deps (Node stdio + a small Content-Length codec), same shape as
// notebook_kernel_smoke.mjs.
//
// The kernel reports capabilities.nflow only when the nflow host surface is built
// into libnlsNflow_gui_builtin. Until that surface ships this test skips cleanly
// (exit 0) instead of failing, so it is runnable on the current binary.
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { readFileSync } from 'node:fs';

function resolveExe() {
  if (process.env.NELSON_ADVCLI) return process.env.NELSON_ADVCLI;
  const here = dirname(fileURLToPath(import.meta.url));
  const repo = join(here, '..', '..', '..');
  const exe = process.platform === 'win32' ? 'NelSon-adv-cli.exe' : 'nelson-adv-cli';
  return join(repo, 'bin', 'x64', exe);
}
const HERE = dirname(fileURLToPath(import.meta.url));
const EXE = resolveExe();
const MODEL = readFileSync(join(HERE, 'nflow_kernel_interop.nflow'), 'utf8');
const log = (...a) => console.log('[nflow-smoke]', ...a);
let failed = false;
const fail = (m) => { failed = true; console.error('[nflow-smoke] FAIL:', m); };

const child = spawn(EXE, ['--notebook-kernel'], { stdio: ['pipe', 'pipe', 'pipe'] });

// ---- frame codec ----
let buf = Buffer.alloc(0);
const pending = new Map();
const notifications = [];
let nextId = 1;

function send(method, params) {
  const id = nextId++;
  const body = Buffer.from(JSON.stringify({ jsonrpc: '2.0', id, method, params }), 'utf8');
  child.stdin.write(`Content-Length: ${body.length}\r\n\r\n`);
  child.stdin.write(body);
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    setTimeout(() => {
      if (pending.has(id)) { pending.delete(id); reject(new Error(`timeout ${method}`)); }
    }, 20000);
  });
}

function onMessage(msg) {
  if (msg.id !== undefined && msg.id !== null && (msg.result !== undefined || msg.error !== undefined)) {
    const p = pending.get(msg.id);
    if (p) {
      pending.delete(msg.id);
      if (msg.error) p.reject(new Error(`${msg.error.code}: ${msg.error.message}`));
      else p.resolve(msg.result);
    }
    return;
  }
  if (msg.method) { notifications.push(msg); }
}

child.stdout.on('data', (d) => {
  buf = Buffer.concat([buf, d]);
  for (;;) {
    const sep = buf.indexOf('\r\n\r\n');
    if (sep < 0) break;
    const header = buf.slice(0, sep).toString('ascii');
    const m = header.match(/Content-Length:\s*(\d+)/i);
    if (!m) { buf = buf.slice(sep + 4); continue; }
    const len = parseInt(m[1], 10);
    if (buf.length < sep + 4 + len) break;
    const body = buf.slice(sep + 4, sep + 4 + len).toString('utf8');
    buf = buf.slice(sep + 4 + len);
    try { onMessage(JSON.parse(body)); } catch (e) { fail('bad frame json: ' + e.message); }
  }
});
child.stderr.on('data', (d) => process.stderr.write('[engine-err] ' + d));
child.on('exit', (code) => { log('engine exited code', code); process.exit(failed ? 1 : 0); });

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const cellOutput = (from) =>
  notifications.filter((n, i) => i >= from && n.method === 'notebook.output')
    .map((n) => n.params.text).join('');
// nflow dispatch results come through as JSON strings (the nflow stringResult
// convention, which also carries the blob mechanism); parse one level when needed.
const asObj = (x) => {
  if (typeof x !== 'string') return x;
  try { return JSON.parse(x); } catch { return x; }
};

async function run() {
  const init = await send('kernel.initialize', { client: 'nflow-smoke', protocolVersion: 1 });
  log('initialize:', JSON.stringify(init));
  if (!init || !init.capabilities) { fail('initialize returned no capabilities'); return; }
  if (init.capabilities.nflow !== true) {
    log('nflow capability absent: the nflow host surface is not built into this binary yet.');
    log('SKIP: rerun after nflow-m1 ships nlsNFlowKernel* in libnlsNflow_gui_builtin.');
    await send('kernel.shutdown', {}).catch(() => {});
    return;
  }
  if (!init.nflowDistDir) {
    log('WARN: nflowDistDir empty (nflow host DistDir returned empty headless); report to nflow-m1.');
  } else {
    log('nflowDistDir:', init.nflowDistDir);
  }

  // A notebook cell sets the source signal: constant 21 over [0, 1].
  const setIn = await send('notebook.execute', { code: 'kv_in = [(0:0.1:1)\', 21*ones(11,1)];' });
  if (!setIn || setIn.status !== 'ok') fail('setting kv_in failed: ' + JSON.stringify(setIn));

  // Start the model. From Workspace reads kv_in set above, proving interop in.
  const started = asObj(await send('nflow.sim.run', { diagramJson: MODEL, async: true }));
  log('sim.run:', JSON.stringify(started));
  const runId = started && (started.runId || (asObj(started.result) || {}).runId);
  if (!runId) { fail('sim.run returned no runId: ' + JSON.stringify(started)); return; }

  // Poll until complete, up to ~15 s.
  let result = null;
  for (let i = 0; i < 60; i++) {
    const poll = asObj(await send('nflow.sim.poll', { runId }));
    if (poll && poll.status === 'complete') { result = asObj(poll.result) || poll; break; }
    if (poll && poll.status === 'error') { fail('sim poll error: ' + JSON.stringify(poll)); break; }
    await sleep(250);
  }
  if (!result) { fail('simulation did not complete'); }
  else {
    const scopes = result.scopes || (asObj(result.result) || {}).scopes;
    if (!Array.isArray(scopes) || scopes.length === 0) log('WARN: no scopes array in result');
    else log('scopes:', scopes.length);
  }

  // Read kv_out back: a gain of 2 on 21 is 42. Proves interop out, tolerant of the
  // To Workspace save format (struct with time, or a plain array).
  const base = notifications.length;
  const readOut = await send('notebook.execute',
    { code: 'v = kv_out; if isstruct(v), v = v.signals.values; end; disp(max(v(:)))' });
  if (!readOut || readOut.status !== 'ok') fail('reading kv_out failed: ' + JSON.stringify(readOut));
  await sleep(100);
  const out = cellOutput(base).trim();
  log('kv_out max:', JSON.stringify(out));
  if (!/42/.test(out)) fail('workspace interop broken (expected 42 from gain*kv_in)');

  await send('nflow.sim.stop', { runId }).catch(() => {});
  log(failed ? 'RESULT: FAIL' : 'RESULT: PASS');
  await send('kernel.shutdown', {}).catch(() => {});
}

setTimeout(() => { fail('global timeout'); child.kill(); }, 90000);
run().catch((e) => { fail(e.message); child.stdin.end(); setTimeout(() => child.kill(), 1000); });
