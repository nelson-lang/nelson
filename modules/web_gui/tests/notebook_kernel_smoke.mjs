//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Phase 3a smoke test: notebook kernel stdio JSON-RPC round-trip.
// Spawns NelSon-adv-cli --notebook-kernel and over its stdin/stdout frame channel
// proves: kernel.initialize -> notebook.execute(surf(peaks)) -> figure.list ->
// figure.getDisplayList, collecting notebook.output and figure.frame notifications.
// No deps (Node stdio + a small Content-Length codec).
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

// Resolve the adv-cli binary: NELSON_ADVCLI env override, else bin/x64 (or
// bin/<arch>) relative to the repo root (this file lives at modules/web_gui/tests).
function resolveExe() {
  if (process.env.NELSON_ADVCLI) return process.env.NELSON_ADVCLI;
  const here = dirname(fileURLToPath(import.meta.url));
  const repo = join(here, '..', '..', '..');
  const exe = process.platform === 'win32' ? 'NelSon-adv-cli.exe' : 'nelson-adv-cli';
  return join(repo, 'bin', 'x64', exe);
}
const EXE = resolveExe();
const log = (...a) => console.log('[smoke]', ...a);
let failed = false;
const fail = (m) => { failed = true; console.error('[smoke] FAIL:', m); };

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
  if (msg.method) { notifications.push(msg); log('notif:', msg.method, JSON.stringify(msg.params).slice(0, 120)); }
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

async function run() {
  const init = await send('kernel.initialize', { client: 'smoke', protocolVersion: 1 });
  log('initialize:', JSON.stringify(init));
  if (!init || init.protocolVersion !== 1) fail('initialize bad protocolVersion');
  if (!init.capabilities || init.capabilities.figures !== true) fail('initialize missing figures capability');

  const exec = await send('notebook.execute', { code: 'surf(peaks);' });
  log('execute:', JSON.stringify(exec));
  if (!exec || exec.status !== 'ok') fail('execute surf(peaks) not ok: ' + JSON.stringify(exec));

  const list = await send('figure.list', {});
  log('figure.list:', JSON.stringify(list));
  if (!list.figures || list.figures.length === 0) { fail('no figure after surf(peaks)'); return; }
  const handle = list.figures[0].handle;

  const dl = await send('figure.getDisplayList', { handle, softwareFallback: true });
  log('getDisplayList keys:', Object.keys(dl).join(','));
  log('  width=%s height=%s is3d=%s primitives=%s webglMeshes=%s fallback=%s',
    dl.width, dl.height, dl.is3d,
    Array.isArray(dl.primitives) ? dl.primitives.length : 'n/a',
    Array.isArray(dl.webglMeshes) ? dl.webglMeshes.length : 'n/a',
    typeof dl.softwareFallbackImage === 'string');
  if (!Array.isArray(dl.primitives)) fail('display list has no primitives array');
  if (dl.width === undefined) fail('display list missing width');

  await sleep(300);
  const frames = notifications.filter((n) => n.method === 'figure.frame');
  log('figure.frame notifications:', frames.length);

  // Workspace persists across executes.
  await send('notebook.execute', { code: 'kv = 42;' });
  notifications.length = 0;
  const persist = await send('notebook.execute', { code: 'disp(kv + 100)' });
  if (persist.status !== 'ok') fail('persistence cell not ok');
  const out = notifications.filter((n) => n.method === 'notebook.output').map((n) => n.params.text).join('');
  log('persisted disp output:', JSON.stringify(out));
  if (!out.includes('142')) fail('workspace did not persist (expected 142)');

  // Error cell: status error with a clean evalue (no "Error:" wrapper).
  const err = await send('notebook.execute', { code: "error('boom from cell')" });
  log('error cell result:', JSON.stringify(err));
  if (err.status !== 'error') fail('error cell not reported as error');
  if (err.evalue !== 'boom from cell') fail('evalue not clean: ' + JSON.stringify(err.evalue));

  log(failed ? 'RESULT: FAIL' : 'RESULT: PASS');
  await send('kernel.shutdown', {}).catch(() => {});
}

setTimeout(() => { fail('global timeout'); child.kill(); }, 90000);
run().catch((e) => { fail(e.message); child.stdin.end(); setTimeout(() => child.kill(), 1000); });
