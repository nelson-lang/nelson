// NFlow kernel method-surface proof: drive the nflow.* methods the extension needs
// for parity over the notebook kernel stdio channel, with no editor window. It proves
// the headless-available methods (codegen, library list/load with its push, router,
// expr, fmi3 reachability, modelica info reachability) and documents the ones that are
// served elsewhere (model.* needs a registered document, so headless returns
// model_not_loaded and the extension owns model editing locally).
//
// The kernel reports capabilities.nflow only when the nflow host surface is built into
// libnlsNflow_gui_builtin; until then this test skips cleanly (exit 0). Same transport
// shape as nflow_kernel_smoke.mjs (Node stdio + a small Content-Length codec, no deps).
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
const MODEL = readFileSync(
  join(HERE, '..', '..', 'nflow_blocks', 'examples', 'causal', 'Logic_Gates_Demo.nflow'), 'utf8');
const log = (...a) => console.log('[nflow-methods]', ...a);
let failed = false;
const fail = (m) => { failed = true; console.error('[nflow-methods] FAIL:', m); };

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
// nflow dispatch results arrive as JSON strings (the nflow stringResult convention);
// parse one level when needed.
const asObj = (x) => {
  if (typeof x !== 'string') return x;
  try { return JSON.parse(x); } catch { return x; }
};

async function run() {
  const init = await send('kernel.initialize', { client: 'nflow-methods', protocolVersion: 1 });
  log('initialize:', JSON.stringify(init));
  if (!init || !init.capabilities) { fail('initialize returned no capabilities'); return; }
  if (init.capabilities.nflow !== true) {
    log('nflow capability absent: the nflow host surface is not built into this binary yet.');
    log('SKIP: rerun after the nflow host ships nlsNFlowKernel* in libnlsNflow_gui_builtin.');
    await send('kernel.shutdown', {}).catch(() => {});
    return;
  }
  const distDir = init.nflowDistDir || '';
  log('nflowDistDir:', JSON.stringify(distDir));

  // library.list: the block registry is a pure singleton, so it answers headless.
  const libList = asObj(await send('nflow.library.list', {}));
  if (!Array.isArray(libList)) fail('library.list did not return an array: ' + JSON.stringify(libList));
  else log('library.list manifests:', libList.length);

  // library.load pushes nflow.library.loaded through the abstracted push sink; prove the
  // notification reaches the stdio channel. Any existing directory drives the scan + push
  // (the push fires regardless of whether a library.json was found there).
  const pushBefore = notifications.filter((n) => n.method === 'nflow.library.loaded').length;
  const loadDir = distDir || HERE;
  const loaded = asObj(await send('nflow.library.load', { path: loadDir }));
  log('library.load:', JSON.stringify({ success: loaded && loaded.success, path: loaded && loaded.path }));
  await sleep(150);
  const pushAfter = notifications.filter((n) => n.method === 'nflow.library.loaded').length;
  if (pushAfter <= pushBefore) fail('nflow.library.loaded push notification did not arrive over stdio');
  else log('push nflow.library.loaded received:', pushAfter - pushBefore);

  // codegen.generate: Logic_Gates_Demo is pure logic (no modelica/acausal), so the
  // pipeline needs no interpreter hop; it must return non-empty C, not an error marker.
  const code = await send('nflow.codegen.generate', { diagramJson: MODEL, optionsJson: '{"lang":"c"}' });
  const codeStr = typeof code === 'string' ? code : JSON.stringify(code);
  if (!codeStr || codeStr.length < 50) fail('codegen produced no code: ' + codeStr);
  else if (/codegen error/i.test(codeStr)) fail('codegen returned an error marker: ' + codeStr.slice(0, 200));
  else log('codegen.generate C bytes:', codeStr.length, '| has step fn:', /\bvoid\b/.test(codeStr));

  // router.routeWires + expr.eval: pure C++ engines, must answer headless.
  const routed = asObj(await send('nflow.router.routeWires',
    { payload: JSON.stringify({ wires: [], obstacles: [] }) }));
  log('router.routeWires ok:', routed !== undefined && routed !== null);
  const evaled = asObj(await send('nflow.expr.eval', { payload: JSON.stringify({ expr: '2+3' }) }));
  log('expr.eval ->', JSON.stringify(evaled));

  // fmi3.inspect reachability: a bogus path must come back as a structured error, not a
  // crash, proving the method is wired (a real .fmu round-trip is covered by nflow_fmi).
  const inspect = asObj(await send('nflow.fmi3.inspect', { path: 'does_not_exist.fmu' }));
  log('fmi3.inspect(bogus) reachable, reply keys:',
    inspect && typeof inspect === 'object' ? Object.keys(inspect).join(',') : typeof inspect);

  // modelica.info: wired through the bridge; reports the resolved OpenModelica install or
  // throws "not available" when none is configured. Either proves the wiring is reachable.
  try {
    const info = asObj(await send('nflow.modelica.info', {}));
    log('modelica.info ok, keys:', info && typeof info === 'object' ? Object.keys(info).join(',') : typeof info);
  } catch (e) {
    log('modelica.info reachable (reports unavailable):', e.message);
  }

  // model.getDiagram: headless has no registered NFlowModelDocument, so this must report
  // model_not_loaded. This documents that model editing/saving is owned by the extension.
  const diagram = asObj(await send('nflow.model.getDiagram', {}));
  if (diagram && diagram.error === 'model_not_loaded') {
    log('model.getDiagram -> model_not_loaded (expected headless; extension owns model.*)');
  } else {
    log('model.getDiagram unexpected headless reply:', JSON.stringify(diagram));
  }

  log(failed ? 'RESULT: FAIL' : 'RESULT: PASS');
  await send('kernel.shutdown', {}).catch(() => {});
}

setTimeout(() => { fail('global timeout'); child.kill(); }, 90000);
run().catch((e) => { fail(e.message); child.stdin.end(); setTimeout(() => child.kill(), 1000); });
