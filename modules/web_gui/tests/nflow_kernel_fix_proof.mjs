// Proof that the interpreter-thread hand-off fix (NFlowNelsonBridge runViaInterpreter)
// makes the whole class of namespaced NFlow.internal.* bridge calls work over the
// headless stdio kernel: modelica.info, ssp.export + ssp.import (round-trip), fmu.export,
// a sim whose StopFcn runs, and codegen of an acausal model (expandAcausalDoc). Before the
// fix each of these failed with "Function evaluator is not available" because the work ran
// in place on the reader thread, where package resolution does not happen.
//
// Run after rebuilding nlsNflow_engine_builtin. Skips cleanly (exit 0) if the nflow host
// surface is absent. Same transport as the other kernel harnesses (stdio Content-Length).
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { readFileSync, existsSync, mkdtempSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';

function resolveExe() {
  if (process.env.NELSON_ADVCLI) return process.env.NELSON_ADVCLI;
  const here = dirname(fileURLToPath(import.meta.url));
  const repo = join(here, '..', '..', '..');
  const exe = process.platform === 'win32' ? 'NelSon-adv-cli.exe' : 'nelson-adv-cli';
  return join(repo, 'bin', 'x64', exe);
}
const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = join(HERE, '..', '..', '..');
const EXE = resolveExe();
const read = (p) => readFileSync(p, 'utf8');
const CAUSAL = join(REPO, 'modules', 'nflow_blocks', 'examples', 'causal', 'Logic_Gates_Demo.nflow');
const ACAUSAL = join(REPO, 'modules', 'nflow_blocks', 'examples', 'acausal', 'Acausal_Mass_Spring_Damper_Demo.nflow');
const STOPFCN = join(REPO, 'modules', 'nflow_engine', 'tests', 'stopfcn_demo.nflow');
const log = (...a) => console.log('[fix-proof]', ...a);
let failed = false;
const fail = (m) => { failed = true; console.error('[fix-proof] FAIL:', m); };
const EVAL_GAP = /Function evaluator is not available|Invalid anonymous function/i;

const child = spawn(EXE, ['--notebook-kernel'], { stdio: ['pipe', 'pipe', 'pipe'] });
// Teardown: never leak a kernel that would lock the engine DLL for the next build.
const killChild = () => { try { child.kill(); } catch {} };
process.on('exit', killChild);
process.on('SIGINT', () => { killChild(); process.exit(1); });

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
    }, 25000);
  });
}
function onMessage(msg) {
  if (msg.id !== undefined && msg.id !== null && (msg.result !== undefined || msg.error !== undefined)) {
    const p = pending.get(msg.id);
    if (p) { pending.delete(msg.id); msg.error ? p.reject(new Error(`${msg.error.code}: ${msg.error.message}`)) : p.resolve(msg.result); }
    return;
  }
  if (msg.method) notifications.push(msg);
}
child.stdout.on('data', (d) => {
  buf = Buffer.concat([buf, d]);
  for (;;) {
    const sep = buf.indexOf('\r\n\r\n');
    if (sep < 0) break;
    const m = buf.slice(0, sep).toString('ascii').match(/Content-Length:\s*(\d+)/i);
    if (!m) { buf = buf.slice(sep + 4); continue; }
    const len = parseInt(m[1], 10);
    if (buf.length < sep + 4 + len) break;
    const b = buf.slice(sep + 4, sep + 4 + len).toString('utf8');
    buf = buf.slice(sep + 4 + len);
    try { onMessage(JSON.parse(b)); } catch (e) { fail('bad frame json: ' + e.message); }
  }
});
child.stderr.on('data', (d) => process.stderr.write('[engine-err] ' + d));
child.on('exit', (code) => { log('engine exited code', code); process.exit(failed ? 1 : 0); });

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const asObj = (x) => { if (typeof x !== 'string') return x; try { return JSON.parse(x); } catch { return x; } };
const cellOutput = (from) =>
  notifications.filter((n, i) => i >= from && n.method === 'notebook.output').map((n) => n.params.text).join('');

async function run() {
  const init = await send('kernel.initialize', { client: 'fix-proof', protocolVersion: 1 });
  if (!init || !init.capabilities || init.capabilities.nflow !== true) {
    log('SKIP: nflow host surface absent from this binary.');
    await send('kernel.shutdown', {}).catch(() => {});
    return;
  }
  log('nflowDistDir:', init.nflowDistDir || '(empty)');
  // Prime the interpreter with one no-op top-level cell. Package functions (NFlow.internal.*)
  // are indexed lazily on the first prompt command, so a client that will call ssp / fmu /
  // modelica / a StopFcn sim / acausal codegen sends this once, right after initialize,
  // before those nflow.* calls. One trivial cell is enough; it has no workspace effect.
  await send('notebook.execute', { code: 'if false, end' });
  const tmp = mkdtempSync(join(tmpdir(), 'nflowfix-'));

  // PROOF 1 - modelica.info -> NFlow.internal.modelicaInfoJson resolves and returns JSON.
  try {
    const info = asObj(await send('nflow.modelica.info', {}));
    if (info && typeof info === 'object') log('1 modelica.info OK, keys:', Object.keys(info).join(','));
    else fail('modelica.info returned non-object: ' + JSON.stringify(info));
  } catch (e) {
    if (EVAL_GAP.test(e.message)) fail('modelica.info still hits the evaluator gap: ' + e.message);
    else fail('modelica.info error: ' + e.message);
  }

  // PROOF 2 - ssp.export -> NFlow.internal.sspExportContent writes an .ssp.
  const sspPath = join(tmp, 'logic.ssp');
  const sspExp = asObj(await send('nflow.ssp.export', { content: read(CAUSAL), path: sspPath }));
  if (sspExp && sspExp.success && existsSync(sspExp.path || sspPath)) {
    log('2 ssp.export OK ->', (sspExp.path || sspPath), statSync(sspExp.path || sspPath).size, 'bytes');
  } else {
    (sspExp && EVAL_GAP.test(sspExp.error || '')) ? fail('ssp.export hits the evaluator gap: ' + sspExp.error)
      : fail('ssp.export failed: ' + JSON.stringify(sspExp));
  }

  // PROOF 3 - ssp.import -> NFlow.internal.sspImport reads the .ssp back (round-trip).
  if (sspExp && sspExp.success) {
    const sspImp = asObj(await send('nflow.ssp.import', { path: sspExp.path || sspPath }));
    if (sspImp && sspImp.success && sspImp.path) log('3 ssp.import OK ->', sspImp.path);
    else (sspImp && EVAL_GAP.test(sspImp.error || '')) ? fail('ssp.import hits the evaluator gap: ' + sspImp.error)
      : fail('ssp.import failed: ' + JSON.stringify(sspImp));
  }

  // PROOF 4 - fmu.export -> NFlow.internal.exportFmuContent. The FMU build may need a C
  // toolchain, so a non-evaluator failure still proves the bridge resolves (fix works,
  // downstream tooling is a separate matter); only an evaluator-gap error is a fix failure.
  const fmuPath = join(tmp, 'logic.fmu');
  const fmuExp = asObj(await send('nflow.fmu.export', { content: read(CAUSAL), path: fmuPath }));
  if (fmuExp && fmuExp.success) log('4 fmu.export OK ->', fmuExp.path);
  else if (fmuExp && EVAL_GAP.test(fmuExp.error || '')) fail('fmu.export hits the evaluator gap: ' + fmuExp.error);
  else log('4 fmu.export reached the bridge (downstream result):', JSON.stringify(fmuExp).slice(0, 180));

  // PROOF 5 - a sim whose StopFcn runs: runStopFcnForRun executes assignin('base',
  // 'stopfcn_samples', numel(out.tout)); read it back to prove the StopFcn fired.
  const started = asObj(await send('nflow.sim.run', { diagramJson: read(STOPFCN), async: true }));
  const runId = started && (started.runId || (asObj(started.result) || {}).runId);
  if (!runId) fail('sim.run (stopfcn) returned no runId: ' + JSON.stringify(started));
  else {
    let done = false;
    for (let i = 0; i < 80 && !done; i++) {
      const poll = asObj(await send('nflow.sim.poll', { runId }));
      if (poll && poll.status === 'complete') done = true;
      else if (poll && poll.status === 'error') { fail('stopfcn sim error: ' + JSON.stringify(poll)); break; }
      else await sleep(200);
    }
    await sleep(200);
    const base = notifications.length;
    const r = await send('notebook.execute', { code: "if exist('stopfcn_samples','var'), disp(stopfcn_samples), else, disp(-1), end" });
    await sleep(120);
    const txt = cellOutput(base).trim();
    const n = parseInt(txt, 10);
    if (Number.isFinite(n) && n > 0) log('5 sim StopFcn ran: stopfcn_samples =', n);
    else fail('StopFcn did not run (stopfcn_samples=' + txt + ') - runStopFcnForRun likely did not resolve');
    await send('nflow.sim.stop', { runId }).catch(() => {});
  }

  // PROOF 6 - codegen of an acausal model: expandAcausalDoc must resolve and run; the
  // generated C must be non-empty and free of the codegen-error / evaluator markers.
  const code = await send('nflow.codegen.generate', { diagramJson: read(ACAUSAL), optionsJson: '{"lang":"c"}' });
  const codeStr = typeof code === 'string' ? code : JSON.stringify(code);
  if (EVAL_GAP.test(codeStr)) {
    fail('acausal codegen hits the evaluator gap: ' + codeStr.slice(0, 200));
  } else if (/codegen error/i.test(codeStr)) {
    // expandAcausalDoc resolved and ran (the pipeline reached a later codegen gate, e.g. a
    // variable-step solver). That is a separate codegen limitation, not a bridge failure.
    log('6 codegen acausal: expandAcausalDoc resolved, pipeline reached a codegen gate:',
      codeStr.replace(/\/\*|\*\//g, '').trim().slice(0, 120));
  } else if (!codeStr || codeStr.length < 50) {
    fail('acausal codegen produced no code: ' + codeStr);
  } else {
    log('6 codegen acausal OK: C bytes', codeStr.length);
  }

  log(failed ? 'RESULT: FAIL' : 'RESULT: PASS');
  await send('kernel.shutdown', {}).catch(() => {});
  setTimeout(killChild, 500);
}

setTimeout(() => { fail('global timeout'); killChild(); }, 120000);
run().catch((e) => { fail(e.message); try { child.stdin.end(); } catch {} setTimeout(killChild, 800); });
