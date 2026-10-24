// Poll the NFLOW_URL until it responds (or timeout). This prevents test
// runs from failing with timeouts when the native host hasn't started.
import http from 'http';
import https from 'https';
import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function probeUrl(target, timeoutMs = 3000) {
  let parsed;
  try {
    parsed = new URL(target);
  } catch (_e) {
    return false;
  }
  const lib = parsed.protocol === 'https:' ? https : http;
  return new Promise((resolve) => {
    const req = lib.request(
      {
        method: 'GET',
        host: parsed.hostname,
        port: parsed.port,
        path: parsed.pathname + (parsed.search || ''),
        timeout: timeoutMs,
      },
      (res) => {
        resolve(Boolean(res.statusCode && res.statusCode >= 200 && res.statusCode < 500));
      }
    );
    req.on('error', () => resolve(false));
    req.on('timeout', () => {
      req.destroy();
      resolve(false);
    });
    req.end();
  });
}

function startStaticServer(port, root) {
  const script = path.join(__dirname, 'static-server.mjs');
  const env = { ...process.env, STATIC_SERVER_PORT: String(port), STATIC_SERVER_ROOT: root };
  const child = spawn(process.execPath, [script], { env, stdio: 'ignore', detached: true });
  try {
    child.unref();
  } catch (_e) {}
  return child.pid;
}

export default async function globalSetup() {
  const initialTarget = process.env.NFLOW_URL || 'http://localhost:1973/nflow.html';

  // Quick probe first
  const ok = await probeUrl(initialTarget, 2000);
  if (ok) {
    console.log(`global-setup: NFLOW_URL reachable: ${initialTarget}`);
    return;
  }

  // Not reachable - start a local static server that serves the module's
  // `dist/` folder so tests can run without the native C++ host.
  const parsed = (() => {
    try {
      return new URL(initialTarget);
    } catch (_e) {
      return null;
    }
  })();
  const port = parsed && parsed.port ? Number(parsed.port) : 1973;
  const root = path.resolve(__dirname, '..', '..', 'dist');

  console.log(`global-setup: NFLOW_URL not reachable; starting static server at port ${port} serving ${root}`);
  const pid = startStaticServer(port, root);
  const pidFile = path.join(__dirname, 'static-server.pid');
  try {
    fs.writeFileSync(pidFile, String(pid), 'utf8');
  } catch (e) {
    console.log('global-setup: failed to write pidfile', e);
  }

  // Wait for the static server to respond
  const deadline = Date.now() + 15_000;
  while (Date.now() < deadline) {
    // eslint-disable-next-line no-await-in-loop
    const ok2 = await probeUrl(initialTarget, 1000);
    if (ok2) {
      console.log(`global-setup: static server responding at ${initialTarget}`);
      return;
    }
    // eslint-disable-next-line no-await-in-loop
    await new Promise((r) => setTimeout(r, 300));
  }

  console.log(`global-setup: timeout waiting for server at ${initialTarget}`);
}
