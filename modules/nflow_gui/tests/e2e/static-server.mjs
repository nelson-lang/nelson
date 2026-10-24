import http from 'http';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = process.env.STATIC_SERVER_ROOT || path.resolve(__dirname, '..', '..', 'dist');
const port = Number(process.env.STATIC_SERVER_PORT || 1973);

function contentTypeByExt(ext) {
  switch (ext.toLowerCase()) {
    case '.html': return 'text/html; charset=utf-8';
    case '.js': return 'application/javascript; charset=utf-8';
    case '.css': return 'text/css; charset=utf-8';
    case '.json': return 'application/json; charset=utf-8';
    case '.svg': return 'image/svg+xml';
    case '.png': return 'image/png';
    case '.jpg':
    case '.jpeg': return 'image/jpeg';
    case '.wasm': return 'application/wasm';
    case '.map': return 'application/octet-stream';
    default: return 'application/octet-stream';
  }
}

const server = http.createServer(async (req, res) => {
  try {
    let pathname = decodeURIComponent(new URL(req.url, `http://localhost`).pathname);
    if (pathname === '/' || pathname === '') pathname = '/nflow.html';
    const safePath = path.normalize(path.join(root, pathname));
    if (!safePath.startsWith(root)) {
      res.statusCode = 403; res.end('Forbidden'); return;
    }
    let stat;
    try { stat = await fs.promises.stat(safePath); } catch (e) { stat = null; }
    let filePath = safePath;
    if (stat && stat.isDirectory()) {
      filePath = path.join(safePath, 'index.html');
    }
    try {
      const data = await fs.promises.readFile(filePath);
      res.writeHead(200, { 'Content-Type': contentTypeByExt(path.extname(filePath)) });
      res.end(data);
    } catch (e) {
      // Provide harmless stubs for known optional assets to avoid noisy
      // 404 console errors during E2E tests when running against the
      // static `dist/` server.
      const p = pathname.toLowerCase();
      if (p.endsWith('/webui.js')) {
        res.writeHead(200, { 'Content-Type': 'application/javascript; charset=utf-8' });
        res.end('// webui bridge stub for tests\n');
        return;
      }
      if (p.endsWith('/favicon.ico')) {
        res.writeHead(200, { 'Content-Type': 'image/x-icon' });
        res.end('');
        return;
      }
      if (p.endsWith('/nflow/ui-settings.json')) {
        res.writeHead(200, { 'Content-Type': 'application/json; charset=utf-8' });
        res.end(JSON.stringify({}));
        return;
      }
      res.statusCode = 404;
      res.end('Not found');
    }
  } catch (e) {
    res.statusCode = 500; res.end('Server error');
  }
});

server.listen(port, () => {
  // eslint-disable-next-line no-console
  console.log(`static-server: serving ${root} on port ${port}`);
});

process.on('SIGTERM', () => {
  server.close(() => process.exit(0));
});
