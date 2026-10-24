// Postbuild: fold the CSS into index.html (keeping JS external to stay
// CSP-friendly) so the shipped bundle in modules/web_gui/frontend/dist is just
// two files — index.html + app.js. Installed builds use this module-tree dist;
// development builds run from bin/<platform>, so refresh that runtime copy too.
//
// Runs automatically after `vite build` via the npm "postbuild" hook.

import { cpSync, existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';

const here = dirname(fileURLToPath(import.meta.url));
const frontend = resolve(here, '..');
const dist = resolve(frontend, 'dist'); // modules/web_gui/frontend/dist
const repoRoot = resolve(frontend, '..', '..', '..');

const indexPath = join(dist, 'index.html');
if (!existsSync(indexPath)) {
  console.error(`[postbuild] ${indexPath} not found — did the build run?`);
  process.exit(1);
}

let html = readFileSync(indexPath, 'utf8');

// Keep the WebUI bridge out of Vite's HTML transform, then restore it in the
// shipped file before the application bundle. Standalone dev runs without this
// script and the RPC client falls back to its mock backend.
const webuiBridgeScript = '<script src="/webui.js"></script>';
if (!html.includes('src="/webui.js"')) {
  const appScriptRe = /<script\s+type=["']module["'][^>]*src=["']\.\/app\.js["'][^>]*><\/script>/i;
  if (appScriptRe.test(html)) {
    html = html.replace(appScriptRe, `${webuiBridgeScript}\n    $&`);
  } else {
    html = html.replace('</head>', `    ${webuiBridgeScript}\n  </head>`);
  }
}

// Inline every emitted stylesheet <link> as a <style> block, then delete the
// now-unreferenced .css file so the dist stays minimal.
const linkRe = /<link[^>]+rel=["']stylesheet["'][^>]*href=["']\.?\/?([^"']+\.css)["'][^>]*>/gi;
const cssFiles = new Set();
html = html.replace(linkRe, (_m, href) => {
  const cssPath = join(dist, href);
  if (!existsSync(cssPath)) return _m;
  cssFiles.add(cssPath);
  return `<style>${readFileSync(cssPath, 'utf8')}</style>`;
});
for (const f of cssFiles) rmSync(f, { force: true });

// Cache-bust app.js. Its filename is stable ("app.js"), so the WebView (and any
// HTTP cache) keeps the previous bundle by URL across rebuilds — the inlined CSS
// in index.html refreshes but the JS stays stale. Append a short content hash so
// a changed bundle is always refetched, and an unchanged one still caches.
const appJsPath = join(dist, 'app.js');
if (existsSync(appJsPath)) {
  const hash = createHash('sha256').update(readFileSync(appJsPath)).digest('hex').slice(0, 10);
  html = html.replace(/(src=["']\.\/app\.js)(\?[^"']*)?(["'])/i, `$1?v=${hash}$3`);
}

writeFileSync(indexPath, html, 'utf8');

// The development executables resolve their module tree below bin/<platform>.
// Keep every configured runtime tree in sync so rebuilding the frontend cannot
// leave WebView serving a stale bundle from an earlier build.
const runtimePlatforms = ['linux', 'macOS', 'x64', 'ARM64'];
for (const platform of runtimePlatforms) {
  const binPlatform = join(repoRoot, 'bin', platform);
  if (!existsSync(binPlatform)) continue;
  const target = join(binPlatform, 'web_gui', 'frontend', 'dist');
  mkdirSync(target, { recursive: true });
  cpSync(dist, target, { recursive: true });
  console.log(`[postbuild] deployed dist -> ${target}`);
}

const files = readFileSync(indexPath, 'utf8').length;
console.log(
  `[postbuild] modules/web_gui/frontend/dist ready (index.html ${files} bytes + app.js).`
);
