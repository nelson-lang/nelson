// Postbuild for the slim figure page: fold the CSS into figure.html, inject the
// WebUI bridge, cache-bust figure.js, then copy the figure files next to the
// desktop bundle in modules/gui/dist (and the runtime bin trees) so a native
// figure window can be served from the same root folder as the desktop.
//
// Runs automatically after `vite build --config vite.figure.config.ts` via the
// npm "postbuild:figure" hook. It must run after the main `build` (whose
// postbuild fills and deploys dist/); the CMake frontend build orders it so.

import {
  cpSync,
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';

const here = dirname(fileURLToPath(import.meta.url));
const frontend = resolve(here, '..');
const distFigure = resolve(frontend, 'dist-figure');
const dist = resolve(frontend, '..', '..', '..', 'dist'); // modules/gui/dist
const repoRoot = resolve(frontend, '..', '..', '..', '..', '..');

const indexPath = join(distFigure, 'figure.html');
if (!existsSync(indexPath)) {
  console.error(`[postbuild:figure] ${indexPath} not found — did the figure build run?`);
  process.exit(1);
}

let html = readFileSync(indexPath, 'utf8');

// Restore the WebUI bridge before the figure bundle (kept out of Vite's HTML
// transform). Standalone dev runs without this script and falls back to the mock.
const webuiBridgeScript = '<script src="/webui.js"></script>';
if (!html.includes('src="/webui.js"')) {
  const appScriptRe =
    /<script\s+type=["']module["'][^>]*src=["']\.\/figure\.js["'][^>]*><\/script>/i;
  if (appScriptRe.test(html)) {
    html = html.replace(appScriptRe, `${webuiBridgeScript}\n    $&`);
  } else {
    html = html.replace('</head>', `    ${webuiBridgeScript}\n  </head>`);
  }
}

// Inline every emitted stylesheet <link> as a <style> block, then delete the
// now-unreferenced .css file so the figure dist stays minimal.
const linkRe = /<link[^>]+rel=["']stylesheet["'][^>]*href=["']\.?\/?([^"']+\.css)["'][^>]*>/gi;
const cssFiles = new Set();
html = html.replace(linkRe, (_m, href) => {
  const cssPath = join(distFigure, href);
  if (!existsSync(cssPath)) return _m;
  cssFiles.add(cssPath);
  return `<style>${readFileSync(cssPath, 'utf8')}</style>`;
});
for (const f of cssFiles) rmSync(f, { force: true });

// Cache-bust figure.js: its filename is stable, so append a short content hash so
// a changed bundle is always refetched and an unchanged one still caches.
const appJsPath = join(distFigure, 'figure.js');
if (existsSync(appJsPath)) {
  const hash = createHash('sha256').update(readFileSync(appJsPath)).digest('hex').slice(0, 10);
  html = html.replace(/(src=["']\.\/figure\.js)(\?[^"']*)?(["'])/i, `$1?v=${hash}$3`);
}

writeFileSync(indexPath, html, 'utf8');

// Copy the figure page next to the desktop bundle. dist/ already holds
// index.html + app.js from the main build; the figure files have distinct names
// and never clobber them. Keep every configured runtime tree in sync too.
function deployFigureFiles(target) {
  mkdirSync(target, { recursive: true });
  for (const name of readdirSync(distFigure)) {
    cpSync(join(distFigure, name), join(target, name), { recursive: true });
  }
}

deployFigureFiles(dist);
console.log(`[postbuild:figure] deployed figure page -> ${dist}`);

const runtimePlatforms = ['linux', 'macOS', 'x64', 'ARM64'];
for (const platform of runtimePlatforms) {
  const binPlatform = join(repoRoot, 'bin', platform);
  if (!existsSync(binPlatform)) continue;
  const target = join(binPlatform, 'gui', 'dist');
  if (!existsSync(target)) continue;
  deployFigureFiles(target);
  console.log(`[postbuild:figure] deployed figure page -> ${target}`);
}

const bytes = readFileSync(indexPath, 'utf8').length;
console.log(`[postbuild:figure] figure.html ${bytes} bytes + figure.js ready.`);
