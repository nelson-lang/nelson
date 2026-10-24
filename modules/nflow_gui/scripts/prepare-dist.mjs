#!/usr/bin/env node
// ============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================
import { promises as fs } from 'fs';
import path from 'path';
import crypto from 'crypto';
import { promisify } from 'util';
import { exec as _exec } from 'child_process';
import { inlineStylesheetImport } from './shared-styles.mjs';
const exec = promisify(_exec);

async function hashRenameAppScripts(htmlPath, appDir) {
  try {
    let html = await fs.readFile(htmlPath, 'utf8');
    const rel = /(?:href|src)="(\.\/app\/([A-Za-z0-9_-]+)\.js)"/g;
    const seen = new Map(); // original ref -> hashed ref
    let m;
    while ((m = rel.exec(html)) !== null) {
      const ref = m[1]; // ./app/app.js
      const base = m[2]; // app
      if (seen.has(ref)) continue;
      const srcFile = path.join(appDir, `${base}.js`);
      let buf;
      try {
        buf = await fs.readFile(srcFile);
      } catch {
        // Not emitted under the stable name: a vite-hashed entry (app-<hash>.js).
        // Rewrite the reference to the emitted file instead of copying.
        try {
          const entries = await fs.readdir(appDir);
          const hashed = entries.filter((f) =>
            new RegExp(`^${base}-[A-Za-z0-9_-]+\\.js$`).test(f)
          );
          if (hashed.length === 1) seen.set(ref, `./app/${hashed[0]}`);
        } catch {
          /* leave the URL untouched */
        }
        continue;
      }
      const hash = crypto.createHash('sha1').update(buf).digest('hex').slice(0, 12);
      const hashedName = `${base}.${hash}.js`;
      await fs.copyFile(srcFile, path.join(appDir, hashedName));
      seen.set(ref, `./app/${hashedName}`);
    }
    // Rewrite each original ref (both attributes) to its hashed name. Done after
    // collection so app.js's modulepreload href and script src get the SAME name.
    for (const [ref, hashedRef] of seen) {
      const esc = ref.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
      html = html.replace(new RegExp(esc, 'g'), hashedRef);
    }
    await fs.writeFile(htmlPath, html, 'utf8');
    console.log(`Hash-renamed ${seen.size} app script(s) in ${htmlPath}`);
  } catch (err) {
    console.warn('hashRenameAppScripts failed:', err && err.message ? err.message : err);
  }
}

// Resolve repo roots robustly on Windows and POSIX
const repoRoot = path.dirname(new URL(import.meta.url).pathname).replace(/^\/(.:)/, '$1');
const tsRoot = path.join(repoRoot, '..', 'src', 'ts');
const distRoot = process.argv[2]
  ? path.resolve(process.argv[2])
  : path.join(repoRoot, '..', 'dist');
const tsLibrariesRoot = path.join(tsRoot, 'libraries');
const distLibrariesRoot = path.join(distRoot, 'libraries');
// The declarative block libraries (library.json manifests + exported SVG icons)
// are data owned by the nflow_blocks module; the TypeScript renderers stay here.
const blocksLibrariesRoot = path.join(repoRoot, '..', '..', 'nflow_blocks', 'libraries');

async function walk(dir) {
  const entries = await fs.readdir(dir, { withFileTypes: true });
  const files = [];
  for (const e of entries) {
    const full = path.join(dir, e.name);
    if (e.isDirectory()) files.push(...await walk(full));
    else if (e.isFile() && e.name === 'library.json') files.push(full);
  }
  return files;
}

async function copyRecursive(src, dest) {
  const stat = await fs.stat(src);
  if (stat.isDirectory()) {
    await fs.mkdir(dest, { recursive: true });
    const entries = await fs.readdir(src, { withFileTypes: true });
    for (const e of entries) {
      await copyRecursive(path.join(src, e.name), path.join(dest, e.name));
    }
  } else if (stat.isFile()) {
    await fs.mkdir(path.dirname(dest), { recursive: true });
    await fs.copyFile(src, dest);
  }
}

async function copyKatexRuntime(src, dest) {
  await fs.rm(dest, { recursive: true, force: true });
  await fs.mkdir(dest, { recursive: true });
  for (const filename of ['katex.min.css', 'katex.min.js']) {
    await fs.copyFile(path.join(src, filename), path.join(dest, filename));
  }
  await copyRecursive(path.join(src, 'fonts'), path.join(dest, 'fonts'));
}

async function main() {
  try {
    await copyManifests();
    await copyGlobalFiles();
    await inlineSharedExampleGalleryStyles();
    await copyLocaleFiles();
    await ensureKatexAssets();
    await copyPreGeneratedSVGs();
    await generateBuiltinIconSprite();
    // Ensure nflow.html is copied to the dist root. Some runtime code
    // (C++ bootstrap) and the generated bootstrap page expect this file to be
    // present in the served root so relative paths resolve correctly.
    try {
      const srcCommon = path.join(tsRoot, 'nflow.html');
      const dstCommon = path.join(distRoot, 'nflow.html');
      await fs.copyFile(srcCommon, dstCommon);
      console.log(`Copied ${srcCommon} -> ${dstCommon}`);
      await hashRenameAppScripts(dstCommon, path.join(distRoot, 'app'));
    } catch (err) {
      console.warn('nflow.html not found or copy failed:', err && err.message ? err.message : err);
    }

    // Ensure a served UI settings file exists at dist/nflow/ui-settings.json
    // so fetch('nflow/ui-settings.json') doesn't produce noisy 404s when
    // running a plain static server (the E2E static server already stubs
    // this path for tests).
    try {
      const srcUi = path.join(tsRoot, 'config', 'ui-settings.json');
      const dstUiDir = path.join(distRoot, 'nflow');
      const dstUi = path.join(dstUiDir, 'ui-settings.json');
      try {
        await fs.stat(srcUi);
        await fs.mkdir(dstUiDir, { recursive: true });
        await fs.copyFile(srcUi, dstUi);
        console.log(`Copied ${srcUi} -> ${dstUi}`);
      } catch {
        // Fall back to writing an empty JSON object so the file exists.
        await fs.mkdir(dstUiDir, { recursive: true });
        await fs.writeFile(dstUi, JSON.stringify({}, null, 2), 'utf8');
        console.log(`Wrote default ${dstUi}`);
      }
    } catch (err) {
      console.warn('Failed to ensure nflow/ui-settings.json in dist:', err && err.message ? err.message : err);
    }

    // Run strip script to remove any generated TypeScript artefacts (e.g. blocks.ts)
    try {
      // repoRoot currently points to the scripts directory (where this file lives).
      // Resolve the module root one level up so we run strip-dist-ts from the
      // module root and avoid constructing paths like scripts/scripts/...
      const moduleRoot = path.dirname(repoRoot);
      const stripScript = path.join(moduleRoot, 'scripts', 'strip-dist-ts.mjs');
      console.log('Running strip-dist-ts to remove generated .ts files from dist...');
      await exec(`node "${stripScript}" "${distRoot}"`, { cwd: moduleRoot });
    } catch (err) {
      console.warn('strip-dist-ts failed:', err && err.message ? err.message : err);
    }

    console.log('prepare-dist completed');
  } catch (err) {
    console.error('prepare-dist failed:', err);
    process.exit(1);
  }
}

async function inlineSharedExampleGalleryStyles() {
  const importRule = "@import url('../../../../webview/frontend/examples/gallery-shared.css');";
  const stylesheetPath = path.join(distRoot, 'styles', 'style.css');
  const sharedStylesheetPath = path.resolve(
    repoRoot,
    '..',
    '..',
    'webview',
    'frontend',
    'examples',
    'gallery-shared.css'
  );
  const [stylesheet, sharedStylesheet] = await Promise.all([
    fs.readFile(stylesheetPath, 'utf8'),
    fs.readFile(sharedStylesheetPath, 'utf8'),
  ]);
  await fs.writeFile(
    stylesheetPath,
    inlineStylesheetImport(stylesheet, importRule, sharedStylesheet),
    'utf8'
  );
  console.log(`Inlined ${sharedStylesheetPath} -> ${stylesheetPath}`);
}

async function copyManifests() {
  const manifests = await walk(blocksLibrariesRoot);
  for (const srcPath of manifests) {
    const rel = path.relative(blocksLibrariesRoot, srcPath);
    const destPathDist = path.join(distLibrariesRoot, rel);
    await fs.mkdir(path.dirname(destPathDist), { recursive: true });
    await fs.copyFile(srcPath, destPathDist);
    console.log(`Copied ${srcPath} -> ${destPathDist}`);
  }
}

async function copyGlobalFiles() {
  // Minimal set: copy libraries, assets and styles. Locale copying is
  // handled by copyLocaleFiles(), and the `app` bundle is produced by
  // the build step (esbuild) so we avoid copying source `app` files.
  const toCopy = ['libraries', 'assets', 'styles', 'config'];

  for (const relPath of toCopy) {
    const src = path.join(tsRoot, relPath);
    const destDist = path.join(distRoot, relPath);
    try {
      await copyRecursive(src, destDist);
      console.log(`Copied ${src} -> ${destDist}`);
    } catch (err) {
      if (err && err.code === 'ENOENT') {
        console.warn(`Skipping missing path for dist: ${relPath}`);
      } else {
        throw err;
      }
    }
  }
}

async function ensureKatexAssets() {
  try {
    const srcKatex = path.join(tsRoot, 'assets', 'katex');
    const destKatex = path.join(distRoot, 'assets', 'katex');
    try {
      const st = await fs.stat(srcKatex);
      if (st && st.isDirectory()) {
        await copyKatexRuntime(srcKatex, destKatex);
        console.log(`Copied ${srcKatex} -> ${destKatex}`);
      }
    } catch {
      // fallback: copy from this package's node_modules/katex/dist if present
      try {
        const packageRoot = path.resolve(repoRoot, '..');
        const nodeKatex = path.join(packageRoot, 'node_modules', 'katex', 'dist');
        const st2 = await fs.stat(nodeKatex);
        if (st2 && st2.isDirectory()) {
          await copyKatexRuntime(nodeKatex, destKatex);
          console.log(`Copied ${nodeKatex} -> ${destKatex}`);
        }
      } catch {
        console.warn(`KaTeX assets not found. Tried ${srcKatex} and ${path.join(path.resolve(repoRoot, '..'), 'node_modules', 'katex', 'dist')}.`);
      }
    }
  } catch (e) {
    console.warn('Failed to copy KaTeX assets:', e && e.message ? e.message : e);
  }
}

async function copyPreGeneratedSVGs() {
  try {
    console.log('Copying pre-generated SVGs from nflow_blocks/libraries/* -> dist/libraries/*');
    const libs = await fs.readdir(blocksLibrariesRoot, { withFileTypes: true });
      const repoRootProject = path.resolve(repoRoot, '..', '..', '..');
      const exporterPath = path.join(repoRootProject, 'tools', 'export-library-svgs.mjs');
      let exporterExists = false;
      try { await fs.stat(exporterPath); exporterExists = true; } catch { exporterExists = false; }
      if (exporterExists) {
        console.log('Running exporter: tools/export-library-svgs.mjs');
        try {
          const { stdout, stderr } = await exec('node tools/export-library-svgs.mjs', { cwd: repoRootProject });
          if (stdout) process.stdout.write(stdout);
          if (stderr) process.stderr.write(stderr);
        } catch (err) {
          console.error('Exporter failed:', err);
          // fall back to copying pre-generated svgs below
        }
      }

      if (!exporterExists) console.log('Exporter not found; copying pre-generated SVGs from nflow_blocks/libraries/*/exports -> dist/libraries/*');

      for (const le of libs) {
        if (!le.isDirectory()) continue;
        const libName = le.name;
        const srcExports = path.join(blocksLibrariesRoot, libName, 'exports');
        try {
          const stat = await fs.stat(srcExports);
          if (!stat.isDirectory()) continue;
        } catch {
          continue;
        }
        const files = await fs.readdir(srcExports, { withFileTypes: true });
        const destDir = path.join(distLibrariesRoot, libName);
        const destExports = path.join(destDir, 'exports');
        await fs.mkdir(destDir, { recursive: true });
        await fs.mkdir(destExports, { recursive: true });
        for (const f of files) {
          if (!f.isFile()) continue;
          if (!f.name.toLowerCase().endsWith('.svg') && !f.name.toLowerCase().endsWith('.png')) continue;
          const srcFile = path.join(srcExports, f.name);
          // Runtime fetches `<lib>/exports/<name>.svg`; a flattened `<lib>/<name>.svg`
          // copy is kept for backward compatibility with the previous dist layout.
          for (const dstFile of [path.join(destExports, f.name), path.join(destDir, f.name)]) {
            try {
              await fs.copyFile(srcFile, dstFile);
              console.log(`Copied ${srcFile} -> ${dstFile}`);
            } catch (e) {
              console.warn(`Failed to copy ${srcFile} -> ${dstFile}:`, e.message || e);
            }
          }
        }
      }
  } catch (err) {
    console.error('SVG copy failed:', err);
  }
}

// Build ONE sprite file from the per-library exports/*.svg icons. The
// individual .svg files stay on disk (authored/versioned as before); the sprite
// is a derived build artifact the runtime loads in a single request so the
// WebView2 host no longer makes 183 slow round-trips (which let diagrams paint
// before their icons arrived). Each icon becomes a <symbol> tagged with its
// library dir and file name so the runtime can map it back to the canonical
// `<lib>/exports/<name>.svg` URL the palette and canvas request.
function svgToSymbol(raw, lib, fname) {
  const m = raw.match(/<svg\b([^>]*)>([\s\S]*)<\/svg>/i);
  if (!m) return null;
  const attrs = m[1];
  const inner = m[2];
  const grab = (re) => {
    const mm = attrs.match(re);
    return mm && mm[1] ? mm[1] : '';
  };
  const vb = grab(/\bviewBox\s*=\s*"([^"]*)"/i);
  const w = grab(/\bwidth\s*=\s*"([^"]*)"/i);
  const h = grab(/\bheight\s*=\s*"([^"]*)"/i);
  const fill = grab(/\bfill\s*=\s*"([^"]*)"/i);
  const stroke = grab(/\bstroke\s*=\s*"([^"]*)"/i);
  let viewBox = vb;
  if (!viewBox && /^\d*\.?\d+/.test(w) && /^\d*\.?\d+/.test(h)) {
    viewBox = `0 0 ${parseFloat(w)} ${parseFloat(h)}`;
  }
  const id = `${lib}__${fname.replace(/\.svg$/i, '')}`;
  const esc = (s) => String(s).replace(/"/g, '&quot;');
  const vbAttr = viewBox ? ` viewBox="${esc(viewBox)}"` : '';
  const fillAttr = fill ? ` fill="${esc(fill)}"` : '';
  const strokeAttr = stroke ? ` stroke="${esc(stroke)}"` : '';
  return `<symbol id="${esc(id)}" data-lib="${esc(lib)}" data-name="${esc(fname)}"${vbAttr}${fillAttr}${strokeAttr}>${inner}</symbol>`;
}

async function generateBuiltinIconSprite() {
  try {
    const libs = await fs.readdir(distLibrariesRoot, { withFileTypes: true });
    const symbols = [];
    let count = 0;
    for (const le of libs) {
      if (!le.isDirectory()) continue;
      const libName = le.name;
      const exportsDir = path.join(distLibrariesRoot, libName, 'exports');
      let files;
      try {
        files = await fs.readdir(exportsDir, { withFileTypes: true });
      } catch {
        continue;
      }
      for (const f of files) {
        if (!f.isFile() || !f.name.toLowerCase().endsWith('.svg')) continue;
        const raw = await fs.readFile(path.join(exportsDir, f.name), 'utf8');
        const sym = svgToSymbol(raw, libName, f.name);
        if (sym) {
          symbols.push(sym);
          count++;
        }
      }
    }
    const sprite =
      '<svg xmlns="http://www.w3.org/2000/svg" style="display:none" aria-hidden="true">\n' +
      symbols.join('\n') +
      '\n</svg>\n';
    const out = path.join(distLibrariesRoot, '_builtin-icons.sprite.svg');
    await fs.writeFile(out, sprite, 'utf8');
    console.log(`Wrote builtin icon sprite (${count} icons) -> ${out}`);
  } catch (err) {
    console.warn('Builtin icon sprite generation failed:', err && err.message ? err.message : err);
  }
}

async function copyLocaleFiles() {
  try {
    const repoRootProject = path.resolve(repoRoot, '..', '..', '..');
    const moduleLocaleDir = path.join(repoRoot, '..', 'locale');
    const srcLocaleDirCandidates = [moduleLocaleDir, path.join(repoRootProject, 'locale')];
    const destLocaleDir = path.join(distRoot, 'locale');

    let found = false;
    for (const srcLocaleDir of srcLocaleDirCandidates) {
      let statLocale;
      try {
        statLocale = await fs.stat(srcLocaleDir);
      } catch {
        statLocale = null;
      }
      if (!statLocale || !statLocale.isDirectory()) {
        // try next candidate
        continue;
      }
      const entries = await fs.readdir(srcLocaleDir, { withFileTypes: true });
      for (const e of entries) {
        if (!e.isFile()) continue;
        if (/^nflow-ui-.*\.json$/i.test(e.name)) {
          const src = path.join(srcLocaleDir, e.name);
          const dest = path.join(destLocaleDir, e.name);
          await fs.mkdir(path.dirname(dest), { recursive: true });
          await fs.copyFile(src, dest);
          console.log(`Copied locale ${src} -> ${dest}`);
          found = true;
        }
      }
      if (found) break;
    }
    if (!found) {
      console.log('No locale directory found in module or project root; tried', srcLocaleDirCandidates.join(', '));
    }
  } catch (err) {
    console.warn('Failed to copy frontend locale files:', err && err.message ? err.message : err);
  }
}

main();
