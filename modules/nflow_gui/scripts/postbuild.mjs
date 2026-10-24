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
// Post-build step after `vite build`:
//   1. bundle the classic (non-module) helper scripts as IIFE with esbuild
//   2. bundle the per-library blocks.ts entries (or emit empty ESM stubs)
//   3. run prepare-dist.mjs (nflow.html, styles, libraries, KaTeX, locales,
//      SVG exports, ui-settings) which also strips .ts sources from dist/
// ============================================================================
import { build } from 'esbuild';
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const moduleRoot = path.resolve(scriptDir, '..');
const srcTs = path.join(moduleRoot, 'src', 'ts');
const dist = path.resolve(moduleRoot, process.env.NFLOW_DIST_DIR || 'dist');
const isWasmDistribution = Boolean(process.env.NFLOW_DIST_DIR);

const esbuildCommon = {
  bundle: true,
  target: 'es2020',
  minify: true,
  sourcemap: !isWasmDistribution,
};

// -- 1. IIFE helper scripts (loaded as classic scripts by nflow.html) --------
const iifeEntries = ['head-init', 'floating-toolbar', 'wheel-prevent'];
for (const name of iifeEntries) {
  await build({
    ...esbuildCommon,
    entryPoints: [path.join(srcTs, 'app', `${name}.ts`)],
    outfile: path.join(dist, 'app', `${name}.js`),
    format: 'iife',
  });
  console.log(`postbuild: bundled app/${name}.js (iife)`);
}

// -- 2. Per-library blocks bundles (ESM) or empty stubs ----------------------
const librariesDir = path.join(srcTs, 'libraries');
const libraryNames = fs
  .readdirSync(librariesDir, { withFileTypes: true })
  .filter((e) => e.isDirectory())
  .map((e) => e.name);
for (const lib of libraryNames) {
  const entry = path.join(librariesDir, lib, 'blocks.ts');
  const outfile = path.join(dist, 'libraries', lib, 'blocks.js');
  fs.mkdirSync(path.dirname(outfile), { recursive: true });
  if (fs.existsSync(entry)) {
    await build({ ...esbuildCommon, entryPoints: [entry], outfile, format: 'esm' });
    console.log(`postbuild: bundled libraries/${lib}/blocks.js`);
  } else {
    fs.writeFileSync(outfile, 'export {};\n');
  }
}

// -- 3. Static assets, locales, KaTeX, html, strip ---------------------------
if (isWasmDistribution) {
  await build({
    ...esbuildCommon,
    entryPoints: [path.join(srcTs, 'wasm', 'fmu-wasi.ts')],
    outfile: path.join(dist, 'fmu', 'wasi.js'),
    format: 'esm',
    platform: 'browser',
  });
}
execFileSync(process.execPath, [path.join(scriptDir, 'prepare-dist.mjs'), dist], {
  stdio: 'inherit',
  cwd: moduleRoot,
});

console.log('postbuild: done');
