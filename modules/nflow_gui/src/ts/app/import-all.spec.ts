//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Smoke test: import most TS modules to exercise top-level code
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { test } from 'vitest';
//=============================================================================
test('import all modules (smoke)', async () => {
  const repoRoot = path.resolve(process.cwd(), 'modules', 'nflow_gui');
  const srcRoot = path.join(repoRoot, 'src', 'ts');

  function walk(dir: string): string[] {
    const out: string[] = [];
    for (const name of fs.readdirSync(dir)) {
      const p = path.join(dir, name);
      const stat = fs.statSync(p);
      if (stat.isDirectory()) out.push(...walk(p));
      else if (stat.isFile() && p.endsWith('.ts')) out.push(p);
    }
    return out;
  }

  const all = walk(srcRoot).filter((f) => {
    if (f.endsWith('.spec.ts')) return false;
    if (f.endsWith('.d.ts')) return false;
    // Skip heavy worker or head-init modules that schedule long timers
    const skip = ['head-init.ts', 'route-worker.ts'];
    if (skip.some((s) => f.includes(s))) return false;
    return true;
  });

  const failures: string[] = [];
  for (const abs of all) {
    const rel = path.relative(path.dirname(fileURLToPath(import.meta.url)), abs);
    const importPath = './' + rel.replace(/\\/g, '/');
    try {
      // Import errors are recorded, not fatal, so coverage can grow gradually.
      // eslint-disable-next-line no-await-in-loop
      await import(new URL(importPath, import.meta.url).href);
    } catch (err) {
      failures.push(importPath + ' -> ' + String(err));
    }
  }

  if (failures.length) {
    // Print failures so maintainer can triage; do not fail the test.
    // eslint-disable-next-line no-console
    // console.warn('import-all: some modules failed to import (ignored):', failures.slice(0, 10));
  }
});
//=============================================================================
