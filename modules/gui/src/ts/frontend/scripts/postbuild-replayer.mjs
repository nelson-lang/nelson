//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Postbuild for the figure replayer bundle: mirror the single-file bundle in
// modules/gui/dist-replayer into each bin/<platform> runtime tree, so a
// development build whose module tree resolves below bin/<platform> finds
// the bundle the same way an installed build finds it in the module tree.
//
// Runs automatically after `vite build --config vite.replayer.config.ts` via the
// npm "postbuild:replayer" hook.

import { cpSync, existsSync, mkdirSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const frontend = resolve(here, '..');
const distReplayer = resolve(frontend, '..', '..', '..', 'dist-replayer'); // modules/gui/dist-replayer
const repoRoot = resolve(frontend, '..', '..', '..', '..', '..');

if (!existsSync(join(distReplayer, 'nelson-figure-replayer.js'))) {
  console.error('[postbuild:replayer] bundle not found — did build:replayer run?');
  process.exit(1);
}

const runtimePlatforms = ['linux', 'macOS', 'x64', 'ARM64'];
for (const platform of runtimePlatforms) {
  const binPlatform = join(repoRoot, 'bin', platform);
  if (!existsSync(binPlatform)) continue;
  const target = join(binPlatform, 'gui', 'dist-replayer');
  mkdirSync(target, { recursive: true });
  cpSync(distReplayer, target, { recursive: true });
  console.log(`[postbuild:replayer] deployed dist-replayer -> ${target}`);
}

console.log('[postbuild:replayer] modules/gui/dist-replayer ready.');
