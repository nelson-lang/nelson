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
// Launches a real Nelson web desktop for back-to-back tests.
// ============================================================================
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(scriptDir, '..', '..', '..', '..', '..');
const b2bRoot = path.join(repoRoot, 'temp', 'web_gui_b2b');
const b2bWorkdir = path.join(b2bRoot, 'workdir');
const b2bPreferences = path.join(b2bRoot, 'preferences');
fs.mkdirSync(b2bWorkdir, { recursive: true });
fs.mkdirSync(b2bPreferences, { recursive: true });
fs.mkdirSync(path.join(b2bWorkdir, 'b2b_menu_folder'), { recursive: true });
fs.writeFileSync(path.join(b2bWorkdir, 'b2b_menu_fixture.m'), '%% fixture\n');

const platformCandidates = {
  darwin: [
    path.join(repoRoot, 'bin', 'macOS', 'nelson-webview-exec'),
    path.join(repoRoot, 'bin', 'macOS', 'nelson-webview'),
  ],
  linux: [
    path.join(repoRoot, 'bin', 'linux', 'nelson-webview-exec'),
    path.join(repoRoot, 'bin', 'linux', 'nelson-webview'),
  ],
  win32: [
    path.join(repoRoot, 'bin', 'x64', 'nelson-webview.exe'),
    path.join(repoRoot, 'bin', 'ARM64', 'nelson-webview.exe'),
  ],
};

const fallbackCandidates = Object.entries(platformCandidates)
  .filter(([platform]) => platform !== process.platform)
  .flatMap(([, paths]) => paths);

const candidates = [
  process.env.NELSON_WEBVIEW_PATH,
  ...(platformCandidates[process.platform] || []),
  ...fallbackCandidates,
].filter(Boolean);
const nelsonWeb = candidates.find((p) => fs.existsSync(p));
if (!nelsonWeb) {
  console.error(
    'start-nelson-webview: Nelson WebView executable not found. Build Nelson first or set NELSON_WEBVIEW_PATH.'
  );
  process.exit(1);
}

console.log(`start-nelson-webview: launching ${nelsonWeb}`);
const child = spawn(nelsonWeb, ['--web', '--url', 'localhost', '--port', '22333'], {
  cwd: b2bWorkdir,
  env: {
    ...process.env,
    NELSON_PREFERENCES_PATH: b2bPreferences,
    NELSON_USERPATH: b2bWorkdir,
  },
  stdio: 'inherit',
});

const stop = () => {
  try {
    child.kill();
  } catch {
    // ignore
  }
};
process.on('SIGTERM', stop);
process.on('SIGINT', stop);
process.on('exit', stop);

child.on('exit', (code) => {
  process.exit(code ?? 0);
});
