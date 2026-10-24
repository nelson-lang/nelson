//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { readFileSync } from 'node:fs';
import path from 'node:path';
//=============================================================================
const cwd = path.resolve(process.cwd());
const moduleDir = path.basename(cwd) === 'nflow_gui' ? cwd : path.join(cwd, 'modules', 'nflow_gui');
const themes = readFileSync(path.join(moduleDir, 'src/ts/styles/themes.css'), 'utf8');
const shell = readFileSync(path.join(moduleDir, 'src/ts/styles/style.css'), 'utf8');
test('NFlow uses the common light and dark desktop palette', () => {
  expect(themes).toContain('/* Theme Name: Default */');
  expect(themes).toMatch(/:root\s*{[^}]*--nflow-foreground:\s*#1f1f1f/s);
  expect(themes).toMatch(/:root\s*{[^}]*--nflow-border:\s*#d4d4d4/s);
  expect(themes).toMatch(/:root\s*{[^}]*--nflow-statusbar-background:\s*#f3f3f3/s);
  expect(themes).toMatch(/\[data-theme=['"]dark['"]\]\s*{[^}]*--nflow-background:\s*#1e1e1e/s);
  expect(themes).toMatch(/\[data-theme=['"]dark['"]\]\s*{[^}]*--nflow-foreground:\s*#d4d4d4/s);
  expect(themes).toMatch(/\[data-theme=['"]dark['"]\]\s*{[^}]*--nflow-border:\s*#333333/s);
  expect(themes).toMatch(
    /\[data-theme=['"]dark['"]\]\s*{[^}]*--nflow-statusbar-background:\s*#252526/s
  );
});
//=============================================================================
test('NFlow uses the common desktop shell metrics', () => {
  expect(shell).toMatch(/\.titlebar\s*{[^}]*height:\s*32px[^}]*flex:\s*0 0 32px/s);
  expect(shell).toMatch(/\.activity-bar\s*{[^}]*width:\s*40px/s);
  expect(shell).toMatch(/\.activity-icon\s*{[^}]*width:\s*32px[^}]*height:\s*32px/s);
  expect(shell).not.toMatch(/\.activity-icon\s*{[^}]*width:\s*40px/s);
  expect(shell).not.toMatch(/\.activity-icon\s*{[^}]*height:\s*40px/s);
  expect(shell).toMatch(/\.activity-icon svg\s*{[^}]*width:\s*20px[^}]*height:\s*20px/s);
  expect(shell).toMatch(/\.status-bar\s*{[^}]*flex:\s*0 0 24px/s);
  expect(shell).toMatch(/\.status-bar\s*{[^}]*height:\s*24px/s);
});
//=============================================================================
test('library block icons fill their reserved slot', () => {
  expect(shell).toMatch(/\.tool-icon\s*{[^}]*width:\s*32px[^}]*height:\s*32px/s);
  expect(shell).toMatch(/\.tool-icon\s*>\s*svg\s*{[^}]*width:\s*100%[^}]*height:\s*100%/s);
});
//=============================================================================
test('diagram interaction states use the common desktop palette', () => {
  expect(shell).toMatch(/\.port\s*{[^}]*fill:\s*var\(--nflow-panel-background\)/s);
  expect(shell).toMatch(/\.port\s*{[^}]*stroke:\s*var\(--nflow-foreground-secondary\)/s);
  expect(shell).toMatch(/\.port\.pending\s*{[^}]*fill:\s*var\(--accent\)/s);
  expect(shell).not.toContain('#ffd700');
  expect(shell).not.toContain('drop-shadow(0 0 6px var(--accent))');
});
//=============================================================================
test('desktop chrome is not text-selectable while editable content remains selectable', () => {
  expect(shell).toMatch(/\.titlebar,[^{]+#canvas,[^{]+\.status-bar\s*{[^}]*user-select:\s*none/s);
  expect(shell).toMatch(
    /\.sidebar input,[^{]+\.nf-console-panel,[^{]+\[contenteditable='true'\]\s*{[^}]*user-select:\s*text/s
  );
});
//=============================================================================
