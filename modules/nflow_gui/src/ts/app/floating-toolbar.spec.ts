//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
//=============================================================================
describe('floatingToolbar', () => {
  let initFloatingToolbar: () => void;

  beforeAll(async () => {
    let mod: any = null;
    try {
      mod = await import('./floating-toolbar.ts');
    } catch (e) {
      // try compiled JS fallback (some runners resolve to the JS file)
      try {
        mod = await import('../../dist/app/floating-toolbar.js');
      } catch (e2) {
        // ignore - we'll try other fallbacks below
      }
    }

    initFloatingToolbar =
      (mod && (mod.initFloatingToolbar || (mod.default && mod.default.initFloatingToolbar))) ??
      (globalThis as any).initFloatingToolbar;

    // As a last resort, load and eval the compiled JS so it registers a global.
    if (typeof initFloatingToolbar !== 'function') {
      try {
        const fs = await import('fs');
        const p = new URL('../../dist/app/floating-toolbar.js', import.meta.url);
        const src = fs.promises
          ? await fs.promises.readFile(p, 'utf8')
          : fs.readFileSync(p, 'utf8');
        // evaluate in the current test VM so it can access document/window
        // eslint-disable-next-line no-eval
        (0, eval)(src);
        initFloatingToolbar = (globalThis as any).initFloatingToolbar;
      } catch (e) {
        // swallow - the tests will fail with clearer error below
      }
    }
  });

  beforeEach(() => {
    document.body.innerHTML = '';
  });

  test('keeps the command bar visible and enables Run when a block is added', async () => {
    const toolbar = document.createElement('div');
    toolbar.id = 'floatingToolbar';
    document.body.appendChild(toolbar);

    const blockLayer = document.createElement('div');
    blockLayer.id = 'blockLayer';
    document.body.appendChild(blockLayer);

    initFloatingToolbar();

    expect(toolbar.style.display).toBe('');
    expect(toolbar.hidden).toBe(false);
    expect((document.getElementById('playPauseBtn') as HTMLButtonElement).disabled).toBe(true);

    const node = document.createElement('div');
    blockLayer.appendChild(node);
    await new Promise((r) => setTimeout(r, 0));

    expect(toolbar.style.display).toBe('');
    expect(toolbar.hidden).toBe(false);
    expect((document.getElementById('playPauseBtn') as HTMLButtonElement).disabled).toBe(false);
  });

  test('reattaches visibility observer when block layer is mounted after first init', async () => {
    const toolbar = document.createElement('div');
    toolbar.id = 'floatingToolbar';
    document.body.appendChild(toolbar);

    initFloatingToolbar();
    expect(toolbar.style.display).toBe('');

    const blockLayer = document.createElement('div');
    blockLayer.id = 'blockLayer';
    document.body.appendChild(blockLayer);
    initFloatingToolbar();

    blockLayer.appendChild(document.createElement('div'));
    await new Promise((r) => setTimeout(r, 0));

    expect(toolbar.style.display).toBe('');
  });

  test('keeps the command bar hidden while availability changes', async () => {
    const toolbar = document.createElement('div');
    toolbar.id = 'floatingToolbar';
    toolbar.hidden = true;
    toolbar.style.display = 'none';
    document.body.appendChild(toolbar);

    const blockLayer = document.createElement('div');
    blockLayer.id = 'blockLayer';
    document.body.appendChild(blockLayer);

    initFloatingToolbar();
    blockLayer.appendChild(document.createElement('div'));
    await new Promise((r) => setTimeout(r, 0));

    expect(toolbar.hidden).toBe(true);
    expect(toolbar.style.display).toBe('');
  });

  test('run button click dispatches play event and marks pending', async () => {
    const toolbar = document.createElement('div');
    toolbar.id = 'floatingToolbar';
    document.body.appendChild(toolbar);

    const runBtn = document.createElement('button');
    runBtn.id = 'playPauseBtn';
    toolbar.appendChild(runBtn);

    initFloatingToolbar();

    const blockLayer = document.createElement('div');
    blockLayer.id = 'blockLayer';
    blockLayer.appendChild(document.createElement('div'));
    document.body.appendChild(blockLayer);
    initFloatingToolbar();

    const renderedRunBtn = document.getElementById('playPauseBtn') as HTMLButtonElement;
    renderedRunBtn.click();
    await new Promise((r) => setTimeout(r, 0));

    expect(toolbar.classList.contains('pending')).toBe(true);
    expect(renderedRunBtn.getAttribute('data-action')).toBe('run');
  });

  test('renders target time, progress, and opens simulation settings', () => {
    document.body.innerHTML = `
      <input id="runtimeInput" value="12" />
      <div id="floatingToolbar"></div>
      <div id="blockLayer"><div></div></div>`;
    initFloatingToolbar();

    document.dispatchEvent(
      new CustomEvent('nf-sim-time', { detail: { time: 3, progress: 25, done: false } })
    );
    expect(document.getElementById('simTimeDisplay')?.textContent).toContain('3.00 / 12.00 s');
    expect(
      document.querySelector('.simulation-progress-track')?.getAttribute('aria-valuenow')
    ).toBe('25');

    let tab = '';
    document.addEventListener(
      'nf-inspector-show',
      (event) => (tab = (event as CustomEvent).detail?.tab),
      { once: true }
    );
    document.getElementById('simSettingsToolbarBtn')?.click();
    expect(tab).toBe('simulation');
  });

  test('a paused run shows Run but keeps Stop enabled until the run stops', () => {
    document.body.innerHTML = `
      <div id="floatingToolbar"></div>
      <div id="blockLayer"><div></div></div>`;
    initFloatingToolbar();
    const stopBtn = () =>
      document.querySelector('#floatingToolbar [data-action="stop"]') as HTMLButtonElement;
    const runBtn = () => document.getElementById('playPauseBtn') as HTMLButtonElement;

    document.dispatchEvent(new CustomEvent('nf-sim-start'));
    expect(runBtn().getAttribute('data-action')).toBe('pause');

    document.dispatchEvent(
      new CustomEvent('nf-sim-state', { detail: { running: false, phase: 'paused' } })
    );
    expect(runBtn().getAttribute('data-action')).toBe('run');
    expect(stopBtn().disabled).toBe(false);

    document.dispatchEvent(
      new CustomEvent('nf-sim-state', { detail: { running: false, phase: 'idle' } })
    );
    expect(stopBtn().disabled).toBe(true);
  });
});
//=============================================================================
