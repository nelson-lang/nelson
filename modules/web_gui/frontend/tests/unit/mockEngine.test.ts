//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { MockEngine } from '../../src/rpc/mockEngine';
//=============================================================================
type Push = { channel: string; payload: unknown };
function makeEngine() {
  const pushes: Push[] = [];
  const engine = new MockEngine((channel, payload) => pushes.push({ channel, payload }));
  return { engine, pushes };
}
//=============================================================================
describe('MockEngine', () => {
  let engine: MockEngine;
  let pushes: Push[];
  beforeEach(() => {
    ({ engine, pushes } = makeEngine());
  });

  it('responds to ping', async () => {
    const r = (await engine.handle('ping', {})) as { pong: boolean; backend: string };
    expect(r.pong).toBe(true);
    expect(r.backend).toBe('mock');
  });

  it('evaluates a scalar into ans and echoes output', async () => {
    await engine.handle('term.input', { line: '42' });
    const out = pushes
      .filter((p) => p.channel === 'term.output')
      .map((p) => (p.payload as { text: string }).text)
      .join('');
    expect(out).toContain('42');
    expect(pushes.some((p) => p.channel === 'term.prompt')).toBe(true);
  });

  it('lists the seeded workspace variable A', async () => {
    const r = (await engine.handle('workspace.list', {})) as {
      variables: { name: string; className: string }[];
    };
    const a = r.variables.find((v) => v.name === 'A');
    expect(a).toBeTruthy();
    expect(a!.className).toBe('double');
  });

  it('offers completions for a prefix', async () => {
    const r = (await engine.handle('completion.request', { line: 'plo' })) as {
      prefix: string;
      builtin?: string[];
      macros?: string[];
    };
    const all = [...(r.builtin ?? []), ...(r.macros ?? [])];
    expect(all).toContain('plot');
  });

  it('controls profiler state through the dedicated RPC', async () => {
    const started = (await engine.handle('profile.control', { action: 'on' })) as {
      on: boolean;
    };
    expect(started.on).toBe(true);

    const stopped = (await engine.handle('profile.control', { action: 'off' })) as {
      on: boolean;
    };
    expect(stopped.on).toBe(false);
  });

  it('validates and changes the current directory with a native separator', async () => {
    const changed = (await engine.handle('fs.chdir', { path: '/workspace/examples' })) as {
      cwd: string;
      separator: string;
      recentDirectories: string[];
    };
    expect(changed).toEqual({
      cwd: '/workspace/examples',
      separator: '/',
      recentDirectories: ['/workspace/examples'],
    });
    const windows = (await engine.handle('fs.chdir', { path: 'C:\\workspace\\examples' })) as {
      cwd: string;
      separator: string;
      recentDirectories: string[];
    };
    expect(windows).toEqual({
      cwd: 'C:\\workspace\\examples',
      separator: '\\',
      recentDirectories: ['C:\\workspace\\examples', '/workspace/examples'],
    });
    await expect(engine.handle('fs.chdir', { path: 'relative/path' })).rejects.toThrow(
      'directory does not exist'
    );
  });

  it('tracks command-window directory changes in MRU order', async () => {
    await engine.handle('term.input', { line: "cd('/workspace/first')" });
    await engine.handle('term.input', { line: "cd('/workspace/second')" });
    await engine.handle('term.input', { line: "cd('/workspace/first')" });
    const current = (await engine.handle('fs.cwd', {})) as {
      cwd: string;
      recentDirectories: string[];
    };
    expect(current.cwd).toBe('/workspace/first');
    expect(current.recentDirectories).toEqual(['/workspace/first', '/workspace/second']);
  });

  it('provides backward-compatible dialog directory RPCs', async () => {
    await engine.handle('fs.chdir', { path: '/workspace/dialogs' });
    await expect(engine.handle('fs.dialog.start', { historyKey: 'desktop.open' })).resolves.toEqual(
      { directory: '/workspace/dialogs' }
    );
    await expect(
      engine.handle('fs.dialog.accept', {
        historyKey: 'desktop.open',
        mode: 'open',
        path: '/workspace/dialogs/example.m',
      })
    ).resolves.toEqual({ ok: true });
    await engine.handle('fs.chdir', { path: '/workspace/other' });
    await expect(
      engine.handle('fs.dialog.start', { historyKey: 'desktop.open', defaultPath: '.' })
    ).resolves.toEqual({ directory: '/workspace/dialogs' });
    await expect(
      engine.handle('fs.dialog.start', { historyKey: 'desktop.save', defaultPath: '.' })
    ).resolves.toEqual({ directory: '/workspace/other' });
  });

  it('reports that system file dialogs are unavailable in the browser mock', async () => {
    await expect(engine.handle('fs.dialog.native', { mode: 'open' })).resolves.toEqual({
      available: false,
      reason: 'not_webview',
    });
  });

  it('exports profiler data only when profiling is stopped', async () => {
    await engine.handle('profile.control', { action: 'on' });
    await expect(engine.handle('profile.export', {})).rejects.toThrow('profiler is running');

    await engine.handle('profile.control', { action: 'off' });
    const exported = (await engine.handle('profile.export', {})) as { index: string; url: string };
    expect(exported.index).toContain('profile_result/index.html');
    expect(exported.url).toContain('file://');
  });

  it('surf creates a figure flagged as a rotatable 3D axes with a View', async () => {
    await engine.handle('term.input', { line: 'surf(peaks)' });
    const list = (await engine.handle('figure.list', {})) as { figures: { handle: number }[] };
    expect(list.figures.length).toBeGreaterThan(0);
    const handle = list.figures[list.figures.length - 1].handle;
    const dl = (await engine.handle('figure.getDisplayList', { handle })) as {
      is3d?: boolean;
      view?: [number, number];
      primitives: unknown[];
      rotatableAxes?: { handle: number; rect: number[]; view: number[] }[];
    };
    expect(dl.is3d).toBe(true);
    expect(dl.view).toHaveLength(2);
    expect(dl.primitives.length).toBeGreaterThan(0);
    // A per-axes entry lets the client rotate the subplot under the cursor.
    expect(dl.rotatableAxes).toHaveLength(1);
    expect(dl.rotatableAxes?.[0].rect).toEqual([0, 0, 1, 1]);
    expect(dl.rotatableAxes?.[0].handle).toBeGreaterThan(0);
  });

  it('reports the desktop role (no standalone figure window in the mock)', async () => {
    await expect(engine.handle('client.role', {})).resolves.toEqual({
      role: 'desktop',
      launchMode: 'none',
    });
  });

  it('figure.list and figure.created report a windowStyle', async () => {
    await engine.handle('term.input', { line: 'surf(peaks)' });
    const list = (await engine.handle('figure.list', {})) as {
      figures: { handle: number; windowStyle?: string }[];
    };
    expect(list.figures.length).toBeGreaterThan(0);
    expect(list.figures[list.figures.length - 1].windowStyle).toBe('normal');
    const created = pushes.find((p) => p.channel === 'figure.created');
    expect((created?.payload as { windowStyle?: string }).windowStyle).toBe('normal');
  });

  it('figure.close destroys the figure so a later surf creates one again', async () => {
    await engine.handle('term.input', { line: 'surf(peaks)' });
    const before = (await engine.handle('figure.list', {})) as {
      figures: { handle: number }[];
    };
    const handle = before.figures[before.figures.length - 1].handle;

    await expect(engine.handle('figure.close', { handle })).resolves.toEqual({ ok: true, handle });
    await expect(engine.handle('figure.list', {})).resolves.toEqual({ figures: [] });
    expect(pushes).toContainEqual({ channel: 'figure.closed', payload: { handle } });

    await engine.handle('term.input', { line: 'surf(peaks)' });
    const after = (await engine.handle('figure.list', {})) as { figures: { handle: number }[] };
    expect(after.figures).toHaveLength(1);
  });

  it('restores a mock reference after compact rotation and keeps later state reads in sync', async () => {
    await engine.handle('term.input', { line: 'surf(peaks)' });
    const list = (await engine.handle('figure.list', {})) as { figures: { handle: number }[] };
    const handle = list.figures[0].handle;
    const initial = (await engine.handle('figure.getDisplayList', { handle })) as {
      view: number[];
    };
    const action = (request: Record<string, unknown>) =>
      engine.handle('figure.action', { handle, request });
    await engine.handle('figure.setView', { handle, azimuth: 45, elevation: 20 });
    await action({ action: 'restore' });
    const restored = (await action({ action: 'state' })) as {
      state: { axes: { View: number[] }[] };
    };
    expect(restored.state.axes[0].View).toEqual(initial.view);
    await action({ action: 'view', azimuth: 30, elevation: 10 });
    await action({ action: 'reference' });
    await action({ action: 'zoom', factor: 2 });
    await action({ action: 'properties', properties: { XGrid: 'on' } });
    const final = (await action({ action: 'restore' })) as {
      state: { axes: { View: number[]; XGrid: string; CameraViewAngle: number }[] };
    };
    expect(final.state.axes[0]).toMatchObject({ View: [30, 10], XGrid: 'on', CameraViewAngle: 10 });
  });

  it('figure.setView records the rotation and returns a fresh display list', async () => {
    await engine.handle('term.input', { line: 'surf(peaks)' });
    const list = (await engine.handle('figure.list', {})) as { figures: { handle: number }[] };
    const handle = list.figures[list.figures.length - 1].handle;
    const dl = (await engine.handle('figure.setView', { handle, azimuth: 45, elevation: 20 })) as {
      view?: [number, number];
    };
    expect(dl.view).toEqual([45, 20]);
    expect(engine.lastView).toEqual({ azimuth: 45, elevation: 20, axesHandle: 0 });
  });

  it('figure.setView records the targeted subplot axesHandle', async () => {
    await engine.handle('term.input', { line: 'surf(peaks)' });
    const list = (await engine.handle('figure.list', {})) as { figures: { handle: number }[] };
    const handle = list.figures[list.figures.length - 1].handle;
    await engine.handle('figure.setView', { handle, azimuth: 10, elevation: 5, axesHandle: 42 });
    expect(engine.lastView).toEqual({ azimuth: 10, elevation: 5, axesHandle: 42 });
  });

  it('routes figure key events only to an existing figure', async () => {
    await engine.handle('term.input', { line: 'surf(peaks)' });
    const list = (await engine.handle('figure.list', {})) as { figures: { handle: number }[] };
    const handle = list.figures[list.figures.length - 1].handle;
    const payload = {
      handle,
      pressed: true,
      character: 'w',
      key: 'W',
      shift: false,
      control: false,
      alt: false,
      meta: false,
    };

    await expect(engine.handle('figure.keyEvent', payload)).resolves.toEqual({ ok: true });
    expect(
      (window as unknown as { __mockLastFigureKeyEvent?: unknown }).__mockLastFigureKeyEvent
    ).toEqual(payload);
    await expect(engine.handle('figure.keyEvent', { ...payload, handle: -1 })).resolves.toEqual({
      ok: false,
    });
  });

  it('reads and writes a variable cell (round-trip persists)', async () => {
    await engine.handle('var.setCell', { name: 'A', row: 0, col: 0, value: 99 });
    const block = (await engine.handle('var.getBlock', {
      name: 'A',
      row0: 0,
      col0: 0,
      rows: 1,
      cols: 1,
    })) as {
      cells: number[][];
    };
    expect(block.cells[0][0]).toBe(99);
  });

  it('publishes the complete lifecycle of progress dialogs', async () => {
    await engine.handle('term.input', {
      line: "p = uiprogressdlg(1, 'Title', 'Import', 'Message', 'Reading', 'Value', 0.25)",
    });
    await Promise.resolve();

    let events = pushes.filter((p) => p.channel === 'dialog.progress');
    expect(events.at(-1)?.payload).toMatchObject({
      visible: true,
      title: 'Import',
      message: 'Reading',
      value: 25,
      minimum: 0,
      maximum: 100,
      indeterminate: false,
    });

    await engine.handle('term.input', { line: "set(p, 'value', 60)" });
    await engine.handle('term.input', { line: "set(p, 'labelText', 'Writing')" });
    await engine.handle('term.input', { line: "set(p, 'maximum', 0)" });
    await Promise.resolve();
    events = pushes.filter((p) => p.channel === 'dialog.progress');
    expect(events.at(-1)?.payload).toMatchObject({
      visible: true,
      message: 'Writing',
      value: 60,
      indeterminate: true,
    });

    await engine.handle('term.input', { line: 'delete(p)' });
    await Promise.resolve();
    events = pushes.filter((p) => p.channel === 'dialog.progress');
    expect(events.at(-1)?.payload).toMatchObject({ visible: false });
  });
});
//=============================================================================
vi.mock('../../src/rpc/client', () => ({}));
//=============================================================================
