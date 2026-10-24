//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { createElement } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
//=============================================================================
// Monaco cannot mount in jsdom; stub the editor and its setup.
vi.mock('@monaco-editor/react', () => ({ __esModule: true, default: () => null }));
vi.mock('../../src/editor/monacoSetup', () => ({ setupMonaco: () => {} }));
// The interactive figure panel pulls in the whole figure stack; stub it.
vi.mock('../../src/panels/FigurePanel', () => ({
  FigurePanel: ({ reloadToken }: { reloadToken?: string | number }) =>
    createElement('div', {
      className: 'test-figure-panel',
      'data-reload-token': String(reloadToken),
    }),
}));
import { CodeCell } from '../../src/notebook/blocks/CodeCell';
import { NotebookContext, type NotebookCtx } from '../../src/notebook/context';
import { markFiguresLive } from '../../src/notebook/liveFigures';
//=============================================================================
const baseCtx = (over: Partial<NotebookCtx> = {}): NotebookCtx => ({
  runAll: async () => {},
  runBlock: async () => {},
  runToHere: async () => {},
  runSection: async () => {},
  runReactive: async () => {},
  running: false,
  runningBlockId: null,
  hideCode: false,
  outputLayout: 'inline',
  dark: true,
  ...over,
});
//=============================================================================
let host: HTMLDivElement, root: Root;
beforeEach(() => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  host = document.createElement('div');
  document.body.append(host);
  root = createRoot(host);
});
//=============================================================================
afterEach(() => {
  act(() => root.unmount());
  host.remove();
  vi.unstubAllGlobals();
});
//=============================================================================
const render = (ctx: NotebookCtx, block: any, editor: any) =>
  act(() =>
    root.render(
      <NotebookContext.Provider value={ctx}>
        <CodeCell block={block} editor={editor} />
      </NotebookContext.Provider>
    )
  );
//=============================================================================
const cellWithOutput = () => ({
  id: 'c2',
  props: { code: 'x = 1', output: 'ans = 1', figures: '[]', errorMsg: '', stale: false },
});
//=============================================================================
it('marks the cell as running when it is the active one', () => {
  render(baseCtx({ runningBlockId: 'c2' }), cellWithOutput(), { updateBlock: vi.fn() });
  expect(host.querySelector('.nls-codecell')?.getAttribute('data-running')).toBe('true');
});
//=============================================================================
it('does not mark the cell running when another cell is active', () => {
  render(baseCtx({ runningBlockId: 'c1' }), cellWithOutput(), { updateBlock: vi.fn() });
  expect(host.querySelector('.nls-codecell')?.getAttribute('data-running')).toBe('false');
});
//=============================================================================
it('shows a Clear output button that clears only this cell', () => {
  const editor = { updateBlock: vi.fn() };
  const block = cellWithOutput();
  render(baseCtx(), block, editor);
  const clear = host.querySelector<HTMLButtonElement>('.nls-clear-output');
  expect(clear).not.toBeNull();
  act(() => clear!.dispatchEvent(new MouseEvent('click', { bubbles: true })));
  expect(editor.updateBlock).toHaveBeenCalledWith(block, {
    props: { output: '', figures: '[]', figureHandles: '[]', errorMsg: '' },
  });
});
//=============================================================================
it('reflects the output layout on the cell (below vs right)', () => {
  render(baseCtx({ outputLayout: 'right' }), cellWithOutput(), { updateBlock: vi.fn() });
  expect(host.querySelector('.nls-codecell')?.getAttribute('data-layout')).toBe('right');
});
//=============================================================================
it('renders the interactive figure panel for a live handle and a PNG otherwise', () => {
  markFiguresLive([77]);
  const block = {
    id: 'c5',
    props: {
      code: 'plot(x)',
      output: '',
      errorMsg: '',
      figures: JSON.stringify(['data:image/png;base64,QUJD', 'data:image/png;base64,REST']),
      figureHandles: JSON.stringify([77, -1]),
    },
  };
  render(baseCtx(), block, { updateBlock: vi.fn() });
  // handle 77 is live -> interactive panel wrapper; -1 -> PNG fallback image
  expect(host.querySelector('.nls-figure-live')).not.toBeNull();
  expect(host.querySelector('img.nls-figure')).not.toBeNull();
});
//=============================================================================
it('uses the session run revision to refresh a reused live figure', () => {
  markFiguresLive([77]);
  const block = {
    id: 'c6',
    props: {
      code: 'plot(x)',
      output: '',
      errorMsg: '',
      figures: JSON.stringify(['data:image/png;base64,QUJD']),
      figureHandles: JSON.stringify([77]),
      runRevision: 3,
    },
  };
  render(baseCtx(), block, { updateBlock: vi.fn() });
  expect(host.querySelector('.test-figure-panel')?.getAttribute('data-reload-token')).toBe('3');
});
//=============================================================================
it('hides the Clear output button when the cell has no result', () => {
  render(
    baseCtx(),
    { id: 'c3', props: { code: 'x = 1', output: '', figures: '[]', errorMsg: '' } },
    {
      updateBlock: vi.fn(),
    }
  );
  expect(host.querySelector('.nls-clear-output')).toBeNull();
});
//=============================================================================
