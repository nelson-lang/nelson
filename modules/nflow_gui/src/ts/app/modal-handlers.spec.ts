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
// Mock i18n to avoid runtime import.meta usage inside the real module during tests
vi.mock('../utils/i18n.js', () => ({
  t: (s: any, opts?: any) => {
    let out = String(s || '');
    if (opts && typeof opts === 'object') {
      Object.entries(opts).forEach(([k, v]) => {
        out = out.replace(new RegExp(`\{${k}\}`, 'g'), String(v));
      });
    }
    return out;
  },
  localizeDOM: (_doc: any) => {},
}));
//=============================================================================
let mod;
let stateMod;
beforeAll(async () => {
  window.alert = vi.fn();
  window.focus = vi.fn();
  URL.createObjectURL = vi.fn(() => 'blob:fake');
  URL.revokeObjectURL = vi.fn();
  mod = await import('./modal-handlers.js');
  stateMod = await import('./state.js');
});
//=============================================================================
afterEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
describe('modal-handlers - setStatusEl', () => {
  test('setter does not throw', () => {
    expect(() => mod.setStatusEl(document.createElement('div'))).not.toThrow();
  });
});
//=============================================================================
describe('modal-handlers - showConfirmSaveModal', () => {
  function buildConfirmDOM() {
    document.body.innerHTML = `
      <div id="confirmSaveModal">
        <button id="saveConfirm">Save</button>
        <button id="dontSaveConfirm">Don't Save</button>
        <button id="closeConfirmSaveModal">Close</button>
      </div>
    `;
  }

  test('uses React event bridge when a listener handles the confirm request', () => {
    buildConfirmDOM();
    const cb = vi.fn();
    document.addEventListener(
      'nflow:confirm-save',
      (event) => {
        event.preventDefault();
        (event as CustomEvent<{ callback: (result: boolean | null) => void }>).detail.callback(
          true
        );
      },
      { once: true }
    );

    mod.showConfirmSaveModal(cb);

    expect(cb).toHaveBeenCalledWith(true);
    expect(document.getElementById('confirmSaveModal')!.classList.contains('show')).toBe(false);
  });

  test('invokes callback with false when React event bridge is not mounted', () => {
    document.body.innerHTML = '';
    const cb = vi.fn();
    mod.showConfirmSaveModal(cb);
    expect(cb).toHaveBeenCalledWith(false);
  });
});
//=============================================================================
describe('modal-handlers - handleSaveAsSubsystem', () => {
  test('sets status when no external ports found', () => {
    const statusEl = document.createElement('div');
    mod.setStatusEl(statusEl);
    stateMod.state.blocks = new Map([['b1', { id: 'b1', type: 'constant', params: {} }]]);
    mod.handleSaveAsSubsystem();
    expect(statusEl.textContent).toContain('Cannot save as subsystem');
  });
  test('saves subsystem via fallback download when external ports exist and modal absent', async () => {
    const statusEl = document.createElement('div');
    mod.setStatusEl(statusEl);
    stateMod.state.blocks = new Map([
      [
        'b1',
        { id: 'b1', type: FieldNames.kLabelSource, params: { isExternalPort: true, label: 'In1' } },
      ],
    ]);
    stateMod.state.diagramName = 'TestSub';
    document.body.innerHTML = '';
    (URL.createObjectURL as unknown as vi.Mock) = vi.fn(() => 'blob:fake');
    mod.handleSaveAsSubsystem();
    await Promise.resolve();
    expect(URL.createObjectURL).toHaveBeenCalled();
    expect(statusEl.textContent).toContain('Saved subsystem: TestSub');
  });

  test('saves subsystem when external ports exist', async () => {
    const statusEl = document.createElement('div');
    mod.setStatusEl(statusEl);
    stateMod.state.blocks = new Map([
      [
        'b1',
        { id: 'b1', type: FieldNames.kLabelSource, params: { isExternalPort: true, label: 'In1' } },
      ],
    ]);
    stateMod.state.diagramName = 'TestSub';
    (URL.createObjectURL as unknown as vi.Mock) = vi.fn(() => 'blob:fake');
    mod.handleSaveAsSubsystem();
    await Promise.resolve();
    expect(URL.createObjectURL).toHaveBeenCalled();
    expect(statusEl.textContent).toContain('Saved subsystem: TestSub');
  });
});
//=============================================================================
describe('modal-handlers - showMissingBlocksModal', () => {
  test('uses React event bridge when a listener handles missing blocks', () => {
    document.body.innerHTML = '';
    document.addEventListener(
      'nflow:missing-blocks',
      (event) => {
        event.preventDefault();
        const detail = (event as CustomEvent<{ missingBlocks: Array<{ type: string }> }>).detail;
        expect(detail.missingBlocks[0].type).toBe('blockA');
      },
      { once: true }
    );

    mod.showMissingBlocksModal([{ type: 'blockA' }]);

    expect(window.alert).not.toHaveBeenCalled();
  });

  test('uses alert fallback when modal not present', () => {
    document.body.innerHTML = '';
    mod.showMissingBlocksModal([{ type: 'unknownBlock' }]);
    expect(window.alert).toHaveBeenCalled();
  });

  test('uses alert fallback when React event bridge is not mounted', () => {
    document.body.innerHTML = `
      <div id="missingBlocksModal">
        <ul id="missingBlocksList"></ul>
        <button id="closeMissingBlocksBtn">Close</button>
        <button id="closeMissingBlocksModal">X</button>
      </div>
    `;
    mod.showMissingBlocksModal([{ type: 'blockA', libraryTitle: 'Library A' }, { type: 'blockB' }]);
    expect(window.alert).toHaveBeenCalled();
    expect(document.getElementById('missingBlocksList')!.children.length).toBe(0);
    expect(document.getElementById('missingBlocksModal')!.classList.contains('show')).toBe(false);
  });
});
//=============================================================================
