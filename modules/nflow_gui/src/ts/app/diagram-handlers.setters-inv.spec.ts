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
import {
  setRendererRef,
  setSubsystemUpBtn,
  updateSubsystemNavUi,
  loadDiagram,
} from './diagram-handlers.js';
import { state } from './state.js';
//=============================================================================
describe('diagram-handlers setters invocation', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    state.blocks.clear();
    state.connections = [];
    state.subsystemStack = [];
  });

  test('setRendererRef and loadDiagram invoke renderer.createBlock', async () => {
    const mockCreateBlock = vi.fn(() => ({ id: 123 }));
    const mockRenderer = {
      current: {
        createBlock: mockCreateBlock,
        updateBlockLabel: vi.fn(),
        clearWorkspace: vi.fn(),
        updateConnections: vi.fn(),
      },
    };
    setRendererRef(mockRenderer as any);

    const btn = document.createElement('button');
    setSubsystemUpBtn(btn);
    updateSubsystemNavUi();
    expect(btn.hidden).toBe(true);

    const data = { name: 'test', blocks: [], connections: [] };
    await loadDiagram(data as any);
    expect(mockRenderer.current.clearWorkspace).toHaveBeenCalled();
    expect(state.diagramName).toBe('test');
  });
});
//=============================================================================
