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
  initEventListeners,
  setRendererRef,
  setSvg,
  setRuntimeInput,
  setSimDt,
  setAutoRouteInput,
  setDiagramNameInput,
  setStatusEl,
  setGridSize,
} from './event-handlers.js';
//=============================================================================
describe('event-handlers setters invocation', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
  });

  test('calling exported setters and initEventListeners does not throw', () => {
    const mockRenderer = {
      current: {
        updateConnections: vi.fn(),
        forceFullRoute: vi.fn(),
        clearPending: vi.fn(),
        selectBlock: vi.fn(),
        selectConnection: vi.fn(),
        updateSelectionBox: vi.fn(),
        startMarqueeSelection: vi.fn(),
        updateBlockTransform: vi.fn(),
      },
    };
    setRendererRef(mockRenderer as any);

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('id', 'svg');
    document.body.appendChild(svg);

    const runtime = document.createElement('input');
    runtime.id = 'runtimeInput';
    document.body.appendChild(runtime);

    const sim = document.createElement('input');
    sim.id = 'simDt';
    document.body.appendChild(sim);

    const auto = document.createElement('input');
    auto.type = 'checkbox';
    auto.id = 'autoRouteInput';
    document.body.appendChild(auto);

    const diag = document.createElement('input');
    diag.id = 'diagramName';
    document.body.appendChild(diag);

    const status = document.createElement('div');
    status.id = 'status';
    document.body.appendChild(status);

    setSvg(svg as any);
    setRuntimeInput(runtime as any);
    setSimDt(sim as any);
    setAutoRouteInput(auto as any);
    setDiagramNameInput(diag as any);
    setStatusEl(status as any);
    setGridSize(30);

    // mock matchMedia used by collapseLibraryOnMobile
    (window as any).matchMedia = (query: string) => ({
      matches: false,
      addEventListener: () => {},
      removeEventListener: () => {},
      addListener: () => {},
      removeListener: () => {},
    });

    expect(() => initEventListeners()).not.toThrow();
  });
});
//=============================================================================
