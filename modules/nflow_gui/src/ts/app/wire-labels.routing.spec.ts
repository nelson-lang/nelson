//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { beforeEach, expect, test, vi } from 'vitest';
import { initWireLabels, refreshWireLabels } from './wire-labels.js';
import { state } from './state.js';
import { makeWireApplyHandlers } from '../render/wire/renderer-wire-apply.js';

beforeEach(() => {
  document.body.innerHTML = '<svg id="svgCanvas"><g id="overlayLayer"></g></svg>';
  state.blocks.clear();
  state.connections = [];
  state.selectedConnection = null;
  state.autoRoute = false;
  state.fastRouting = false;
  state.dirty = false;
});

test.each(['full', 'fast'])(
  '%s routing displays saved labels without a click or dirty event',
  (mode) => {
    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    document.querySelector('svg')!.appendChild(path);
    const conn = { from: 'a', to: 'b', label: 'altitude', points: [], path };
    state.connections = [conn];
    initWireLabels(document.querySelector('svg'));
    refreshWireLabels();
    expect(document.querySelector('.wire-label')).toBeNull();
    const changed = vi.fn();
    window.addEventListener('diagramChanged', changed);
    try {
      const renderer = makeWireApplyHandlers({
        state,
        buildFallbackPathFromPorts: () => [
          { x: 0, y: 0 },
          { x: 100, y: 0 },
        ],
        buildDragRenderPoints: (_c: any, p: any) => p,
        applyWireOffsets: (_c: any, p: any) => p,
        simplifyOrthogonalPath: (p: any) => p,
        dedupePoints: (p: any) => p,
        tryShorterOrthogonalPath: (_c: any, p: any) => p,
        renderSharedSourceJunctionDots: () => {},
      } as any);
      if (mode === 'full') renderer.applyWirePaths(null);
      else renderer.applyWirePathsFast(new Map([[conn, conn]]));
      expect(path.getAttribute('d')).toContain('100');
      expect(document.querySelector('.wire-label')?.textContent).toBe('altitude');
      expect(changed).not.toHaveBeenCalled();
      expect(state.dirty).toBe(false);
      expect(state.selectedConnection).toBeNull();
    } finally {
      window.removeEventListener('diagramChanged', changed);
    }
  }
);
