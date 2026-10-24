//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
//=============================================================================
beforeAll(async () => {
  await vi.doMock('./diagram-handlers', () => {
    let _rendererRef: unknown = null;
    return {
      loadDiagram: async (d: any) => {
        const s = await import('./state.js');
        s.state.blocks.clear();
        if (Array.isArray(d.blocks)) {
          for (const b of d.blocks) {
            s.state.blocks.set(b.id, { ...b });
          }
        }
        s.state.connections = Array.isArray(d.connections) ? d.connections.slice() : [];
        s.state.diagramName = String(d.name || s.state.diagramName);
      },
      setRendererRef: (r: unknown) => {
        _rendererRef = r;
      },
      getRendererRef: () => _rendererRef,
      getStatusEl: () => null,
      getRuntimeInput: () => null,
      connectionKey: (c: any) =>
        `${String(c.from)}:${Number(c.fromIndex ?? 0)}->${String(c.to)}:${Number(c.toIndex ?? 0)}`,
      updateSubsystemNavUi: () => {},
    } as unknown as Record<string, unknown>;
  });
});
//=============================================================================
describe('history integration: createSubsystemFromSelection undo/redo', () => {
  test('creating a subsystem is undoable and redoable', async () => {
    const hist = await import('./history.js');
    const m = await import('./subsystem-handlers.js');
    const dh = await import('./diagram-handlers.js');
    const s = await import('./state.js');

    // Prepare a renderer that actually mutates `state` so snapshots reflect changes
    let nextId = 4000;
    const renderer = {
      current: {
        createBlock: (type: string, x: number, y: number, opts?: Record<string, unknown>) => {
          const id = nextId++;
          const block: Record<string, unknown> = {
            id,
            type,
            x: Number(x) || 0,
            y: Number(y) || 0,
            params: opts?.params || {},
            width: opts && (opts as any).width ? (opts as any).width : 60,
            height: opts && (opts as any).height ? (opts as any).height : 40,
          };
          s.state.blocks.set(id as number, block as any);
          return { id } as unknown;
        },
        deleteBlock: (id: number | string) => {
          s.state.blocks.delete(Number(id));
          (s.state.connections as Array<Record<string, unknown>>) = (
            s.state.connections as Array<Record<string, unknown>>
          ).filter((c) => String(c.from) !== String(id) && String(c.to) !== String(id));
        },
        createConnection: (from: unknown, to: unknown, toIndex?: number, fromIndex?: number) => {
          (s.state.connections as Array<Record<string, unknown>>).push({
            from,
            to,
            toIndex: Number(toIndex ?? 0),
            fromIndex: Number(fromIndex ?? 0),
          });
          return {};
        },
        updateSelectionBox: () => {},
      },
    } as unknown as { current: unknown };

    (dh as any).setRendererRef(renderer);
    hist.initHistory({ renderer } as any);

    s.state.blocks.clear();
    (s.state.connections as Array<Record<string, unknown>>) = [];
    s.state.blocks.set(
      1 as any,
      { id: 1, type: 'gain', x: 0, y: 50, width: 60, height: 40, params: {} } as any
    );
    s.state.blocks.set(
      2 as any,
      { id: 2, type: 'gain', x: 200, y: 50, width: 60, height: 40, params: {} } as any
    );
    (s.state.connections as Array<Record<string, unknown>>) = [{ from: 1, to: 2 } as any];
    s.state.selectedIds = new Set([1, 2] as any);

    expect(s.state.blocks.has(1)).toBe(true);
    expect(s.state.blocks.has(2)).toBe(true);

    (m as any).createSubsystemFromSelection();

    expect(s.state.blocks.has(1)).toBe(false);
    expect(s.state.blocks.has(2)).toBe(false);
    const subs = Array.from(s.state.blocks.values()).find((b: any) => b.type === 'subsystem');
    expect(subs).toBeDefined();

    const okUndo = await hist.undo();
    expect(okUndo).toBe(true);
    expect(s.state.blocks.has(1)).toBe(true);
    expect(s.state.blocks.has(2)).toBe(true);
    const stillHasSubsystem = Array.from(s.state.blocks.values()).some(
      (b: any) => b.type === 'subsystem'
    );
    expect(stillHasSubsystem).toBe(false);

    const okRedo = await hist.redo();
    expect(okRedo).toBe(true);
    expect(s.state.blocks.has(1)).toBe(false);
    expect(s.state.blocks.has(2)).toBe(false);
    const subs2 = Array.from(s.state.blocks.values()).find((b: any) => b.type === 'subsystem');
    expect(subs2).toBeDefined();
  });
});
//=============================================================================
