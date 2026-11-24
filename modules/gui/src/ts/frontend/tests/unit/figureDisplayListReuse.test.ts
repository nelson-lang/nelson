//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import type { FigureDisplayListResult, UIControl } from '../../src/rpc/types';
import { reuseStableDisplayListParts } from '../../src/panels/figureDisplayListReuse';
//=============================================================================
function displayList(overrides: Partial<FigureDisplayListResult> = {}): FigureDisplayListResult {
  return {
    handle: 1,
    width: 640,
    height: 480,
    layoutSignature: 'layout-1',
    primitives: [],
    controls: [],
    ...overrides,
  };
}
//=============================================================================
function control(overrides: Partial<UIControl> = {}): UIControl {
  return {
    handle: 11,
    style: 'pushbutton',
    text: 'Run',
    x: 0,
    y: 0,
    w: 80,
    h: 24,
    value: 0,
    min: 0,
    max: 1,
    fontSize: 12,
    enabled: true,
    ...overrides,
  };
}
//=============================================================================
describe('reuseStableDisplayListParts', () => {
  it('reuses stable controls and menus when only animated pixels changed', () => {
    const controls = [control()];
    const menus = [{ handle: 22, text: 'Tools' }];
    const contextMenu = [{ handle: 33, text: 'Context' }];
    const current = displayList({
      controls,
      menus,
      contextMenu,
      controlLayoutWidth: 600,
      controlLayoutHeight: 400,
    });
    const next = displayList({
      displaySignature: 'new-pixels',
      controls: [],
      menus: [],
      contextMenu: [],
    });

    const reused = reuseStableDisplayListParts(current, next);

    expect(reused.controls).toBe(controls);
    expect(reused.menus).toBe(menus);
    expect(reused.contextMenu).toBe(contextMenu);
    expect(reused.controlLayoutWidth).toBe(600);
    expect(reused.controlLayoutHeight).toBe(400);
    expect(reused.displaySignature).toBe('new-pixels');
  });

  it('reuses stable controls and menus when compact animation frames omit them', () => {
    const controls = [control()];
    const menus = [{ handle: 22, text: 'Tools' }];
    const contextMenu = [{ handle: 33, text: 'Context' }];
    const current = displayList({
      controls,
      menus,
      contextMenu,
      controlLayoutWidth: 600,
      controlLayoutHeight: 400,
    });
    const next = displayList({ displaySignature: 'new-pixels' });
    delete next.controls;
    delete next.menus;
    delete next.contextMenu;

    const reused = reuseStableDisplayListParts(current, next);

    expect(reused.controls).toBe(controls);
    expect(reused.menus).toBe(menus);
    expect(reused.contextMenu).toBe(contextMenu);
    expect(reused.controlLayoutWidth).toBe(600);
    expect(reused.controlLayoutHeight).toBe(400);
    expect(reused.displaySignature).toBe('new-pixels');
  });

  it('keeps new layout data when the layout signature changes', () => {
    const nextControls = [control({ handle: 12, text: 'Stop' })];
    const reused = reuseStableDisplayListParts(
      displayList({ layoutSignature: 'layout-1' }),
      displayList({
        layoutSignature: 'layout-2',
        controls: nextControls,
      })
    );

    expect(reused.controls).toBe(nextControls);
  });
});
//=============================================================================
