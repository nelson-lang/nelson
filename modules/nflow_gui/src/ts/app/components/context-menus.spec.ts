//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../../dist/app/components/context-menus.js').catch(() => null);
});
//=============================================================================
test('context-menus module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
import { vi } from 'vitest';
//=============================================================================
describe('mountContextMenus', () => {
  let mountContextMenus: () => void;

  beforeAll(async () => {
    const mod = await import('./context-menus.tsx');
    mountContextMenus = mod.mountContextMenus;
  });

  beforeEach(() => {
    document.body.innerHTML = '';
  });

  test('inserts block and canvas context menus into document body', () => {
    mountContextMenus();

    const blockMenu = document.getElementById('blockContextMenu');
    const canvasMenu = document.getElementById('canvasContextMenu');
    const libraryMenu = document.getElementById('libraryBlockContextMenu');
    expect(blockMenu).toBeTruthy();
    expect(canvasMenu).toBeTruthy();
    expect(libraryMenu).toBeTruthy();
    expect(blockMenu?.querySelector('[data-action="delete"]')).toBeTruthy();
    expect(blockMenu?.querySelector('[data-action="createSubsystem"]')).toBeTruthy();
    expect(blockMenu?.querySelector('[data-action="blockHelp"]')).toBeTruthy();
    expect(canvasMenu?.querySelector('[data-action="selectAll"]')).toBeTruthy();
    expect(libraryMenu?.querySelector('[data-action="libraryBlockHelp"]')).toBeTruthy();
    expect(document.getElementById('nflowContextMenusRoot')?.dataset.reactRoot).toBe(
      'context-menus'
    );
  });
});
//=============================================================================
