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
import { renderOpenRecentMenu } from './open-recent-menu.js';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
test('renderOpenRecentMenu renders an empty state and clear action', () => {
  const container = document.createElement('div');
  const onAction = vi.fn();

  renderOpenRecentMenu(container, [], onAction);

  expect(container.textContent).toContain('No recently opened files.');
  expect(container.querySelector('[data-action="clearRecent"]')).toBeTruthy();
});
//=============================================================================
test('renderOpenRecentMenu renders display names and emits encoded actions', () => {
  const container = document.createElement('div');
  const onAction = vi.fn();

  renderOpenRecentMenu(container, ['/tmp/demo.nflow'], onAction);

  const item = container.querySelector('[data-action^="openRecent:"]') as HTMLElement;
  expect(item).toBeTruthy();
  expect(item.textContent).toBe('demo.nflow');
  expect(item.querySelector('span')?.getAttribute('title')).toBe('/tmp/demo.nflow');

  item.click();
  expect(onAction).toHaveBeenCalledWith(`openRecent:${encodeURIComponent('/tmp/demo.nflow')}`);
});
//=============================================================================
