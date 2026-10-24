//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { renderSubsystemBreadcrumb } from './subsystem-breadcrumb.js';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
test('renderSubsystemBreadcrumb renders hidden root state', () => {
  const container = document.createElement('div');

  renderSubsystemBreadcrumb(container, true, [], 'Root', vi.fn());

  const breadcrumb = container.querySelector('#subsystemBreadcrumb') as HTMLElement | null;
  expect(breadcrumb).toBeTruthy();
  expect(breadcrumb?.hidden).toBe(true);
  expect(container.querySelector('.breadcrumb-current')).toBeNull();
});
//=============================================================================
test('renderSubsystemBreadcrumb renders path and emits levelsUp', () => {
  const container = document.createElement('div');
  const onNavigate = vi.fn();

  renderSubsystemBreadcrumb(
    container,
    false,
    [
      { name: 'Root Diagram', levelsUp: 2 },
      { name: 'Motor', levelsUp: 1 },
    ],
    'Controller',
    onNavigate
  );

  const items = container.querySelectorAll('.breadcrumb-item');
  expect(items.length).toBe(2);
  expect(items[0].textContent).toBe('Root Diagram');
  expect(items[1].textContent).toBe('Motor');
  expect(container.querySelector('.breadcrumb-current')?.textContent).toBe('Controller');

  (items[1] as HTMLElement).click();
  expect(onNavigate).toHaveBeenCalledWith(1);
});
//=============================================================================
