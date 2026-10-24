//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as wp from './wheel-prevent';
//=============================================================================
test('wheel-prevent module loads', () => {
  expect(typeof wp).toBe('object');
});
//=============================================================================
// wheel-prevent attaches a global listener on import that prevents zooming
import '../../../dist/app/wheel-prevent.js';
//=============================================================================
describe('wheel-prevent listener', () => {
  test('prevents default when ctrlKey is true', () => {
    const ev = new WheelEvent('wheel', { ctrlKey: true, bubbles: true, cancelable: true });
    const prevented = !window.dispatchEvent(ev);
    // dispatchEvent returns false when preventDefault was called on a cancelable event
    expect(ev.defaultPrevented).toBe(true);
  });

  test('does not prevent default when ctrlKey is false', () => {
    const ev = new WheelEvent('wheel', { ctrlKey: false, bubbles: true, cancelable: true });
    window.dispatchEvent(ev);
    expect(ev.defaultPrevented).toBe(false);
  });
});
//=============================================================================
describe('wheel-prevent', () => {
  test('wheel listener prevents default when ctrlKey is set', async () => {
    await import('../../../dist/app/wheel-prevent.js').catch(() => null);
    const ev = new WheelEvent('wheel', { ctrlKey: true, bubbles: true, cancelable: true });
    window.dispatchEvent(ev);
    expect(ev.defaultPrevented).toBe(true);
  });

  test('wheel listener prevents default when metaKey is set (Mac)', async () => {
    await import('../../../dist/app/wheel-prevent.js').catch(() => null);
    const ev = new WheelEvent('wheel', { metaKey: true, bubbles: true, cancelable: true });
    window.dispatchEvent(ev);
    expect(ev.defaultPrevented).toBe(true);
  });
});
//=============================================================================
export {};
//=============================================================================
