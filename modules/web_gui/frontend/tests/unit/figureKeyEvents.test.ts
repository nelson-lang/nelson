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
import { figureKeyEventPayload } from '../../src/panels/figureKeyEvents';
//=============================================================================
function keyboardEvent(key: string, modifiers: Partial<KeyboardEvent> = {}): KeyboardEvent {
  return new KeyboardEvent('keydown', { key, ...modifiers });
}
//=============================================================================
describe('figure key events', () => {
  it('preserves printable characters and normalizes their key names', () => {
    expect(figureKeyEventPayload(12, keyboardEvent('w'), true)).toEqual({
      handle: 12,
      pressed: true,
      character: 'w',
      key: 'W',
      shift: false,
      control: false,
      alt: false,
      meta: false,
    });
  });

  it('normalizes named keys and forwards modifiers', () => {
    const payload = figureKeyEventPayload(
      4,
      keyboardEvent('ArrowLeft', { shiftKey: true, ctrlKey: true }),
      false
    );
    expect(payload).toMatchObject({
      handle: 4,
      pressed: false,
      character: '',
      key: 'Left',
      shift: true,
      control: true,
    });
  });
});
//=============================================================================
