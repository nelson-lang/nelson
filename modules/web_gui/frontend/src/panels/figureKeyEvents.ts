//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type FigureKeyEventPayload = {
  handle: number;
  pressed: boolean;
  character: string;
  key: string;
  shift: boolean;
  control: boolean;
  alt: boolean;
  meta: boolean;
};
//=============================================================================
const namedKeys: Record<string, string> = {
  ArrowDown: 'Down',
  ArrowLeft: 'Left',
  ArrowRight: 'Right',
  ArrowUp: 'Up',
  Escape: 'Esc',
  Enter: 'Return',
  ' ': 'Space',
};
//=============================================================================
export function figureKeyEventPayload(
  handle: number,
  event: Pick<KeyboardEvent, 'key' | 'shiftKey' | 'ctrlKey' | 'altKey' | 'metaKey'>,
  pressed: boolean
): FigureKeyEventPayload {
  const character = event.key.length === 1 ? event.key : '';
  const key = namedKeys[event.key] ?? (character ? character.toUpperCase() : event.key);
  return {
    handle,
    pressed,
    character,
    key,
    shift: event.shiftKey,
    control: event.ctrlKey,
    alt: event.altKey,
    meta: event.metaKey,
  };
}
//=============================================================================
