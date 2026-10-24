//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { CSSProperties } from 'react';
import type { UIControl } from '../rpc/types';
//=============================================================================
// Where a pushbutton / togglebutton puts its label: HorizontalAlignment and
// VerticalAlignment of uibutton, the state button and uitogglebutton. The same
// placement serves the live HTML button and the static SVG export.
export const BUTTON_LABEL_MARGIN = 4;
// Multi-line text keeps its line breaks.
export function buttonLabelPlacement(c: UIControl): CSSProperties {
  const justify = c.hAlign === 'left' ? 'flex-start' : c.hAlign === 'right' ? 'flex-end' : 'center';
  const align = c.vAlign === 'top' ? 'flex-start' : c.vAlign === 'bottom' ? 'flex-end' : 'center';
  return {
    display: 'flex',
    justifyContent: justify,
    alignItems: align,
    textAlign: c.hAlign === 'left' || c.hAlign === 'right' ? c.hAlign : 'center',
    whiteSpace: 'pre-line',
    padding: BUTTON_LABEL_MARGIN,
  };
}
//=============================================================================
// Anchor of the label in the static SVG: x with its text-anchor, y of the
// first line's center, for `lines` lines of `lineHeight` pixels.
export function buttonLabelAnchor(
  c: UIControl,
  lines: number,
  lineHeight: number
): { x: number; anchor: string; y: number } {
  const x =
    c.hAlign === 'left'
      ? c.x + BUTTON_LABEL_MARGIN
      : c.hAlign === 'right'
      ? c.x + c.w - BUTTON_LABEL_MARGIN
      : c.x + c.w / 2;
  const anchor = c.hAlign === 'left' ? 'start' : c.hAlign === 'right' ? 'end' : 'middle';
  const block = lines * lineHeight;
  const top =
    c.vAlign === 'top'
      ? c.y + BUTTON_LABEL_MARGIN
      : c.vAlign === 'bottom'
      ? c.y + c.h - BUTTON_LABEL_MARGIN - block
      : c.y + (c.h - block) / 2;
  return { x, anchor, y: top + lineHeight / 2 };
}
//=============================================================================
