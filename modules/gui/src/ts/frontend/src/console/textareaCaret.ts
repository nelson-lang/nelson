//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type ViewportPoint = { left: number; top: number };
const MIRRORED_PROPERTIES = [
  'borderBottomWidth',
  'borderLeftWidth',
  'borderRightWidth',
  'borderTopWidth',
  'boxSizing',
  'fontFamily',
  'fontSize',
  'fontStyle',
  'fontVariant',
  'fontWeight',
  'letterSpacing',
  'lineHeight',
  'paddingBottom',
  'paddingLeft',
  'paddingRight',
  'paddingTop',
  'tabSize',
  'textAlign',
  'textIndent',
  'textTransform',
] as const;
//=============================================================================
/** Return the viewport position of a character boundary inside a textarea. */
export function textareaCaretPosition(
  textarea: HTMLTextAreaElement,
  offset: number
): ViewportPoint {
  const style = window.getComputedStyle(textarea);
  const bounds = textarea.getBoundingClientRect();
  const mirror = document.createElement('div');
  const marker = document.createElement('span');

  mirror.setAttribute('aria-hidden', 'true');
  mirror.style.position = 'fixed';
  mirror.style.visibility = 'hidden';
  mirror.style.pointerEvents = 'none';
  mirror.style.overflow = 'hidden';
  mirror.style.left = `${bounds.left}px`;
  mirror.style.top = `${bounds.top}px`;
  mirror.style.width = `${bounds.width}px`;
  mirror.style.height = `${bounds.height}px`;
  mirror.style.whiteSpace = textarea.wrap === 'off' ? 'pre' : 'pre-wrap';
  mirror.style.overflowWrap = textarea.wrap === 'off' ? 'normal' : 'break-word';
  for (const property of MIRRORED_PROPERTIES) mirror.style[property] = style[property];

  const safeOffset = Math.max(0, Math.min(offset, textarea.value.length));
  mirror.textContent = textarea.value.slice(0, safeOffset);
  marker.textContent = '\u200b';
  mirror.append(marker);
  document.body.append(mirror);
  mirror.scrollLeft = textarea.scrollLeft;
  mirror.scrollTop = textarea.scrollTop;
  const markerBounds = marker.getBoundingClientRect();
  mirror.remove();

  return { left: markerBounds.left, top: markerBounds.top };
}
//=============================================================================
