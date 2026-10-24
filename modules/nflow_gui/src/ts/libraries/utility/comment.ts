//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { UiBlock, Helpers } from '../types.js';
//=============================================================================
export function comment(block: UiBlock, helpers: Helpers): void {
  const { createSvgElement } = helpers;
  const group = block.group as Element & { appendChild: (child: Node) => void };
  const body = createSvgElement!('rect', {
    x: 0,
    y: 0,
    width: block.width,
    height: block.height,
    class: 'nf-block comment-body',
  });
  group.appendChild(body);
  block.commentBody = body;
  const textPadding = 10;
  const foreign = createSvgElement('foreignObject', {
    x: textPadding,
    y: textPadding,
    width: Math.max(1, block.width - textPadding * 2),
    height: Math.max(1, block.height - textPadding * 2),
    class: 'comment-foreign upright',
  });
  group.appendChild(foreign);
  block.commentForeign = foreign;
  if (typeof document !== 'undefined') {
    const div = document.createElement('div');
    div.setAttribute('xmlns', 'http://www.w3.org/1999/xhtml');
    div.className = 'comment-text';
    const ct = String(block.params?.CommentText || '');
    try {
      div.textContent = ct;
    } catch (_e) {}
    try {
      (div as HTMLElement).innerText = ct;
    } catch (_e) {}
    try {
      (div.style as CSSStyleDeclaration).whiteSpace = 'pre-wrap';
    } catch (_e) {}
    foreign.appendChild(div);
    block.commentTextEl = div;
  } else {
    const fallback = createSvgElement!(
      'text',
      { x: textPadding, y: 24, class: 'block-text upright comment-text-fallback' },
      String(block.params?.CommentText || '')
    );
    group.appendChild(fallback);
    block.commentTextFallback = fallback;
  }
}
//=============================================================================
export default comment;
//=============================================================================
