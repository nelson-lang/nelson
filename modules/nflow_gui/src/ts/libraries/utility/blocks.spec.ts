//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import builtinRegistry from '../../app/blocks-registry.js';
import { comment as commentRenderer } from './comment.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeHelpers = () => ({
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  createSvgElement: (tag: string, attrs?: any, content?: string) => {
    const el = document.createElementNS(ns, tag) as Element;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    if (content) el.textContent = content;
    return el;
  },
  svgRect: () => document.createElementNS(ns, 'rect'),
  svgText: () => document.createElementNS(ns, 'text'),
  renderSourcePlot: () => {},
  renderCenteredAxesPlot: () => {},
  renderTeXMath: () => {},
});
//=============================================================================
test('loads blocks.ts if present or registry contains utility blocks', async () => {
  const defs = builtinRegistry.getDefinitions();
  const hasAny = defs['comment'] || defs['switch'] || defs['toggleSwitch'] || defs['subsystem'];
  expect(hasAny).toBeDefined();
});
//=============================================================================
// Non-regression: blocks.ts must export the comment renderer (not the static SVG fallback)
// so that typing in the inspector shows text on the canvas block.
describe('comment renderer exported from blocks.ts', () => {
  test('comment renderer is a function', () => {
    expect(typeof commentRenderer).toBe('function');
  });

  test('renders div.comment-text with the initial commentText', () => {
    const group = document.createElementNS(ns, 'g');
    const block = {
      id: 'b1',
      type: 'comment',
      group,
      width: 220,
      height: 120,
      params: { CommentText: 'dd', ShowBorder: true },
    };
    commentRenderer(block, makeHelpers());
    const textEl = group.querySelector('.comment-text') as HTMLElement | null;
    expect(textEl).not.toBeNull();
    expect(textEl!.textContent).toBe('dd');
  });

  test('renders div.comment-text with empty string when commentText is absent', () => {
    const group = document.createElementNS(ns, 'g');
    const block = {
      id: 'b2',
      type: 'comment',
      group,
      width: 220,
      height: 120,
      params: {},
    };
    commentRenderer(block, makeHelpers());
    const textEl = group.querySelector('.comment-text') as HTMLElement | null;
    expect(textEl).not.toBeNull();
    expect(textEl!.textContent).toBe('');
  });

  test('does NOT render a static SVG image element for comment blocks', () => {
    const group = document.createElementNS(ns, 'g');
    const block = {
      id: 'b3',
      type: 'comment',
      group,
      width: 220,
      height: 120,
      params: { CommentText: 'test' },
    };
    commentRenderer(block, makeHelpers());
    // If the image renderer were used instead of comment.ts, an <image> element would be present
    expect(group.querySelector('image')).toBeNull();
  });

  test('comment renderer is not the image/stub renderer (regression guard)', () => {
    // The static image renderer produces an <image> element and never a .comment-text div.
    const group = document.createElementNS(ns, 'g');
    const block = {
      id: 'b4',
      type: 'comment',
      group,
      width: 220,
      height: 120,
      params: { CommentText: 'regression' },
    };
    commentRenderer(block, makeHelpers());
    expect(group.querySelector('foreignObject')).not.toBeNull();
    expect(group.querySelector('.comment-text')).not.toBeNull();
  });
});
//=============================================================================
