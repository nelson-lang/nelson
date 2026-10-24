//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { comment } from './comment';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeHelpers = () => ({
  createSvgElement: (tag: string, attrs?: any, content?: string) => {
    const el = document.createElementNS(ns, tag) as any;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    if (content) el.textContent = content;
    Object.defineProperty(el, 'dataset', { value: {}, writable: true, configurable: true });
    return el;
  },
  svgRect: () => document.createElementNS(ns, 'rect'),
  svgText: () => document.createElementNS(ns, 'text'),
  renderSourcePlot: () => {},
  renderCenteredAxesPlot: () => {},
  renderTeXMath: () => {},
});
//=============================================================================
const makeBlock = (overrides: any = {}) => ({
  id: 'c1',
  group: document.createElementNS(ns, 'g'),
  width: 120,
  height: 60,
  params: { CommentText: 'Hello world' },
  ...overrides,
});
//=============================================================================
describe('comment renderer', () => {
  test('appends comment-body rect to group', () => {
    const block = makeBlock();

    comment(block as any, makeHelpers() as any);

    expect(block.group.querySelector('rect.comment-body')).not.toBeNull();
  });

  test('sets block.commentBody', () => {
    const block = makeBlock();

    comment(block as any, makeHelpers() as any);

    expect((block as any).commentBody).toBeDefined();
  });

  test('sets block.commentForeign with comment-foreign class', () => {
    const block = makeBlock();

    comment(block as any, makeHelpers() as any);

    expect((block as any).commentForeign).toBeDefined();
    expect((block as any).commentForeign.getAttribute('class')).toContain('comment-foreign');
  });

  test('sets block.commentTextEl with commentText content', () => {
    const block = makeBlock({ params: { CommentText: 'Test note' } });

    comment(block as any, makeHelpers() as any);

    expect((block as any).commentTextEl).toBeDefined();
    expect((block as any).commentTextEl.textContent).toBe('Test note');
  });

  test('commentTextEl has comment-text class', () => {
    const block = makeBlock();

    comment(block as any, makeHelpers() as any);

    expect((block as any).commentTextEl.className).toBe('comment-text');
  });

  test('commentText defaults to empty string when absent', () => {
    const block = makeBlock({ params: {} });

    comment(block as any, makeHelpers() as any);

    expect((block as any).commentTextEl.textContent).toBe('');
  });
});
//=============================================================================
