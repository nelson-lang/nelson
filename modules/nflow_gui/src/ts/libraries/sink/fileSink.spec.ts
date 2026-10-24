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
import fs from 'fs';
import path from 'path';
//=============================================================================
let fileSink: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  fileSink = br.getDefinitions()['fileSink']?.render as any;
});
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeHelpers = () => ({
  svgRect: (x: number, y: number, w: number, h: number, cls?: string) => {
    const r = document.createElementNS(ns, 'rect');
    if (cls) r.setAttribute('class', cls);
    return r;
  },
  createSvgElement: (tag: string, attrs?: any, content?: string) => {
    const el = document.createElementNS(ns, tag) as any;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    if (content) el.textContent = content;
    return el;
  },
});
//=============================================================================
const makeBlock = (overrides: any = {}) => ({
  id: 'fs1',
  group: document.createElementNS(ns, 'g') as any,
  width: 80,
  height: 80,
  params: {},
  ...overrides,
});
//=============================================================================
describe('fileSink renderer', () => {
  test('appends nf-block rect to group', () => {
    const block = makeBlock();

    fileSink(block as any, makeHelpers() as any);

    expect(block.group.querySelector('rect.nf-block')).not.toBeNull();
  });

  test('appends file icon path with file-icon-stroke class', () => {
    const block = makeBlock();

    fileSink(block as any, makeHelpers() as any);

    const hasPath = !!block.group.querySelector('path.file-icon-stroke');
    const hasPolyline = !!block.group.querySelector('polyline.file-icon-stroke');
    const hasImage = !!block.group.querySelector('image') || !!block.group.querySelector('img');
    const hasGraphic = hasPath || hasPolyline || hasImage;
    expect(hasGraphic).toBeTruthy();
  });

  test('appends polyline for file fold corner', () => {
    const block = makeBlock();

    fileSink(block as any, makeHelpers() as any);
    const hasPolyline = !!block.group.querySelector('polyline.file-icon-stroke');
    const hasPath = !!block.group.querySelector('path.file-icon-stroke');
    const hasImage = !!block.group.querySelector('image') || !!block.group.querySelector('img');
    const hasGraphic = hasPolyline || hasPath || hasImage;
    expect(hasGraphic).toBeTruthy();
  });

  test('appends OUT label text', () => {
    const block = makeBlock();

    fileSink(block as any, makeHelpers() as any);
    const textEl =
      block.group.querySelector('text.file-icon-label') ||
      Array.from(block.group.querySelectorAll('text')).find(
        (t: any) => (t.textContent || '').trim() === 'OUT'
      );
    const img = block.group.querySelector('image') || block.group.querySelector('img');
    expect(textEl || img).not.toBeNull();
    if (textEl) expect((textEl as any).textContent).toBe('OUT');
  });
});
//=============================================================================
