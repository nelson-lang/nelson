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
import { isMatrixOnlyFrame, mergeMatrixOnlyFrame } from '../../src/wasm/figureFrameCache';
import type { FigureDisplayListResult } from '../../src/rpc/types';
//=============================================================================
const complete: FigureDisplayListResult = {
  handle: 0,
  width: 640,
  height: 480,
  layoutSignature: 'layout-1',
  displaySignature: 'display-1',
  controlLayoutWidth: 640,
  controlLayoutHeight: 480,
  primitives: [{ kind: 'rect', x: 0, y: 0, w: 10, h: 10 } as never],
  controls: [{ handle: 7, style: 'pushbutton', text: 'Start', x: 12, y: 8, w: 86, h: 28 } as never],
  menus: [{ text: 'Demo' } as never],
  webglMeshes: [
    {
      id: 1,
      revision: '3',
      positions: [0, 0, 0, 1, 1, 1],
      model: [1, 0, 0, 1],
      projection: [1, 0, 0, 1],
      viewport: [0, 0, 640, 480],
    } as never,
  ],
};
//=============================================================================
const compact: FigureDisplayListResult = {
  handle: 0,
  width: 640,
  height: 480,
  primitives: [],
  controls: [],
  matrixOnly: true,
  webglMeshes: [
    {
      id: 1,
      revision: '3',
      model: [2, 0, 0, 2],
      projection: [3, 0, 0, 3],
      viewport: [0, 0, 640, 480],
    } as never,
    { id: 9, revision: '1', model: [4] } as never,
  ],
};
//=============================================================================
describe('mergeMatrixOnlyFrame', () => {
  it('keeps the complete frame parts and takes only the matrices from the compact one', () => {
    const merged = mergeMatrixOnlyFrame(complete, compact);
    expect(merged.controls).toEqual(complete.controls);
    expect(merged.menus).toEqual(complete.menus);
    expect(merged.primitives).toEqual(complete.primitives);
    expect(merged.layoutSignature).toBe('layout-1');
    expect(merged.displaySignature).toBe('display-1');
    expect(merged.matrixOnly).toBeUndefined();
    expect(merged.webglMeshes).toEqual([
      {
        id: 1,
        revision: '3',
        positions: [0, 0, 0, 1, 1, 1],
        model: [2, 0, 0, 2],
        projection: [3, 0, 0, 3],
        viewport: [0, 0, 640, 480],
      },
    ]);
  });

  it('follows the compact frame size', () => {
    const merged = mergeMatrixOnlyFrame(complete, { ...compact, width: 920, height: 547 });
    expect(merged.width).toBe(920);
    expect(merged.height).toBe(547);
  });

  it('leaves a complete frame without meshes untouched apart from the size', () => {
    const { webglMeshes: _meshes, ...flat } = complete;
    const merged = mergeMatrixOnlyFrame(flat, compact);
    expect(merged.webglMeshes).toBeUndefined();
    expect(merged.controls).toEqual(complete.controls);
  });

  it('recognizes compact frames', () => {
    expect(isMatrixOnlyFrame(compact)).toBe(true);
    expect(isMatrixOnlyFrame(complete)).toBe(false);
  });
});
//=============================================================================
