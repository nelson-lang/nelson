//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { GRID_SIZE, snapToGrid, alignToGrid } from '../utils/geometry';
import {
  enforcePortStubs,
  buildStubPoint,
  isValidStub,
  dedupePoints,
  removeColinearPoints,
  pointsToPath,
  rotatePoint,
  getRotatedBounds,
  minTurnDistanceToEndpointCells,
  firstTurnInfo,
  canAdjustSharedTurn,
  wouldCreateNonSharedOverlap,
  getPortSide,
  sideToDir,
} from './router';
//=============================================================================
function makeBlock(id, x, y, width = 100, height = 100, portX = 50, portY = 50, portType = 'out') {
  return {
    id,
    x,
    y,
    width,
    height,
    rotation: 0,
    ports: [
      {
        type: portType,
        index: 0,
        x: portX,
        y: portY,
      },
    ],
  };
}
//=============================================================================
describe('router helpers: stubs, rotation, path utils', () => {
  test('buildStubPoint and isValidStub for left side', () => {
    const port = { x: 50, y: 40 };
    const stub = buildStubPoint(port, 'left');
    expect(stub.x).toBe(port.x - GRID_SIZE);
    expect(stub.y).toBe(port.y);

    const other = { x: port.x - GRID_SIZE, y: port.y };
    expect(isValidStub(port, other, 'left')).toBe(true);
    expect(isValidStub(port, { x: port.x, y: port.y }, 'left')).toBe(false);
  });

  test('dedupePoints and removeColinearPoints', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 10, y: 0 }, // duplicate
      { x: 20, y: 0 },
      { x: 20, y: 10 },
    ];
    const deduped = dedupePoints(pts);
    expect(deduped).toEqual([
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 20, y: 0 },
      { x: 20, y: 10 },
    ]);
    const simplified = removeColinearPoints(deduped);
    expect(simplified).toEqual([
      { x: 0, y: 0 },
      { x: 20, y: 0 },
      { x: 20, y: 10 },
    ]);
  });

  test('pointsToPath produces SVG path string', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 10, y: 10 },
    ];
    const path = pointsToPath(pts);
    expect(path).toBe('M 0 0 L 10 0 L 10 10');
  });

  test('rotatePoint respects rotation 90 and 180', () => {
    const block90 = { x: 0, y: 0, width: 10, height: 10, rotation: 90 };
    const p = { x: 0, y: 0 };
    // center (5,5), angle 90: { x: cx - dy, y: cy + dx }
    const r90 = rotatePoint(p, block90);
    expect(r90).toEqual({ x: 10, y: 0 });

    const block180 = { x: 0, y: 0, width: 10, height: 10, rotation: 180 };
    const r180 = rotatePoint(p, block180);
    // angle 180: { x: cx - dx, y: cy - dy }
    expect(r180).toEqual({ x: 10, y: 10 });
  });

  test('getPortSide and sideToDir mapping', () => {
    const block = makeBlock('B', 0, 0, 100, 100, 50, 50, 'out');
    const leftPt = { x: block.x + 0, y: block.y + 50 };
    expect(getPortSide(block, leftPt)).toBe('left');
    expect(sideToDir('left')).toBe('left');
    const rightPt = { x: block.x + 100, y: block.y + 50 };
    expect(getPortSide(block, rightPt)).toBe('right');
    expect(sideToDir('right')).toBe('right');
    const topPt = { x: block.x + 50, y: block.y + 0 };
    expect(getPortSide(block, topPt)).toBe('top');
    expect(sideToDir('top')).toBe('up');
    const bottomPt = { x: block.x + 50, y: block.y + 100 };
    expect(getPortSide(block, bottomPt)).toBe('bottom');
    expect(sideToDir('bottom')).toBe('down');
  });

  test('enforcePortStubs adds port positions and stubs', () => {
    const from = makeBlock('S', 0, 0, 100, 100, 10, 50, 'out');
    const to = makeBlock('T', 200, 0, 100, 100, 50, 50, 'in');
    const state = {
      blocks: new Map([
        ['S', from],
        ['T', to],
      ]),
    };
    const conn = { from: 'S', to: 'T', fromIndex: 0, toIndex: 0 };
    // points are arbitrary and do not include the exact port positions
    const pts = [
      { x: 100, y: 50 },
      { x: 200, y: 50 },
    ];
    const out = enforcePortStubs(state, conn, pts);
    const fromPortPos = alignToGrid({ x: from.x + from.ports[0].x, y: from.y + from.ports[0].y });
    const toPortPos = alignToGrid({ x: to.x + to.ports[0].x, y: to.y + to.ports[0].y });
    expect(out[0]).toEqual(fromPortPos);
    expect(out[out.length - 1]).toEqual(toPortPos);
    expect(out[0]).toEqual(fromPortPos);
    expect(out[out.length - 1]).toEqual(toPortPos);
    out.forEach((p) => {
      expect(p.x % GRID_SIZE).toBe(0);
      expect(p.y % GRID_SIZE).toBe(0);
    });
    for (let i = 0; i < out.length - 1; i += 1) {
      const a = out[i];
      const b = out[i + 1];
      expect(a.x === b.x || a.y === b.y).toBe(true);
    }
  });

  test('removeColinearPoints removes vertical colinear points', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 0, y: 10 },
      { x: 0, y: 20 },
      { x: 10, y: 20 },
    ];
    const out = removeColinearPoints(pts);
    expect(out).toEqual([
      { x: 0, y: 0 },
      { x: 0, y: 20 },
      { x: 10, y: 20 },
    ]);
  });

  test('getRotatedBounds swaps width/height for 90/270 rotations', () => {
    const block = { x: 0, y: 0, width: 20, height: 10, rotation: 90 };
    const b = getRotatedBounds(block);
    // center = (10,5) w=height=10 h=width=20
    expect(b).toEqual({ left: 5, right: 15, top: -5, bottom: 15 });
  });

  test('buildStubPoint and isValidStub for right/top/bottom', () => {
    const port = { x: 50, y: 40 };
    expect(buildStubPoint(port, 'right')).toEqual({ x: port.x + GRID_SIZE, y: port.y });
    expect(buildStubPoint(port, 'top')).toEqual({ x: port.x, y: port.y - GRID_SIZE });
    expect(buildStubPoint(port, 'bottom')).toEqual({ x: port.x, y: port.y + GRID_SIZE });

    expect(isValidStub(port, { x: port.x + GRID_SIZE, y: port.y }, 'right')).toBe(true);
    expect(isValidStub(port, { x: port.x, y: port.y - GRID_SIZE }, 'top')).toBe(true);
    expect(isValidStub(port, { x: port.x, y: port.y + GRID_SIZE }, 'bottom')).toBe(true);
  });

  test('rotatePoint and getRotatedBounds for 270 degrees', () => {
    const block270 = { x: 0, y: 0, width: 10, height: 10, rotation: 270 };
    const p = { x: 0, y: 0 };
    const r270 = rotatePoint(p, block270);
    expect(r270).toEqual({ x: 0, y: 10 });

    const b270 = getRotatedBounds({ x: 0, y: 0, width: 20, height: 10, rotation: 270 });
    // center (10,5), width and height swap
    expect(b270).toEqual({ left: 5, right: 15, top: -5, bottom: 15 });
  });

  test('minTurnDistanceToEndpointCells and firstTurnInfo', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 10, y: 20 },
      { x: 20, y: 20 },
    ];
    expect(minTurnDistanceToEndpointCells(pts, 0)).toBe(1);
    expect(minTurnDistanceToEndpointCells(pts, pts.length - 1)).toBe(1);
    const info = firstTurnInfo(pts);
    expect(info).toBeDefined();
    expect(info.initDir).toBe('H');
    expect(info.turnIndex).toBe(1);
    expect(info.isSimpleL).toBe(true);
    expect(info.forwardSign).toBe(1);
  });
});
//=============================================================================
