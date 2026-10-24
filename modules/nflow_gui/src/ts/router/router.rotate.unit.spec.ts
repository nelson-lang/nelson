//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { routeDirtyConnections } from './router.js';
//=============================================================================
describe('router.ts - rotatePoint branches (via routeDirtyConnections)', () => {
  [90, 180, 270].forEach((angle) => {
    test(`rotation ${angle} completes without error`, () => {
      const A = {
        id: 'A',
        x: 200,
        y: 200,
        width: 40,
        height: 40,
        rotation: angle,
        ports: [{ type: 'out', index: 0, x: 10, y: 10 }],
      } as any;
      const B = {
        id: 'B',
        x: 400,
        y: 200,
        width: 40,
        height: 40,
        rotation: 0,
        ports: [{ type: 'in', index: 0, x: 30, y: 10 }],
      } as any;
      const conn = {
        from: 'A',
        to: 'B',
        points: [
          { x: 210, y: 210 },
          { x: 390, y: 210 },
        ],
      } as any;
      const state: any = {
        blocks: new Map([
          ['A', A],
          ['B', B],
        ]),
        connections: [conn],
      };
      const res = routeDirtyConnections(state, 800, 400, { x: 0, y: 0 }, new Set(), 200);
      expect(res instanceof Map).toBe(true);
      expect(Array.isArray(state.connections[0].points)).toBe(true);
    });
  });
});
//=============================================================================
