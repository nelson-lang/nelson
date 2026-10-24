//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let ri: any;
beforeEach(async () => {
  ri = await import('../../../dist/render/index.js');
});
//=============================================================================
test('exports key constants with expected types', () => {
  expect(typeof ri.GRID_SIZE).toBe('number');
  expect(typeof ri.WIRE_COLOR).toBe('string');
  expect(typeof ri.MIN_ZOOM).toBe('number');
  expect(typeof ri.INITIAL_VIEWBOX_SIZE).toBe('number');
});
//=============================================================================
test('createRenderer returns provided elements and defaults', () => {
  const svg = document.createElement('svg');
  const blockLayer = document.createElement('g');
  const obj = ri.createRenderer({ svg, blockLayer });
  expect(obj.svg).toBe(svg);
  expect(obj.blockLayer).toBe(blockLayer);
  expect(obj.gridSize).toBe(ri.GRID_SIZE);
  expect(obj.portRadius).toBe(ri.PORT_RADIUS);
});
//=============================================================================
test('createRenderer accepts overrides for gridSize and portRadius', () => {
  const obj = ri.createRenderer({ gridSize: 123, portRadius: 7 });
  expect(obj.gridSize).toBe(123);
  expect(obj.portRadius).toBe(7);
});
//=============================================================================
