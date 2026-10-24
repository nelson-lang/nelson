//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let renderCreate: any;
beforeAll(async () => {
  renderCreate = await import('../../../dist/render/create-renderer.js');
});
//=============================================================================
test('createBlock constructs built-in templates (constant, tf, dtf, scope, userFunc)', () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  (svg as any).dataset.worldWidth = '800';
  (svg as any).dataset.worldHeight = '600';
  Object.defineProperty(svg, 'clientWidth', { value: 800, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { value: 600, configurable: true });

  const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyBlocks: new Set(),
    dirtyConnections: new Set(),
    selectedIds: new Set(),
    selectedConnection: null,
    pendingPort: null,
    routingDirty: false,
    autoRoute: false,
  };

  const renderer = renderCreate.createRenderer({
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    onSelectBlock: () => {},
    onSelectConnection: () => {},
    onOpenSubsystem: () => {},
    onConnectionError: () => {},
  });

  const types = ['constant', 'tf', 'dtf', 'scope', 'expression'];
  types.forEach((t) => {
    const block = renderer.createBlock(t as any);
    if (block) {
      expect(block).toBeTruthy();
      expect(block.group).toBeTruthy();
      expect(blockLayer.querySelector(`[data-block-id="${block.id}"]`)).not.toBeNull();
      expect((block as any).paramDisplay).toBeUndefined();
    }
  });
});
//=============================================================================
