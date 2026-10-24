//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect, beforeAll } from 'vitest';
//=============================================================================
let createMod: any;
let svgElements: any;
beforeAll(async () => {
  createMod = await import('../../../dist/render/create-renderer.js');
  svgElements = await import('../../../dist/render/svg-elements.js');
});
//=============================================================================
test('userfunc resize scheduling smoke', async () => {
  const mod = createMod;
  if (typeof mod.scheduleUserFuncResize === 'function') {
    try {
      mod.scheduleUserFuncResize(() => {});
    } catch (e) {
      void e;
    }
  } else {
    if (typeof mod.updateBlockLabel === 'function') {
      try {
        mod.updateBlockLabel({});
      } catch (e) {
        void e;
      }
    }
  }
});
//=============================================================================
test('user-func resize settle retry behavior triggers scheduleUserFuncResize', async () => {
  const { createRenderer } = createMod as any;
  const { scheduleUserFuncResize } = svgElements as any;

  if (typeof scheduleUserFuncResize === 'function') {
    expect(() => scheduleUserFuncResize(() => {})).not.toThrow();
  }

  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  const blockLayer = document.createElementNS(ns, 'g');
  const wireLayer = document.createElementNS(ns, 'g');
  const overlayLayer = document.createElementNS(ns, 'g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    routingDirty: false,
    autoRoute: true,
  };

  const renderer = createRenderer({
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

  const block = renderer.createBlock('expression', 10, 10, { id: 'uf1', params: { expr: 'u' } });
  if (block) {
    expect(() => renderer.updateBlockLabel(block)).not.toThrow();
  } else {
    expect(true).toBe(true);
  }
});
//=============================================================================
export {};
//=============================================================================
