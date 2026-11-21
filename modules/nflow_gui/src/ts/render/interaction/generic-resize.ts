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
const SPECIAL = new Set([
  'scope',
  'xyScope',
  'xyzScope',
  'comment',
  'display',
  'toggleSwitch',
  'constant',
  'gain',
  'sum',
  'mult',
  'trigFunction',
  'mathFunction',
  'busCreator',
  'busSelector',
]);
export const usesGenericResize = (type: unknown): boolean =>
  typeof type === 'string' && type.length > 0 && !SPECIAL.has(type);

export function captureGenericResize(block: any) {
  return {
    width: block.width,
    height: block.height,
    visuals: Array.from(block.group?.children || [])
      .filter(
        (el: any) =>
          !el.matches('.port-group, .physical-pin, .drag-handle, .resize-handle, .acausal-caption')
      )
      .map((el: any) => ({ el, transform: el.getAttribute('transform') || '' })),
    ports: (block.ports || []).map((port: any) => ({
      port,
      x: port.x,
      y: port.y,
      wireX: port.wireX ?? port.x,
      wireY: port.wireY ?? port.y,
    })),
  };
}

export function resizeGenericBlock(block: any, start: any, ctx: any) {
  block.width = Math.max(24, block.width);
  block.height = Math.max(24, block.height);
  block.params.width = block.width;
  block.params.height = block.height;
  const sx = block.width / start.width;
  const sy = block.height / start.height;
  for (const visual of start.visuals) {
    visual.el.setAttribute('transform', `scale(${sx}, ${sy}) ${visual.transform}`.trim());
  }
  for (const item of start.ports) {
    const port = item.port;
    port.x = item.x * sx;
    port.y = item.y * sy;
    port.wireX = item.wireX * sx;
    port.wireY = item.wireY * sy;
    if (port.type === 'phys') {
      port.el?.setAttribute('cx', String(port.x));
      port.el?.setAttribute('cy', String(port.y));
      const pin = block.physicalPins?.[port.index];
      if (pin) {
        pin.x = port.x;
        pin.y = port.y;
      }
    } else {
      ctx.updatePortElement?.(port);
    }
  }
  const specs = (type: string) =>
    (block.ports || [])
      .filter((p: any) => p.type === type)
      .sort((a: any, b: any) => a.index - b.index)
      .map((p: any) => ({ x: p.x, y: p.y, side: p.side, wireX: p.wireX, wireY: p.wireY }));
  block.dynamicInputs = specs('in');
  block.dynamicOutputs = specs('out');
  const w = Math.max(80, block.width),
    h = Math.max(80, block.height);
  block.dragRect?.setAttribute('x', String((block.width - w) / 2));
  block.dragRect?.setAttribute('y', String((block.height - h) / 2));
  block.dragRect?.setAttribute('width', String(w));
  block.dragRect?.setAttribute('height', String(h));
  block.resizeHandle?.setAttribute('x', String(block.width - 16));
  block.resizeHandle?.setAttribute('y', String(block.height - 16));
  ctx.updateBlockTransform?.(block);
}
//=============================================================================
