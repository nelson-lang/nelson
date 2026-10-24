//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { rotatePoint } from '../render/geometry.js';
import type { Block } from '../render/geometry.js';
import { disposeTooltipView, renderTooltipView } from './components/tooltip-view.js';
//=============================================================================
let svgRoot: SVGSVGElement | null = null;
let tooltipVisible = false;
let tooltipX = 0;
let tooltipY = 0;
let tooltipText = '';
type TooltipPort = { x?: number; y?: number; type?: string; index?: number };
type TooltipBlock = Block & {
  ports?: (TooltipPort & { x?: number; y?: number })[];
  params?: { name?: string };
  label?: string;
  type?: string;
};
//=============================================================================
type RendererForTooltip = {
  clientToSvg?: (x: number, y: number) => { x: number; y: number };
  state?: { blocks: Map<string, TooltipBlock> };
};
//=============================================================================
let rendererRef: RendererForTooltip | { current: RendererForTooltip | null } | null = null;
function renderTooltip(): void {
  renderTooltipView({
    visible: tooltipVisible,
    x: tooltipX,
    y: tooltipY,
    text: tooltipText,
  });
}
//=============================================================================
function hideTooltip(): void {
  tooltipVisible = false;
  renderTooltip();
}
//=============================================================================
function showTooltipAt(x: number, y: number, text: string): void {
  tooltipX = x;
  tooltipY = y;
  tooltipText = text;
  tooltipVisible = true;
  renderTooltip();
}
//=============================================================================
function findNearestPortInfo(
  block: TooltipBlock | undefined | null,
  svgPoint: { x: number; y: number }
) {
  if (!block || !Array.isArray(block.ports)) return null;
  let bestPort: TooltipPort | null = null;
  let bestDist = Infinity;
  const ports = block.ports || [];
  for (let i = 0; i < ports.length; i++) {
    const p = ports[i] as TooltipPort & { x?: number; y?: number };
    const px = Number(block.x || 0) + Number(p.x || 0);
    const py = Number(block.y || 0) + Number(p.y || 0);
    const rp = rotatePoint({ x: px, y: py }, block as Block);
    const dx = rp.x - svgPoint.x;
    const dy = rp.y - svgPoint.y;
    const d = Math.sqrt(dx * dx + dy * dy);
    if (d < bestDist) {
      bestDist = d;
      bestPort = p;
    }
  }
  if (bestPort !== null && bestDist < 24) return bestPort;
  return null;
}
//=============================================================================
export function initTooltip({
  svg,
  renderer,
}: {
  svg: SVGSVGElement | null;
  renderer: RendererForTooltip | { current: RendererForTooltip | null } | null;
}): void {
  svgRoot = svg;
  rendererRef = renderer;
  renderTooltip();

  if (!svgRoot) return;

  svgRoot.addEventListener(
    'pointerover',
    (ev: PointerEvent) => {
      const target = ev.target as Element;
      const portEl = target.closest ? target.closest('.port') : null;
      if (!portEl) return;
      const blockEl = target.closest ? target.closest('[data-block-id]') : null;
      const blockId = blockEl ? blockEl.getAttribute('data-block-id') : null;
      let renderer: RendererForTooltip | null = null;
      if (rendererRef) {
        if ('current' in rendererRef) renderer = rendererRef.current ?? null;
        else renderer = rendererRef as RendererForTooltip;
      }
      const svgPoint = renderer?.clientToSvg
        ? renderer.clientToSvg(ev.clientX, ev.clientY)
        : { x: ev.clientX, y: ev.clientY };
      let label = 'Port';
      try {
        if (blockId && renderer && renderer.state && renderer.state.blocks) {
          const block =
            renderer.state.blocks.get(blockId) ||
            (rendererRef && 'current' in rendererRef && rendererRef.current
              ? rendererRef.current!.state?.blocks.get(blockId || '')
              : undefined);
          if (block) {
            const name = block.params?.name || block.label || block.type || 'Block';
            const portInfo = findNearestPortInfo(block, svgPoint);
            if (portInfo) {
              label = `${name} - ${portInfo.type || 'port'} ${Number(portInfo.index ?? 0) + 1}`;
            } else {
              label = `${name} - port`;
            }
          }
        }
      } catch (e) {}
      showTooltipAt(ev.clientX, ev.clientY, label);
    },
    { passive: true }
  );

  svgRoot.addEventListener(
    'pointermove',
    (ev: PointerEvent) => {
      if (tooltipVisible) {
        tooltipX = ev.clientX;
        tooltipY = ev.clientY;
        renderTooltip();
      }
    },
    { passive: true }
  );

  svgRoot.addEventListener(
    'pointerout',
    (ev: PointerEvent) => {
      const target = ev.target as Element;
      const portEl = target.closest ? target.closest('.port') : null;
      if (portEl) hideTooltip();
    },
    { passive: true }
  );
}
//=============================================================================
export function disposeTooltip(): void {
  disposeTooltipView();
  tooltipVisible = false;
  tooltipX = 0;
  tooltipY = 0;
  tooltipText = '';
  svgRoot = null;
  rendererRef = null;
}
//=============================================================================
