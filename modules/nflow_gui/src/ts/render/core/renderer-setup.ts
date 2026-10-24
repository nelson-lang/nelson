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
import { createSvgElement } from '../svg-elements.js';
import type { RendererCtx } from './renderer-types.js';
//=============================================================================
export function setupRendererDom(ctx: RendererCtx) {
  const C = ctx as unknown as { svg: SVGSVGElement | null; overlayLayer: Element };

  let defs: Element | null = null;
  if (C.svg) defs = C.svg.querySelector('defs') as Element | null;
  if (!defs && C.svg) {
    defs = createSvgElement('defs');
    C.svg.insertBefore(defs, C.svg.firstChild as Node | null);
  }
  if (defs && !defs.querySelector('#wire-arrow')) {
    const marker = createSvgElement('marker', {
      id: 'wire-arrow',
      markerWidth: 12,
      markerHeight: 12,
      refX: 12,
      refY: 6,
      orient: 'auto',
      markerUnits: 'userSpaceOnUse',
    });
    marker.appendChild(createSvgElement('path', { d: 'M0,0 L12,6 L0,12 Z', class: 'wire-arrow' }));
    defs.appendChild(marker);
  }

  if (C.svg) {
    ctx.selectionRect = createSvgElement('rect', {
      class: 'selection-rect',
      x: 0,
      y: 0,
      width: 0,
      height: 0,
      display: 'none',
    });
  }
  C.overlayLayer.appendChild(ctx.selectionRect as Element);

  if (C.svg) {
    ctx.marqueeRect = createSvgElement('rect', {
      class: 'selection-marquee',
      x: 0,
      y: 0,
      width: 0,
      height: 0,
      display: 'none',
    });
  }
  C.overlayLayer.appendChild(ctx.marqueeRect as Element);

  ctx.wireJunctionLayer = createSvgElement('g', { class: 'wire-junction-layer' });
  C.overlayLayer.appendChild(ctx.wireJunctionLayer);

  ctx.wireCornerLayer = createSvgElement('g', { class: 'wire-corner-layer' });
  C.overlayLayer.appendChild(ctx.wireCornerLayer);

  ctx.debugLog = document.getElementById('debugLog');
  const copyDebugButton = document.getElementById('copyDebug');
  if (ctx.debugLog && copyDebugButton) {
    copyDebugButton.style.display = 'inline-flex';
    copyDebugButton.addEventListener('click', async () => {
      const text = ctx.debugLog!.textContent || '';
      if (!text) return;
      try {
        await navigator.clipboard.writeText(text);
      } catch (_err) {
        const textarea = document.createElement('textarea');
        textarea.value = text;
        textarea.setAttribute('readonly', 'true');
        textarea.style.position = 'fixed';
        textarea.style.left = '-9999px';
        document.body.appendChild(textarea);
        textarea.select();
        document.execCommand('copy');
        document.body.removeChild(textarea);
      }
    });
  }
}
//=============================================================================
