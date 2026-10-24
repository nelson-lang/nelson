//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Point, Rect } from '../geometry.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
type RendererCtx = {
  svg: SVGSVGElement | null;
  blockLayer?: Element | null;
  wireLayer?: Element | null;
  overlayLayer?: Element | null;
  state?: State | null;
};
//=============================================================================
type State = {
  blocks?: Map<string, { group?: Element }>;
  connections?: Array<{ path?: Element }>;
  nextId?: number;
  dirtyBlocks?: Set<unknown>;
  dirtyConnections?: Set<unknown>;
  selectedIds?: Set<string>;
  selectedConnections?: Set<unknown>;
  pendingPort?: unknown;
};
//=============================================================================
export function makeUtilHandlers(ctx: RendererCtx) {
  function clientToSvg(clientX: number, clientY: number): Point {
    const svg = ctx.svg;
    if (!svg) return { x: clientX, y: clientY };
    const createPt = (svg as SVGSVGElement).createSVGPoint;
    let pt:
      | DOMPoint
      | { x: number; y: number; matrixTransform: (m: DOMMatrix) => { x: number; y: number } };
    if (typeof createPt === 'function') pt = createPt.call(svg);
    else
      pt = {
        x: 0,
        y: 0,
        matrixTransform: (m: DOMMatrix) => {
          void m;
          return { x: 0, y: 0 };
        },
      };
    pt.x = clientX;
    pt.y = clientY;
    const ctm = svg.getScreenCTM();
    if (!ctm) return { x: clientX, y: clientY };
    const inv = ctm.inverse();
    if ('matrixTransform' in pt) {
      const p = pt.matrixTransform(inv as DOMMatrix);
      const pPoint = p as unknown as { x: number; y: number };
      return { x: pPoint.x ?? clientX, y: pPoint.y ?? clientY };
    }
    return { x: clientX, y: clientY };
  }

  function setLoopHighlight(blockIds: string[] | Set<string> | null, conns: unknown[] | null) {
    const svg = ctx.svg;
    if (!svg) return;
    if (blockIds === null && conns === null) {
      svg
        .querySelectorAll('.loop-highlight')
        .forEach((el) => el.classList.remove('loop-highlight'));
      try {
        if (ctx.state && ctx.state.blocks instanceof Map) {
          for (const [, blk] of ctx.state.blocks) {
            if (blk && blk.group && blk.group.classList)
              blk.group.classList.remove('loop-highlight');
          }
        }
        if (ctx.state && Array.isArray(ctx.state.connections)) {
          for (const c of ctx.state.connections) {
            if (c && c.path && c.path.classList) c.path.classList.remove('loop-highlight');
          }
        }
      } catch (e) {}
      return;
    }
    svg.querySelectorAll('.loop-highlight').forEach((el) => el.classList.remove('loop-highlight'));
    try {
      if (ctx.state && ctx.state.blocks instanceof Map) {
        for (const [, blk] of ctx.state.blocks) {
          if (blk && blk.group && blk.group.classList) blk.group.classList.remove('loop-highlight');
        }
      }
      if (ctx.state && Array.isArray(ctx.state.connections)) {
        for (const c of ctx.state.connections) {
          if (c && c.path && c.path.classList) c.path.classList.remove('loop-highlight');
        }
      }
    } catch (e) {}
    const ids =
      blockIds instanceof Set ? Array.from(blockIds) : Array.isArray(blockIds) ? blockIds : [];
    ids.forEach((id) => {
      let el: Element | null = null;
      try {
        if (ctx.state && ctx.state.blocks instanceof Map) {
          const blk = ctx.state.blocks.get(id);
          if (blk && blk.group) el = blk.group as Element;
        }
      } catch (e) {}
      if (!el) el = svg.querySelector(`[data-block-id="${id}"]`);
      if (el) el.classList.add('loop-highlight');
    });
    (conns || []).forEach((c) => {
      const p = c as { path?: { classList?: { add?: (s: string) => void } } } | undefined;
      const path = p?.path as Element | undefined;
      if (path && path.classList && typeof path.classList.add === 'function')
        path.classList.add('loop-highlight');
    });
  }

  function updateSelectionBox(node: Element | null, rect: Rect | null) {
    if (!node) return;
    if (!rect) {
      node.removeAttribute('x');
      node.removeAttribute('y');
      node.removeAttribute(FieldNames.kWidth);
      node.removeAttribute(FieldNames.kHeight);
      return;
    }
    const x = rect.left;
    const y = rect.top;
    const width = rect.right - rect.left;
    const height = rect.bottom - rect.top;
    node.setAttribute('x', String(x));
    node.setAttribute('y', String(y));
    node.setAttribute('width', String(width));
    node.setAttribute('height', String(height));
  }

  function clearWorkspace() {
    if (ctx.blockLayer) ctx.blockLayer.innerHTML = '';
    if (ctx.wireLayer) ctx.wireLayer.innerHTML = '';
    if (ctx.overlayLayer) {
      // Preserve core overlay elements (selection rects and special layers)
      const preserve = new Set([
        'selection-rect',
        'selection-marquee',
        'wire-junction-layer',
        'wire-corner-layer',
      ]);
      const children = Array.from(ctx.overlayLayer.children);
      for (const ch of children) {
        const cls = ch.classList && ch.classList.length ? Array.from(ch.classList) : [];
        const keep = cls.some((c) => preserve.has(c));
        if (!keep) ctx.overlayLayer.removeChild(ch);
      }
      const sel = ctx.overlayLayer.querySelector('.selection-rect');
      if (sel && typeof sel.setAttribute === 'function') sel.setAttribute('display', 'none');
      const marquee = ctx.overlayLayer.querySelector('.selection-marquee');
      if (marquee && typeof marquee.setAttribute === 'function')
        marquee.setAttribute('display', 'none');
    }
    try {
      const s = ctx.state;
      if (s) {
        if (s.blocks && typeof s.blocks.clear === 'function') s.blocks.clear();
        if (Array.isArray(s.connections)) s.connections.length = 0;
      }
    } catch (e) {
      // ignore
    }
  }

  return { clientToSvg, setLoopHighlight, updateSelectionBox, clearWorkspace };
}
//=============================================================================
