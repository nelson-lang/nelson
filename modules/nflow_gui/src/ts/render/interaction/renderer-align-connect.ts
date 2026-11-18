//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Align to connect hint. When an output and an input line up with a clear gap
// and no wire yet, show a clickable arrow in the overlay to create the wire.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { createSvgElement } from '../svg-elements.js';
import type { RendererCtx } from '../core/renderer-types.js';
//=============================================================================
const Y_TOL = 6; // ports count as aligned within this vertical gap (SVG px)
const MIN_GAP = 10; // the output must sit at least this far left of the input
const MAX_GAP = 400; // and no farther than this (ignore distant pairs)
const HINT_ID = '_nflow-align-hint';
//=============================================================================
export type AlignPair = {
  from: string;
  fromIndex: number;
  to: string;
  toIndex: number;
  o: { x: number; y: number };
  i: { x: number; y: number };
};
//=============================================================================
export function makeAlignConnectHandlers(ctx: RendererCtx) {
  const C = ctx as any;
  const S = ctx.state as any;

  const portsOf = (blk: any, type: 'in' | 'out'): any[] =>
    (blk?.ports ?? []).filter((p: any) => p.type === type);

  const alreadyConnected = (
    from: string,
    fromIndex: number,
    to: string,
    toIndex: number
  ): boolean =>
    (S.connections as any[]).some(
      (c) =>
        c.from === from &&
        Number(c.fromIndex ?? 0) === fromIndex &&
        c.to === to &&
        Number(c.toIndex ?? 0) === toIndex
    );

  const inputOccupied = (to: string, toIndex: number): boolean =>
    (S.connections as any[]).some((c) => c.to === to && Number(c.toIndex ?? 0) === toIndex);

  // True when another block overlaps the corridor between the two ports.
  const corridorBlocked = (
    ox: number,
    oy: number,
    ix: number,
    iy: number,
    aId: string,
    bId: string
  ): boolean => {
    const top = Math.min(oy, iy) - 2;
    const bottom = Math.max(oy, iy) + 2;
    const left = Math.min(ox, ix);
    const right = Math.max(ox, ix);
    for (const blk of S.blocks.values() as Iterable<any>) {
      if (blk.id === aId || blk.id === bId) continue;
      const bw = blk.width ?? 0;
      const bh = blk.height ?? 0;
      if (blk.x < right && blk.x + bw > left && blk.y < bottom && blk.y + bh > top) return true;
    }
    return false;
  };

  // The closest output and input pair that is aligned, clear and not wired yet.
  function findAlignPair(): AlignPair | null {
    let best: AlignPair | null = null;
    let bestGap = Infinity;
    const blocks = [...(S.blocks.values() as Iterable<any>)];
    for (const ob of blocks) {
      for (const op of portsOf(ob, 'out')) {
        const o = C.getPortPosition?.(ob.id, 'out', op.index);
        if (!o) continue;
        for (const ib of blocks) {
          if (ib.id === ob.id) continue;
          for (const ip of portsOf(ib, 'in')) {
            const i = C.getPortPosition?.(ib.id, 'in', ip.index);
            if (!i) continue;
            const gap = i.x - o.x;
            if (gap < MIN_GAP || gap > MAX_GAP) continue;
            if (Math.abs(o.y - i.y) > Y_TOL) continue;
            if (inputOccupied(ib.id, ip.index)) continue;
            if (alreadyConnected(ob.id, op.index, ib.id, ip.index)) continue;
            if (corridorBlocked(o.x, o.y, i.x, i.y, ob.id, ib.id)) continue;
            if (gap < bestGap) {
              bestGap = gap;
              best = { from: ob.id, fromIndex: op.index, to: ib.id, toIndex: ip.index, o, i };
            }
          }
        }
      }
    }
    return best;
  }

  function clearAlignHints(): void {
    C.overlayLayer?.querySelector(`#${HINT_ID}`)?.remove();
  }

  function refreshAlignHints(): void {
    clearAlignHints();
    if (S.locked) return;
    const pair = findAlignPair();
    if (!pair) return;
    const group = createSvgElement('g', { id: HINT_ID, class: 'align-connect-hint' });
    const d = `M ${pair.o.x} ${pair.o.y} L ${pair.i.x} ${pair.i.y}`;
    // Wide transparent hit path first so the thin dashed arrow is easy to click.
    group.appendChild(createSvgElement('path', { class: 'align-connect-hit', d }));
    group.appendChild(
      createSvgElement('path', {
        class: 'align-connect-arrow',
        d,
        'marker-end': 'url(#wire-arrow)',
      })
    );
    group.addEventListener('click', (event: Event) => {
      event.stopPropagation();
      C.createConnection?.(pair.from, pair.to, pair.toIndex, pair.fromIndex);
      clearAlignHints();
    });
    C.overlayLayer?.appendChild(group);
  }

  return { refreshAlignHints, clearAlignHints, findAlignPair };
}
//=============================================================================
