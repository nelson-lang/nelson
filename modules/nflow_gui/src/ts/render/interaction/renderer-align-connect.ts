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
// Pull the hit stroke in past the port hit radius (12) so it never covers a
// port in the overlay and block a wire start.
const PORT_CLEARANCE = 16;
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

  // A wire the user just detached (re-targeted) is not re-proposed, so the hint
  // never nags the user to undo the move. Set by the reconnect handler, cleared
  // on the next create/delete connection. See renderer-wire-reconnect.ts.
  const isJustDetached = (
    from: string,
    fromIndex: number,
    to: string,
    toIndex: number
  ): boolean => {
    const s = S.suppressedAlignPair;
    return (
      !!s &&
      s.from === from &&
      Number(s.fromIndex ?? 0) === fromIndex &&
      s.to === to &&
      Number(s.toIndex ?? 0) === toIndex
    );
  };

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
            if (isJustDetached(ob.id, op.index, ib.id, ip.index)) continue;
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

  // The segment pulled in by `pad` at each end, or null when the ports are too
  // close to leave a clickable gap.
  function insetSegment(
    a: { x: number; y: number },
    b: { x: number; y: number },
    pad: number
  ): [{ x: number; y: number }, { x: number; y: number }] | null {
    const dx = b.x - a.x;
    const dy = b.y - a.y;
    const len = Math.hypot(dx, dy);
    if (len <= 2 * pad) return null;
    const ux = dx / len;
    const uy = dy / len;
    return [
      { x: a.x + ux * pad, y: a.y + uy * pad },
      { x: b.x - ux * pad, y: b.y - uy * pad },
    ];
  }

  function refreshAlignHints(): void {
    clearAlignHints();
    if (S.locked) return;
    const pair = findAlignPair();
    if (!pair) return;
    // No room for a hit stroke clear of the ports means no safe click target.
    const hit = insetSegment(pair.o, pair.i, PORT_CLEARANCE);
    if (!hit) return;
    const group = createSvgElement('g', { id: HINT_ID, class: 'align-connect-hint' });
    const d = `M ${pair.o.x} ${pair.o.y} L ${pair.i.x} ${pair.i.y}`;
    // Wide transparent hit path first so the dashed arrow is easy to click.
    group.appendChild(
      createSvgElement('path', {
        class: 'align-connect-hit',
        d: `M ${hit[0].x} ${hit[0].y} L ${hit[1].x} ${hit[1].y}`,
      })
    );
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
