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
// Acausal SCHEMATIC layout. The generic flow auto-layout (engine nflow_layout,
// longest-path columns) is right for causal signal-flow block diagrams but wrong
// for acausal circuits, which are UNDIRECTED nets, not a left-to-right pipeline.
// This module lays acausal models out the way a circuit is hand-drawn: an energy
// SOURCE stands vertical on the left, the series elements march across a top
// rail, shunt/return elements drop to a reference (ground/wall) rail along the
// bottom, and the reference marker sits at the bottom. Masses/inertias hang above
// their node, sensors hang below, and the causal signal blocks (scopes, sinks,
// signal sources) keep the flow layout off to the sides.
//
// It is a pure, framework-free function (unit-testable headless): it takes an
// abstract model -- blocks with their physical pins (side + domain), the physical
// nets, and the signal connections -- and returns a { x, y, rotation } per block.
// Positions are purely visual: acausal wiring is by pin NAME, so a re-layout never
// changes what the model simulates. Orientation is expressed as a rotation in
// {0,90,180,270} because the renderer's router is rotation-aware (rotatePoint), so
// a pin driven to a chosen edge routes cleanly with no arrowheads.
//=============================================================================
export type PinSide = 'left' | 'right' | 'top' | 'bottom';
export interface SchematicPin {
  name: string;
  side: PinSide;
  domain: string;
}
//=============================================================================
export interface SchematicBlock {
  id: string;
  type: string;
  acausal: boolean;
  width: number;
  height: number;
  // Physical (undirected) pins, resolved to their edge side. Per-instance frames
  // (PlanarBody) must already be included here by the caller.
  pins: SchematicPin[];
  hasSignalOut: boolean;
  hasSignalIn: boolean;
}
//=============================================================================
// One physical net: the pin keys "<blockId>.<pinName>" it joins.
export interface SchematicNet {
  pins: string[];
}
//=============================================================================
export interface SchematicEdge {
  from: string;
  to: string;
}
//=============================================================================
export interface SchematicModel {
  blocks: SchematicBlock[];
  physicalWires: SchematicNet[];
  connections: SchematicEdge[];
}
//=============================================================================
export interface Placement {
  x: number;
  y: number;
  rotation: number;
}
//=============================================================================
export interface SchematicLayoutOptions {
  grid?: number;
  colWidth?: number;
  originX?: number;
  loadY?: number; // masses / inertias / bodies lane (above the rail)
  railY?: number; // top rail pin-line
  bottomY?: number; // reference rail pin-line
  sensorY?: number; // sensors lane (below the reference rail)
}
//=============================================================================
// Reference anchors -> bottom rail (fixed potential / position / temperature).
const REF_TYPES = new Set(['Ground', 'Fixed', 'RotFixed', 'FixedTemperature']);
// Planar kinematic anchors: not a ground rail but the fixed pivot at the START
// of the chain, so they are laid inline on the left facing into the chain.
const ANCHOR1 = new Set(['PlanarWorld', 'PlanarFixed']);
// Two-terminal energy sources -> vertical on the far left, hot terminal up.
const SOURCE2 = new Set([
  'ConstantVoltage',
  'SignalVoltage',
  'SineVoltage',
  'ExpSineVoltage',
  'TrapezoidVoltage',
  'RampVoltage',
  'ConstantCurrent',
  'SignalCurrent',
  'SineCurrent',
  'ExpSineCurrent',
  'TrapezoidCurrent',
  'RampCurrent',
]);
//=============================================================================
// One-terminal drivers (flange sources) -> chain start, pin facing into the net.
const SOURCE1 = new Set([
  'ConstantForce',
  'ConstantTorque',
  'ConstantRotSpeed',
  'ConstantSpeed',
  'ConstantPosition',
]);
//=============================================================================
interface ResolvedPin extends SchematicPin {
  net: number;
}
/**
 * Compute a circuit-schematic layout for an acausal model.
 * @param model blocks (with resolved physical pins), physical nets, signal edges
 * @param options optional geometry overrides
 * @returns map blockId -> { x, y, rotation }; only acausal blocks and the causal
 *          signal blocks that decorate them are placed (every block in the model).
 */
export function computeSchematicLayout(
  model: SchematicModel,
  options: SchematicLayoutOptions = {}
): Map<string, Placement> {
  const GRID = options.grid ?? 10;
  const COLW = options.colWidth ?? 200;
  const X0 = options.originX ?? 200;
  const Y_LOAD = options.loadY ?? 90;
  const Y_RAIL = options.railY ?? 250;
  const Y_BOT = options.bottomY ?? 370;
  const Y_SENS = options.sensorY ?? 490;
  const snap = (v: number) => Math.round(v / GRID) * GRID;
  // Vertical (drop/source/EMF) components are 100 tall once rotated, so their bottom pin
  // lands on the reference rail (Y_BOT). A hot pin pointing up and a series pin pointing
  // left only make a clean L when the source pin is below the series rail; on the rail it
  // forces an up-over-down staple.
  const dropTopLine = Y_BOT - 100;

  const blocks = model.blocks;
  const byId = new Map(blocks.map((b) => [b.id, b]));
  const isAcausal = (b: SchematicBlock) => b.acausal || b.pins.length > 0;

  const nets = (model.physicalWires ?? []).map((w) => w.pins.slice());
  const pinNet = new Map<string, number>();
  nets.forEach((pins, i) => pins.forEach((k) => pinNet.set(k, i)));
  const netOf = (id: string, pin: string) => pinNet.get(id + '.' + pin) ?? -1;

  const compPins = new Map<string, ResolvedPin[]>();
  for (const b of blocks) {
    if (!isAcausal(b)) continue;
    compPins.set(
      b.id,
      b.pins.map((p) => ({ ...p, net: netOf(b.id, p.name) }))
    );
  }

  const refNet = new Set<number>();
  for (const b of blocks) {
    if (!REF_TYPES.has(b.type)) continue;
    for (const p of compPins.get(b.id) ?? []) if (p.net >= 0) refNet.add(p.net);
  }

  // net adjacency (a component bridging two distinct nets)
  const adj = new Map<number, Set<number>>();
  const ensure = (n: number) => {
    let s = adj.get(n);
    if (!s) {
      s = new Set();
      adj.set(n, s);
    }
    return s;
  };
  nets.forEach((_, i) => ensure(i));
  for (const b of blocks) {
    const ps = (compPins.get(b.id) ?? []).filter((p) => p.net >= 0);
    for (let i = 0; i < ps.length; i += 1)
      for (let j = i + 1; j < ps.length; j += 1)
        if (ps[i].net !== ps[j].net) {
          ensure(ps[i].net).add(ps[j].net);
          ensure(ps[j].net).add(ps[i].net);
        }
  }

  // root net: a source/anchor's (non-reference) net, else the lowest-degree net
  let root = -1;
  for (const b of blocks) {
    if (SOURCE2.has(b.type) || SOURCE1.has(b.type) || ANCHOR1.has(b.type))
      for (const p of compPins.get(b.id) ?? [])
        if (p.net >= 0 && !refNet.has(p.net)) {
          root = p.net;
          break;
        }
    if (root >= 0) break;
  }
  if (root < 0) {
    let bestDeg = Infinity;
    nets.forEach((_, i) => {
      if (!refNet.has(i)) {
        const d = ensure(i).size;
        if (d < bestDeg) {
          bestDeg = d;
          root = i;
        }
      }
    });
  }

  // BFS columns for the non-reference nets (reference nets are the bottom rail)
  const col = new Map<number, number>();
  const bfs = (start: number, base: number) => {
    const q = [start];
    col.set(start, base);
    while (q.length) {
      const n = q.shift() as number;
      for (const m of ensure(n))
        if (!refNet.has(m) && !col.has(m)) {
          col.set(m, (col.get(n) as number) + 1);
          q.push(m);
        }
    }
  };
  if (root >= 0) bfs(root, 0);
  nets.forEach((_, i) => {
    if (!refNet.has(i) && !col.has(i)) {
      let mx = 0;
      col.forEach((c) => (mx = Math.max(mx, c)));
      bfs(i, mx + 2);
    }
  });
  const colX = (c: number) => X0 + c * COLW;

  // per-lane slot allocator: keep placed x's apart within a lane
  const used: Record<string, number[]> = { load: [], rail: [], bot: [], sens: [] };
  const place = (lane: keyof typeof used, cx: number, width: number) => {
    let x = cx;
    while (used[lane].some((u) => Math.abs(u - x) < width + 30)) x += width + 40;
    used[lane].push(x);
    return x;
  };

  const pos = new Map<string, Placement>();
  const setPos = (id: string, x: number, y: number, rotation: number) =>
    pos.set(id, { x: snap(x), y: snap(y), rotation });
  const yHoriz = (pinLine: number) => pinLine - 40; // rot 0/180: pin at +40
  const yVertTop = (topLine: number) => topLine + 10; // rot 90/270: top pin at -10
  const refMarkers: Array<{ id: string; net: number }> = [];
  const sensorTaps: Array<{ id: string; net: number }> = [];

  for (const b of blocks) {
    if (!isAcausal(b)) continue;
    const ps = (compPins.get(b.id) as ResolvedPin[]).filter((p) => p.net >= 0);
    const deg = ps.length;
    const nonRef = ps.filter((p) => !refNet.has(p.net));
    const isSource2 = SOURCE2.has(b.type);
    const isSource1 = SOURCE1.has(b.type) || ANCHOR1.has(b.type);
    const isRef = REF_TYPES.has(b.type);
    const isAnchor = ANCHOR1.has(b.type);
    const isSensor = b.hasSignalOut && deg >= 1;

    if (isAnchor && deg === 0) {
      // a floating anchor (declared but not wired) parks out of the way top-left
      setPos(b.id, X0 - COLW, Y_LOAD, 0);
      continue;
    }
    const domains = new Set(b.pins.map((p) => p.domain));
    const isEmf = b.type === 'EMF' || (deg >= 3 && domains.size >= 2);

    if (isEmf) {
      // electromechanical boundary: vertical, electrical hot on top, reference at
      // the bottom, mechanical shaft (flange) leaving to the right.
      const elec = ps.filter((p) => p.domain === 'electrical');
      const hot = elec.find((p) => !refNet.has(p.net)) ?? nonRef[0] ?? ps[0];
      const c = col.get(hot.net) ?? 0;
      const x = place('rail', colX(c) - 50, b.width);
      setPos(b.id, x, yVertTop(dropTopLine), 90);
      continue;
    }
    if (isRef && deg <= 1) {
      // reference marker at the bottom, under the leftmost column it feeds, so the tap onto
      // the return rail is short and straight (SPICE-style ground under the source)
      const neigh = [...(adj.get(ps[0]?.net) ?? [])].filter((n) => col.has(n));
      const c = neigh.length ? Math.min(...neigh.map((n) => col.get(n) as number)) : 0;
      const x = place('bot', colX(c) - 50, b.width);
      setPos(b.id, x, Y_BOT + 30, 90); // pin up toward the rail
      if (ps[0]) refMarkers.push({ id: b.id, net: ps[0].net });
      continue;
    }
    if (deg === 2 && nonRef.length === 2) {
      const [a, bb] = nonRef;
      let leftPin = a;
      let cl = col.get(a.net) as number;
      let cr = col.get(bb.net) as number;
      if (cl > cr) {
        leftPin = bb;
        const t = cl;
        cl = cr;
        cr = t;
      }
      const rot = leftPin.side === 'left' ? 0 : 180; // low-column pin on the left
      const x = place('rail', colX(cl), b.width);
      setPos(b.id, x, yHoriz(Y_RAIL), rot);
      continue;
    }
    if (deg === 2 && nonRef.length === 1) {
      // shunt / source: vertical, non-reference pin up, reference pin down
      const np = nonRef[0];
      const c = col.get(np.net) ?? 0;
      const rot = np.side === 'left' ? 90 : 270;
      if (isSource2)
        setPos(b.id, place('rail', colX(0) - COLW * 0.75, b.width), yVertTop(dropTopLine), rot);
      else setPos(b.id, place('rail', colX(c) - 50, b.width), yVertTop(dropTopLine), rot);
      continue;
    }
    if (deg === 1 && !isRef) {
      const np = ps[0];
      const c = col.get(np.net) ?? 0;
      if (isSource1) {
        const rot = np.side === 'left' ? 180 : 0; // flange faces right into the net
        setPos(b.id, place('rail', colX(c) - COLW, b.width), yHoriz(Y_RAIL), rot);
      } else if (isSensor) {
        const rot = np.side === 'left' ? 90 : 270; // pin up toward the rail
        setPos(b.id, place('sens', colX(c) - 50, b.width), yVertTop(Y_SENS) - 10, rot);
        sensorTaps.push({ id: b.id, net: np.net });
      } else {
        const rot = np.side === 'left' ? 270 : 90;
        // A through-element (bridging two non-reference nets) already sits on this net's
        // column: offset the load so its drop taps the node with a clean L instead of
        // running down the element's edge, which the router hooks around.
        const hasThrough = blocks.some(
          (o) =>
            o.id !== b.id &&
            (compPins.get(o.id) ?? []).some((pp) => pp.net === np.net) &&
            new Set(
              (compPins.get(o.id) ?? [])
                .filter((pp) => pp.net >= 0 && !refNet.has(pp.net))
                .map((pp) => pp.net)
            ).size >= 2
        );
        setPos(b.id, place('load', colX(c) - (hasThrough ? 110 : 50), b.width), Y_LOAD, rot);
      }
      continue;
    }
    // fallback (multi-terminal, single domain): sit on the rail at the min column
    const cmin = ps.length ? Math.min(...ps.map((p) => col.get(p.net) ?? 0)) : 0;
    setPos(b.id, place('rail', colX(cmin), b.width), yHoriz(Y_RAIL), 0);
  }

  // Re-align each reference marker under the leftmost block on its net (the source
  // column), so the tap onto the return rail is short and straight.
  for (const rm of refMarkers) {
    let leftX = Infinity;
    for (const b of blocks) {
      if (b.id === rm.id) continue;
      if (!(compPins.get(b.id) ?? []).some((p) => p.net === rm.net)) continue;
      const bp = pos.get(b.id);
      if (bp) leftX = Math.min(leftX, bp.x);
    }
    const p = pos.get(rm.id);
    if (p && Number.isFinite(leftX)) p.x = snap(leftX);
  }

  // Align each single-terminal sensor under the node block it measures, so its lead is a
  // short drop. Prefer the load it reads (body/inertia/mass) over the through-element,
  // and never stack two sensors on the same column.
  const usedSensorX: number[] = [];
  const byId2 = byId;
  for (const st of sensorTaps) {
    // rank candidate blocks on the net: non-sensor first, then fewest pins
    // (a leaf load beats a 2-terminal through-element), then leftmost.
    let best: { x: number; rank: [number, number, number] } | null = null;
    for (const b of blocks) {
      if (b.id === st.id) continue;
      if (!(compPins.get(b.id) ?? []).some((pp) => pp.net === st.net)) continue;
      const bp = pos.get(b.id);
      if (!bp) continue;
      const isSensorBlk = byId2.get(b.id)?.hasSignalOut ? 1 : 0;
      const rank: [number, number, number] = [isSensorBlk, (compPins.get(b.id) ?? []).length, bp.x];
      const better =
        !best ||
        rank[0] < best.rank[0] ||
        (rank[0] === best.rank[0] &&
          (rank[1] < best.rank[1] || (rank[1] === best.rank[1] && rank[2] < best.rank[2])));
      if (better) best = { x: bp.x, rank };
    }
    const p = pos.get(st.id);
    if (!p || !best) continue;
    let x = best.x;
    while (usedSensorX.some((u) => Math.abs(u - x) < 120)) x += 120;
    usedSensorX.push(x);
    p.x = snap(x);
  }

  // causal signal blocks: sources to the left of their consumer, sinks on the right
  let maxX = -Infinity;
  let minX = Infinity;
  pos.forEach((p, id) => {
    const g = byId.get(id) as SchematicBlock;
    maxX = Math.max(maxX, p.x + g.width);
    minX = Math.min(minX, p.x);
  });
  if (!Number.isFinite(maxX)) {
    maxX = X0;
    minX = X0;
  }
  const conns = model.connections ?? [];
  // Keep the visualization close to the circuit it plots, so the measurement path is a short hop to the scope.
  const sinkX = maxX + 60;
  let sinkRow = 0;
  let srcRow = 0;
  for (const b of blocks) {
    if (isAcausal(b)) continue;
    if (b.hasSignalOut && !b.hasSignalIn) {
      const c = conns.find((e) => e.from === b.id);
      const tgt = c ? pos.get(c.to) : undefined;
      if (tgt) setPos(b.id, tgt.x - 190, tgt.y - 130, 0);
      else setPos(b.id, minX - 200, Y_LOAD + srcRow++ * 120, 0);
    } else {
      // A small sink (toWorkspace) fed by a bottom-lane sensor drops straight below that
      // sensor, so its lead does not cross the other nets' buses. Scopes (large displays)
      // always sit on the right.
      const src = conns.find((e) => e.to === b.id);
      const srcId = src?.from;
      const sp = srcId ? pos.get(srcId) : undefined;
      const isScope = b.type === 'scope' || b.type === 'xyScope';
      // Only when the sensor drives no scope; otherwise the sink stays beside the scope.
      const srcFeedsScope =
        srcId != null &&
        conns.some((e) => {
          const t = byId.get(e.to);
          return e.from === srcId && (t?.type === 'scope' || t?.type === 'xyScope');
        });
      if (!isScope && sp && sp.y >= Y_SENS - 60 && !srcFeedsScope) {
        setPos(b.id, sp.x + 50 - b.width / 2, sp.y + 160, 0);
      } else {
        setPos(b.id, sinkX, Y_RAIL - 120 + sinkRow++ * 170, 0);
      }
    }
  }
  return pos;
}
//=============================================================================
