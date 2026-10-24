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
// Layered ("Sugiyama") auto-layout for causal signal-flow diagrams.
//
// A left-to-right flow layout produced by the classic four-stage pipeline:
//   1. cycle breaking      - reverse feedback edges (e.g. plant -> sum) so the
//                            graph is acyclic for layering;
//   2. layer assignment    - longest-path layering: sources on the left, each
//                            block one column right of its latest producer;
//   3. ordering            - median heuristic + adjacent-transpose sweeps that
//                            minimise wire crossings, with virtual (dummy) nodes
//                            standing in for edges that span several columns;
//   4. coordinate assignment - a priority method that pulls each block toward the
//                            median of its neighbours (dummies get top priority),
//                            straightening long edges while keeping a minimum gap.
//
// Pure geometry: no DOM, no renderer, no state. `computeAutoLayout` takes the
// block sizes and the connection list and returns new top-left positions. The
// caller (the "Auto-arrange" command, or a headless regeneration script) applies
// them. Disconnected components are laid out independently and stacked; isolated
// blocks (dashboards, unconnected widgets) are packed into a tidy grid instead of
// a degenerate single column.
//=============================================================================
import { GRID_SIZE } from './geometry.js';
//=============================================================================
/** Minimal block description required by the layout. */
export type LayoutNode = {
  id: string;
  width: number;
  height: number;
};
//=============================================================================
/** A directed connection between two blocks (ports are ignored for layering). */
export type LayoutEdge = {
  from: string;
  to: string;
};
//=============================================================================
export type LayoutOptions = {
  /** Snap grid; every returned coordinate is a multiple of this. */
  gridSize?: number;
  /** Horizontal gap between one column's right edge and the next column. */
  layerGap?: number;
  /** Minimum vertical gap between two blocks stacked in the same column. */
  nodeGap?: number;
  /** Top-left margin of the whole drawing. */
  marginX?: number;
  marginY?: number;
  /** Vertical gap between two independent connected components. */
  componentGap?: number;
  /** Ordering-refinement sweeps (crossing minimisation). */
  maxSweeps?: number;
  /** Coordinate-assignment relaxation rounds (edge straightening). */
  coordPasses?: number;
  /** Pull pure sinks (scopes, label taps, terminators) into the last column so
   *  the instruments group on the right and the flow columns stay uncluttered. */
  alignSinksRight?: boolean;
};
//=============================================================================
export type LayoutPositions = Map<string, { x: number; y: number }>;
const DEFAULTS = {
  gridSize: GRID_SIZE,
  layerGap: 80,
  nodeGap: 40,
  marginX: 60,
  marginY: 60,
  componentGap: 80,
  maxSweeps: 8,
  coordPasses: 6,
  alignSinksRight: true,
};
//=============================================================================
type Vertex = {
  id: string; // block id, or a synthetic id for a dummy
  dummy: boolean;
  width: number;
  height: number;
  layer: number;
  order: number;
  center: number; // y of the vertical centre (assigned in stage 4)
  up: number[]; // vertex indices in the previous layer (predecessors, drawn side)
  down: number[]; // vertex indices in the next layer (successors)
};
/**
 * Compute new top-left positions for every block. Blocks absent from `nodes`
 * are ignored; edges referencing unknown blocks are dropped. The result is
 * deterministic: identical inputs (including id order) yield identical output.
 */
export function computeAutoLayout(
  nodes: LayoutNode[],
  edges: LayoutEdge[],
  options: LayoutOptions = {}
): LayoutPositions {
  const opt = { ...DEFAULTS, ...options };
  const positions: LayoutPositions = new Map();
  if (!nodes.length) return positions;

  const nodeById = new Map<string, LayoutNode>();
  for (const n of nodes) nodeById.set(n.id, n);

  // Keep only edges whose endpoints both exist and are not self-loops; collapse
  // parallel edges (they do not change the layered structure).
  const edgeSet = new Set<string>();
  const cleanEdges: LayoutEdge[] = [];
  for (const e of edges) {
    if (!nodeById.has(e.from) || !nodeById.has(e.to) || e.from === e.to) continue;
    const key = `${e.from}${e.to}`;
    if (edgeSet.has(key)) continue;
    edgeSet.add(key);
    cleanEdges.push({ from: e.from, to: e.to });
  }

  const components = connectedComponents(nodes, cleanEdges);

  const laidOut: Array<{ pos: LayoutPositions; width: number; height: number }> = [];
  const singletons: LayoutNode[] = [];
  for (const comp of components) {
    if (comp.length === 1) {
      singletons.push(nodeById.get(comp[0])!);
      continue;
    }
    const compNodes = comp.map((id) => nodeById.get(id)!);
    const compEdges = cleanEdges.filter((e) => comp.includes(e.from));
    laidOut.push(layoutComponent(compNodes, compEdges, opt));
  }

  laidOut.sort((a, b) => b.pos.size - a.pos.size);

  let cursorY = opt.marginY;
  for (const block of laidOut) {
    for (const [id, p] of block.pos) positions.set(id, { x: p.x, y: p.y + cursorY });
    cursorY += block.height + opt.componentGap;
  }

  if (singletons.length) {
    layoutSingletonGrid(singletons, opt, cursorY, positions);
  }

  const heights = new Map<string, number>();
  for (const n of nodes) heights.set(n.id, n.height);
  finalizeToGrid(positions, opt, heights);
  return positions;
}
//=============================================================================
function connectedComponents(nodes: LayoutNode[], edges: LayoutEdge[]): string[][] {
  const adj = new Map<string, Set<string>>();
  for (const n of nodes) adj.set(n.id, new Set());
  for (const e of edges) {
    adj.get(e.from)!.add(e.to);
    adj.get(e.to)!.add(e.from);
  }
  const seen = new Set<string>();
  const comps: string[][] = [];
  for (const n of nodes) {
    if (seen.has(n.id)) continue;
    const stack = [n.id];
    const comp: string[] = [];
    seen.add(n.id);
    while (stack.length) {
      const v = stack.pop()!;
      comp.push(v);
      for (const w of adj.get(v)!) {
        if (!seen.has(w)) {
          seen.add(w);
          stack.push(w);
        }
      }
    }
    comp.sort(); // stable interior order
    comps.push(comp);
  }
  return comps;
}
//=============================================================================
function layoutComponent(
  nodes: LayoutNode[],
  edges: LayoutEdge[],
  opt: Required<LayoutOptions>
): { pos: LayoutPositions; width: number; height: number } {
  const index = new Map<string, number>();
  nodes.forEach((n, i) => index.set(n.id, i));

  const out: number[][] = nodes.map(() => []);
  const inc: number[][] = nodes.map(() => []);
  for (const e of edges) {
    const u = index.get(e.from)!;
    const v = index.get(e.to)!;
    out[u].push(v);
    inc[v].push(u);
  }

  // Stage 1: break cycles by reversing DFS back edges. The DFS starts from the lowest
  // in-degree nodes (true sources), so the flipped edge in a control loop is the real
  // feedback edge (plant -> sum). `reversed[u]` lists the successors of u whose edge
  // was flipped.
  const startOrder = [...Array(nodes.length).keys()].sort(
    (a, b) => inc[a].length - inc[b].length || a - b
  );
  const reversed = breakCycles(nodes.length, out, startOrder);
  const forward: number[][] = nodes.map(() => []);
  for (let u = 0; u < nodes.length; u += 1) {
    for (const v of out[u]) {
      if (reversed[u].has(v)) forward[v].push(u);
      else forward[u].push(v);
    }
  }

  // Stage 2: longest-path layering on the acyclic `forward` graph.
  const layer = longestPathLayers(nodes.length, forward);
  // Group pure sinks (scopes, label taps, terminators) in the last column so
  // the flow columns are not cluttered by instruments that only consume signals.
  if (opt.alignSinksRight) {
    const maxL = Math.max(...layer);
    for (let v = 0; v < nodes.length; v += 1) {
      if (forward[v].length === 0 && layer[v] < maxL) layer[v] = maxL;
    }
  }
  const layerCount = Math.max(...layer) + 1;

  const built = buildVertices(nodes, forward, layer, layerCount);

  // Stage 3: ordering (crossing minimisation).
  orderLayers(built.layers, built.vertices, opt.maxSweeps);

  // Stage 4: coordinate assignment.
  assignCoordinates(built.layers, built.vertices, opt);

  // Column X positions: left-aligned columns, gap between right edge and next.
  const colX: number[] = new Array(built.layers.length).fill(0);
  let x = 0;
  for (let l = 0; l < built.layers.length; l += 1) {
    colX[l] = x;
    let maxW = 0;
    for (const vi of built.layers[l]) maxW = Math.max(maxW, built.vertices[vi].width);
    x += maxW + opt.layerGap;
  }

  const pos: LayoutPositions = new Map();
  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;
  for (const v of built.vertices) {
    if (v.dummy) continue;
    const px = colX[v.layer];
    const py = v.center - v.height / 2;
    pos.set(v.id, { x: px, y: py });
    minX = Math.min(minX, px);
    minY = Math.min(minY, py);
    maxX = Math.max(maxX, px + v.width);
    maxY = Math.max(maxY, py + v.height);
  }
  for (const [id, p] of pos) pos.set(id, { x: p.x - minX, y: p.y - minY });
  return { pos, width: maxX - minX, height: maxY - minY };
}
//=============================================================================
function breakCycles(n: number, out: number[][], startOrder: number[]): Array<Set<number>> {
  const reversed: Array<Set<number>> = Array.from({ length: n }, () => new Set<number>());
  const state = new Uint8Array(n); // 0 = unvisited, 1 = on stack, 2 = done
  // Iterative DFS to avoid recursion limits on large graphs; sources first.
  for (const start of startOrder) {
    if (state[start] !== 0) continue;
    const stack: Array<{ u: number; i: number }> = [{ u: start, i: 0 }];
    state[start] = 1;
    while (stack.length) {
      const top = stack[stack.length - 1];
      if (top.i < out[top.u].length) {
        const v = out[top.u][top.i];
        top.i += 1;
        if (state[v] === 1) {
          reversed[top.u].add(v);
        } else if (state[v] === 0) {
          state[v] = 1;
          stack.push({ u: v, i: 0 });
        }
      } else {
        state[top.u] = 2;
        stack.pop();
      }
    }
  }
  return reversed;
}
//=============================================================================
function longestPathLayers(n: number, forward: number[][]): number[] {
  const inc: number[][] = Array.from({ length: n }, () => []);
  for (let u = 0; u < n; u += 1) for (const v of forward[u]) inc[v].push(u);
  const layer = new Array(n).fill(0);
  // Kahn topological order, then relax layer = max(pred)+1.
  const indeg = inc.map((l) => l.length);
  const queue: number[] = [];
  for (let i = 0; i < n; i += 1) if (indeg[i] === 0) queue.push(i);
  queue.sort((a, b) => a - b);
  const topo: number[] = [];
  while (queue.length) {
    const u = queue.shift()!;
    topo.push(u);
    for (const v of forward[u]) {
      indeg[v] -= 1;
      if (indeg[v] === 0) queue.push(v);
    }
  }
  for (const u of topo) {
    for (const v of forward[u]) {
      if (layer[u] + 1 > layer[v]) layer[v] = layer[u] + 1;
    }
  }
  return layer;
}
//=============================================================================
function buildVertices(
  nodes: LayoutNode[],
  forward: number[][],
  layer: number[],
  layerCount: number
): { vertices: Vertex[]; layers: number[][] } {
  const vertices: Vertex[] = nodes.map((nd, i) => ({
    id: nd.id,
    dummy: false,
    width: nd.width,
    height: nd.height,
    layer: layer[i],
    order: 0,
    center: 0,
    up: [],
    down: [],
  }));

  let dummyCounter = 0;
  const link = (a: number, b: number) => {
    // a is in layer L, b in layer L+1.
    vertices[a].down.push(b);
    vertices[b].up.push(a);
  };
  for (let u = 0; u < nodes.length; u += 1) {
    for (const v of forward[u]) {
      const lu = layer[u];
      const lv = layer[v];
      if (lv <= lu) {
        // Should not happen for forward edges, but guard against it.
        link(u, v);
        continue;
      }
      if (lv - lu === 1) {
        link(u, v);
        continue;
      }
      // Insert a dummy chain u -> d(lu+1) -> ... -> d(lv-1) -> v.
      let prev = u;
      for (let l = lu + 1; l < lv; l += 1) {
        const d: Vertex = {
          id: `__dummy_${dummyCounter++}`,
          dummy: true,
          width: 0,
          height: 0,
          layer: l,
          order: 0,
          center: 0,
          up: [],
          down: [],
        };
        const di = vertices.length;
        vertices.push(d);
        link(prev, di);
        prev = di;
      }
      link(prev, v);
    }
  }

  const layers: number[][] = Array.from({ length: layerCount }, () => []);
  vertices.forEach((v, i) => layers[v.layer].push(i));
  for (const lay of layers) lay.forEach((vi, k) => (vertices[vi].order = k));
  return { vertices, layers };
}
//=============================================================================
function orderLayers(layers: number[][], vertices: Vertex[], maxSweeps: number): void {
  const syncOrder = () => {
    for (const lay of layers) lay.forEach((vi, k) => (vertices[vi].order = k));
  };
  syncOrder();

  let best = layers.map((l) => l.slice());
  let bestCrossings = totalCrossings(layers, vertices);

  for (let sweep = 0; sweep < maxSweeps; sweep += 1) {
    const downward = sweep % 2 === 0;
    if (downward) {
      for (let l = 1; l < layers.length; l += 1) medianSort(layers, vertices, l, true);
    } else {
      for (let l = layers.length - 2; l >= 0; l -= 1) medianSort(layers, vertices, l, false);
    }
    syncOrder();
    transpose(layers, vertices);
    syncOrder();
    const crossings = totalCrossings(layers, vertices);
    if (crossings < bestCrossings) {
      bestCrossings = crossings;
      best = layers.map((l) => l.slice());
      if (crossings === 0) break;
    }
  }

  for (let l = 0; l < layers.length; l += 1) layers[l] = best[l];
  syncOrder();
}
/** Sort one layer by the median position of its neighbours in the adjacent
 *  layer (`useUp` = look at predecessors, else successors). Nodes with no
 *  neighbour keep their relative position (a stable fixed-point). */
function medianSort(layers: number[][], vertices: Vertex[], l: number, useUp: boolean): void {
  const lay = layers[l];
  const medians = new Map<number, number>();
  for (const vi of lay) {
    const neigh = useUp ? vertices[vi].up : vertices[vi].down;
    medians.set(vi, medianOf(neigh.map((ni) => vertices[ni].order)));
  }
  // Keep fixed nodes (median < 0) in place; sort the rest by their median.
  const fixedPositions: Array<{ pos: number; vi: number }> = [];
  const movable: number[] = [];
  lay.forEach((vi, k) => {
    if (medians.get(vi)! < 0) fixedPositions.push({ pos: k, vi });
    else movable.push(vi);
  });
  movable.sort((a, b) => {
    const d = medians.get(a)! - medians.get(b)!;
    return d !== 0 ? d : a - b;
  });
  const result: number[] = new Array(lay.length).fill(-1);
  for (const f of fixedPositions) result[f.pos] = f.vi;
  let mi = 0;
  for (let k = 0; k < result.length; k += 1) {
    if (result[k] === -1) result[k] = movable[mi++];
  }
  layers[l] = result;
}
//=============================================================================
function medianOf(values: number[]): number {
  if (!values.length) return -1;
  const s = values.slice().sort((a, b) => a - b);
  const m = Math.floor(s.length / 2);
  if (s.length % 2 === 1) return s[m];
  if (s.length === 2) return (s[0] + s[1]) / 2;
  // Weighted median (Sugiyama): bias toward the denser side.
  const left = s[m - 1] - s[0];
  const right = s[s.length - 1] - s[m];
  if (left + right === 0) return (s[m - 1] + s[m]) / 2;
  return (s[m - 1] * right + s[m] * left) / (left + right);
}
/** Adjacent-transpose: repeatedly swap neighbours in a layer while it strictly
 *  reduces the number of crossings with both adjacent layers. */
function transpose(layers: number[][], vertices: Vertex[]): void {
  let improved = true;
  let guard = 0;
  while (improved && guard < 4) {
    improved = false;
    guard += 1;
    for (let l = 0; l < layers.length; l += 1) {
      const lay = layers[l];
      for (let i = 0; i < lay.length - 1; i += 1) {
        const before =
          crossingsBetween(layers, vertices, l - 1, l) +
          crossingsBetween(layers, vertices, l, l + 1);
        [lay[i], lay[i + 1]] = [lay[i + 1], lay[i]];
        lay.forEach((vi, k) => (vertices[vi].order = k));
        const after =
          crossingsBetween(layers, vertices, l - 1, l) +
          crossingsBetween(layers, vertices, l, l + 1);
        if (after < before) {
          improved = true;
        } else {
          [lay[i], lay[i + 1]] = [lay[i + 1], lay[i]];
          lay.forEach((vi, k) => (vertices[vi].order = k));
        }
      }
    }
  }
}
//=============================================================================
function totalCrossings(layers: number[][], vertices: Vertex[]): number {
  let total = 0;
  for (let l = 0; l < layers.length - 1; l += 1)
    total += crossingsBetween(layers, vertices, l, l + 1);
  return total;
}
/** Count edge crossings between two adjacent layers (upper `lu`, lower `ll`),
 *  using the standard "sort lower endpoints by upper order, count inversions"
 *  formulation. */
function crossingsBetween(layers: number[][], vertices: Vertex[], lu: number, ll: number): number {
  if (lu < 0 || ll >= layers.length) return 0;
  const upperOrder = new Map<number, number>();
  layers[lu].forEach((vi, k) => upperOrder.set(vi, k));
  const seq: number[] = [];
  for (const lower of layers[ll]) {
    const ups = vertices[lower].up
      .filter((ui) => upperOrder.has(ui))
      .map((ui) => upperOrder.get(ui)!)
      .sort((a, b) => a - b);
    for (const u of ups) seq.push(u);
  }
  return countInversions(seq);
}
//=============================================================================
function countInversions(arr: number[]): number {
  if (arr.length < 2) return 0;
  const tmp = new Array(arr.length);
  const sort = (lo: number, hi: number): number => {
    if (hi - lo < 2) return 0;
    const mid = (lo + hi) >> 1;
    let count = sort(lo, mid) + sort(mid, hi);
    let i = lo;
    let j = mid;
    let k = lo;
    while (i < mid && j < hi) {
      if (arr[i] <= arr[j]) tmp[k++] = arr[i++];
      else {
        tmp[k++] = arr[j++];
        count += mid - i;
      }
    }
    while (i < mid) tmp[k++] = arr[i++];
    while (j < hi) tmp[k++] = arr[j++];
    for (let t = lo; t < hi; t += 1) arr[t] = tmp[t];
    return count;
  };
  return sort(0, arr.length);
}
//=============================================================================
function assignCoordinates(
  layers: number[][],
  vertices: Vertex[],
  opt: Required<LayoutOptions>
): void {
  const gap = opt.nodeGap;
  for (const lay of layers) {
    let y = 0;
    for (const vi of lay) {
      const v = vertices[vi];
      v.center = y + v.height / 2;
      y += v.height + gap;
    }
  }

  const priority = (vi: number): number => {
    const v = vertices[vi];
    if (v.dummy) return Number.MAX_SAFE_INTEGER; // straighten long edges first
    return v.up.length + v.down.length; // busier nodes settle before quiet ones
  };

  for (let pass = 0; pass < opt.coordPasses; pass += 1) {
    const downward = pass % 2 === 0;
    const range = downward
      ? [...Array(layers.length).keys()]
      : [...Array(layers.length).keys()].reverse();
    for (const l of range) {
      alignLayer(layers[l], vertices, downward, priority, gap);
    }
  }
}
/** Move each node of a layer toward the median of its reference-side neighbours,
 *  honouring node priority (dummies win) and the minimum vertical separation. */
function alignLayer(
  lay: number[],
  vertices: Vertex[],
  useUp: boolean,
  priority: (vi: number) => number,
  gap: number
): void {
  const n = lay.length;
  if (n === 0) return;
  const target = new Array(n).fill(NaN);
  for (let k = 0; k < n; k += 1) {
    const v = vertices[lay[k]];
    const neigh = useUp ? v.up : v.down;
    if (neigh.length) {
      const centres = neigh.map((ni) => vertices[ni].center).sort((a, b) => a - b);
      target[k] = median(centres);
    }
  }
  const minSep = new Array(n).fill(0);
  for (let k = 1; k < n; k += 1) {
    minSep[k] = vertices[lay[k - 1]].height / 2 + vertices[lay[k]].height / 2 + gap;
  }
  const prio = lay.map((vi) => priority(vi));
  const orderByPrio = [...Array(n).keys()].sort((a, b) => prio[b] - prio[a] || a - b);

  const center = lay.map((vi) => vertices[vi].center);
  for (const k of orderByPrio) {
    if (Number.isNaN(target[k])) continue;
    if (target[k] > center[k]) {
      // Move down; blocked by the first equal-or-higher-priority node below.
      let need = 0;
      let cap = Infinity;
      for (let j = k + 1; j < n; j += 1) {
        need += minSep[j];
        if (prio[j] >= prio[k]) {
          cap = center[j] - need;
          break;
        }
      }
      const nc = Math.min(target[k], cap);
      if (nc > center[k]) {
        center[k] = nc;
        for (let j = k + 1; j < n; j += 1) {
          const minC = center[j - 1] + minSep[j];
          if (center[j] < minC) center[j] = minC;
          else break;
        }
      }
    } else if (target[k] < center[k]) {
      let need = 0;
      let cap = -Infinity;
      for (let j = k - 1; j >= 0; j -= 1) {
        need += minSep[j + 1];
        if (prio[j] >= prio[k]) {
          cap = center[j] + need;
          break;
        }
      }
      const nc = Math.max(target[k], cap);
      if (nc < center[k]) {
        center[k] = nc;
        for (let j = k - 1; j >= 0; j -= 1) {
          const maxC = center[j + 1] - minSep[j + 1];
          if (center[j] > maxC) center[j] = maxC;
          else break;
        }
      }
    }
  }
  for (let k = 0; k < n; k += 1) vertices[lay[k]].center = center[k];
}
//=============================================================================
function median(sorted: number[]): number {
  const m = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1 ? sorted[m] : (sorted[m - 1] + sorted[m]) / 2;
}
//=============================================================================
function layoutSingletonGrid(
  singletons: LayoutNode[],
  opt: Required<LayoutOptions>,
  startY: number,
  positions: LayoutPositions
): void {
  const sorted = singletons.slice().sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
  const cols = Math.max(1, Math.ceil(Math.sqrt(sorted.length)));
  const cellW = Math.max(...sorted.map((s) => s.width)) + opt.layerGap;
  let col = 0;
  let rowHeight = 0;
  let y = startY;
  for (const s of sorted) {
    const x = opt.marginX + col * cellW;
    positions.set(s.id, { x, y });
    rowHeight = Math.max(rowHeight, s.height);
    col += 1;
    if (col >= cols) {
      col = 0;
      y += rowHeight + opt.nodeGap;
      rowHeight = 0;
    }
  }
}
//=============================================================================
function finalizeToGrid(
  positions: LayoutPositions,
  opt: Required<LayoutOptions>,
  heights: Map<string, number>
): void {
  let minX = Infinity;
  let minY = Infinity;
  for (const p of positions.values()) {
    minX = Math.min(minX, p.x);
    minY = Math.min(minY, p.y);
  }
  const g = opt.gridSize;
  for (const [id, p] of positions) {
    const h = heights.get(id) || 0;
    const x = Math.round((p.x - minX + opt.marginX) / g) * g;
    // Snap the vertical CENTRE to the grid, not the top edge: a block of odd height
    // snapped by its top would put its centred port half a cell off the grid, and a wire
    // between two such blocks would show a small jog.
    const centreY = p.y - minY + opt.marginY + h / 2;
    const y = Math.round(centreY / g) * g - h / 2;
    positions.set(id, { x, y });
  }
}
//=============================================================================
