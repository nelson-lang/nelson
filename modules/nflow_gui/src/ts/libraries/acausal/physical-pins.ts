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
// Acausal physical-pin geometry and markers. A physical pin is UNDIRECTED: a small
// hollow ring on the block edge with NO arrowhead (unlike a directed signal port).
// The module is pure (no rendering framework), so it is unit-testable headless.
// The pin schema matches nflow_acausal_generate_library's `physicalPins`
// ({ name, x, y, side, domain }).
//=============================================================================
export type PinSide = 'left' | 'right' | 'top' | 'bottom';
export type PhysicalPin = {
  name: string;
  x: number; // block-local coordinates
  y: number;
  side: PinSide;
  domain: string;
};
//=============================================================================
// Domain ink colours (shared with the palette icons; undirected markers).
const DOMAIN_INK: Record<string, string> = {
  electrical: '#1A3E8C',
  thermal: '#9C2A1A',
  translational: '#8C5A08',
  rotational: '#1E6B2E',
  planar: '#3C3489',
};
//=============================================================================
export function pinInk(domain: string): string {
  return DOMAIN_INK[domain] ?? '#333333';
}
//=============================================================================
export function pinWorldPosition(
  pin: Pick<PhysicalPin, 'x' | 'y'>,
  blockX: number,
  blockY: number
): { x: number; y: number } {
  return { x: blockX + pin.x, y: blockY + pin.y };
}
//=============================================================================
// Nearest pin to a world point within `radius`, or null. Used for
// click-to-connect: physical wiring starts by grabbing the closest pin.
export function hitTestPin(
  pins: PhysicalPin[],
  blockX: number,
  blockY: number,
  px: number,
  py: number,
  radius = 8
): PhysicalPin | null {
  let best: PhysicalPin | null = null;
  let bestD2 = radius * radius;
  for (const p of pins) {
    const w = pinWorldPosition(p, blockX, blockY);
    const dx = w.x - px;
    const dy = w.y - py;
    const d2 = dx * dx + dy * dy;
    if (d2 <= bestD2) {
      bestD2 = d2;
      best = p;
    }
  }
  return best;
}
//=============================================================================
// Auto-distribute pins with no explicit coordinates evenly along a block edge.
// Left/right pins spread in y; top/bottom pins spread in x.
export function layoutDefaultPins(
  names: string[],
  width: number,
  height: number,
  side: PinSide,
  domain: string
): PhysicalPin[] {
  const n = names.length;
  const along = (i: number, span: number) =>
    n === 1 ? span / 2 : span * (0.2 + (0.6 * i) / (n - 1));
  return names.map((name, i) => {
    if (side === 'left' || side === 'right') {
      return { name, x: side === 'left' ? 0 : width, y: along(i, height), side, domain };
    }
    return { name, x: along(i, width), y: side === 'top' ? 0 : height, side, domain };
  });
}
//=============================================================================
// Point on a block edge for a fractional position (0..1) along that edge.
// Left/right vary y; top/bottom vary x.
function edgePoint(
  frac: number,
  width: number,
  height: number,
  side: PinSide
): { x: number; y: number } {
  if (side === 'left') return { x: 0, y: height * frac };
  if (side === 'right') return { x: width, y: height * frac };
  if (side === 'top') return { x: width * frac, y: 0 };
  return { x: width * frac, y: height };
}
//=============================================================================
// Place a side that carries BOTH manifest pins and per-instance frames. Frames are
// the attach points for joints/springs (the kinematic trunk), so they take the
// centre of the edge: a lone frame lands dead-centre and lines up with the centred
// pins of the blocks it connects to (straight wire). Manifest pins (e.g. a
// PlanarBody's "com") yield to the margins, filling outward from the 0.2/0.8 marks
// and alternating top/bottom, so no two pins stack on one point.
function layoutMixedSide(
  frameNames: string[],
  manifestNames: string[],
  width: number,
  height: number,
  side: PinSide
): Map<string, { x: number; y: number }> {
  const pos = new Map<string, { x: number; y: number }>();
  const F = frameNames.length;
  frameNames.forEach((name, i) => {
    const frac = F === 1 ? 0.5 : 0.32 + (0.36 * i) / (F - 1);
    pos.set(name, edgePoint(frac, width, height, side));
  });
  manifestNames.forEach((name, k) => {
    const ring = Math.floor(k / 2); // 0 = the 0.2/0.8 marks, then step outward
    const raw = k % 2 === 0 ? 0.2 - 0.12 * ring : 0.8 + 0.12 * ring;
    const frac = Math.min(0.94, Math.max(0.06, raw));
    pos.set(name, edgePoint(frac, width, height, side));
  });
  return pos;
}
//=============================================================================
// A PlanarBody (and any acausal block) can declare per-INSTANCE named frames in
// `params.frames` ({ name: [dx, dy] } offsets in body-local model units, +y up).
// Frames are attach points for joints/springs and must be connectable physical
// pins, otherwise a wire to "<id>.<frame>" cannot round-trip through save/Run and
// the engine reports it dangling.
// This combines a block's manifest physical pins with its frames into one
// non-overlapping pin set. Each frame goes on the edge its offset points to
// (dominant axis). A side with only frames or only manifest pins is spread evenly
// (a lone pin stays centred). A side with both centres the frame and pushes the
// manifest pin to the margin, so frame-to-frame wires stay straight. Positions are
// visual only (wiring is by pin name). The returned ORDER is unchanged so the pin
// index stays stable across load/save.
export function layoutPhysicalPinsWithFrames(
  manifestPins: PhysicalPin[],
  frames: Record<string, unknown>,
  width: number,
  height: number,
  domain: string
): PhysicalPin[] {
  const bySide: Record<PinSide, string[]> = { left: [], right: [], top: [], bottom: [] };
  const domainOf = new Map<string, string>();
  const isFrame = new Set<string>();
  for (const p of manifestPins) {
    bySide[p.side].push(p.name);
    domainOf.set(p.name, p.domain);
  }
  const sideOf = (dx: number, dy: number): PinSide => {
    if (Math.abs(dx) >= Math.abs(dy)) return dx < 0 ? 'left' : 'right';
    return dy > 0 ? 'top' : 'bottom'; // model +y is up -> the top edge
  };
  for (const [name, off] of Object.entries(frames)) {
    const dx = Array.isArray(off) ? Number((off as unknown[])[0]) || 0 : 0;
    const dy = Array.isArray(off) ? Number((off as unknown[])[1]) || 0 : 0;
    bySide[sideOf(dx, dy)].push(name);
    domainOf.set(name, domain);
    isFrame.add(name);
  }
  const pins: PhysicalPin[] = [];
  for (const side of ['left', 'right', 'top', 'bottom'] as PinSide[]) {
    const names = bySide[side];
    if (names.length === 0) continue;
    const frameNames = names.filter((n) => isFrame.has(n));
    const manifestNames = names.filter((n) => !isFrame.has(n));
    if (frameNames.length > 0 && manifestNames.length > 0) {
      // Mixed side: keep the array order (index stability), reposition by role.
      const pos = layoutMixedSide(frameNames, manifestNames, width, height, side);
      for (const name of names) {
        const p = pos.get(name) as { x: number; y: number };
        pins.push({ name, x: p.x, y: p.y, side, domain: domainOf.get(name) ?? domain });
      }
    } else {
      // Only frames, or only manifest pins: even spread (a lone pin is centred).
      for (const p of layoutDefaultPins(names, width, height, side, domain)) {
        pins.push({ ...p, domain: domainOf.get(p.name) ?? domain });
      }
    }
  }
  return pins;
}
//=============================================================================
// SVG marker for one undirected physical pin: a hollow ring (fill = surface,
// stroke = domain ink) with no arrowhead. Returns an SVG fragment string.
export function physicalPinMarkerSvg(pin: PhysicalPin, r = 3.5): string {
  const ink = pinInk(pin.domain);
  return (
    `<circle class="physical-pin" data-pin="${pin.name}" data-domain="${pin.domain}" ` +
    `cx="${pin.x}" cy="${pin.y}" r="${r}" fill="var(--surface-2, #fff)" ` +
    `stroke="${ink}" stroke-width="1.5"/>`
  );
}
//=============================================================================
// True for a block from the acausal library (`acausal: true`); signal ports live
// in inputs/outputs instead.
export function isPhysicalPinBlock(block: { acausal?: boolean } | undefined): boolean {
  return !!block && block.acausal === true;
}
//=============================================================================
// Minimal structural types so this stays framework-free (unit-testable in
// jsdom): the renderer injects its own createSvgElement + block group.
export type SvgFactory = (
  tag: string,
  attrs: Record<string, string | number>
) => { appendChild?: (child: unknown) => void } & Record<string, unknown>;
//=============================================================================
export type RenderablePort = {
  name: string;
  type: 'phys';
  index: number; // 0..P-1, matches the manifest physicalPins order (and fromIndex/toIndex)
  domain: string;
  x: number;
  y: number;
  wireX: number;
  wireY: number;
  el: unknown;
};
//=============================================================================
// Render a block's physical pins into its SVG group AND register them as
// connectable ports on `block.ports` (type 'phys') for the wire logic. Returns the
// created elements. Framework-free: the renderer passes its createSvgElement + group.
export function appendPhysicalPins(
  group: { appendChild: (child: unknown) => void },
  block: { ports: RenderablePort[] },
  pins: PhysicalPin[],
  createSvgElement: SvgFactory,
  r = 3.5
): unknown[] {
  const els: unknown[] = [];
  let index = 0;
  for (const pin of pins) {
    const el = createSvgElement('circle', {
      cx: pin.x,
      cy: pin.y,
      r,
      class: 'physical-pin',
      'data-pin': pin.name,
      'data-domain': pin.domain,
      fill: 'var(--surface-2, #fff)',
      stroke: pinInk(pin.domain),
      'stroke-width': 1.5,
    });
    group.appendChild(el);
    block.ports.push({
      name: pin.name,
      type: 'phys',
      index: index++,
      domain: pin.domain,
      x: pin.x,
      y: pin.y,
      wireX: pin.x,
      wireY: pin.y,
      el,
    });
    els.push(el);
  }
  return els;
}
//=============================================================================
