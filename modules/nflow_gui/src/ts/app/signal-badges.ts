//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Signal value badges: after a simulation run the engine reports the final value
// of every output port; hovering a wire shows the value it carries.
//=============================================================================
import { state } from './state.js';
import { renderSignalBadgeView } from './components/signal-badge-view.js';
//=============================================================================
/** A port value: scalar wire = number, vector/matrix wire = number[]. */
export type SignalPortValue = number | number[];
let values: Record<string, SignalPortValue[]> = {};
let types: Record<string, string[]> = {};
let dims: Record<string, Array<[number, number]>> = {};
let complexFlags: Record<string, boolean[]> = {};
let busInfos: Record<string, Array<unknown | null>> = {};
let badgeVisible = false;
let badgeX = 0;
let badgeY = 0;
let badgeText = '';
export function setSignalValues(
  signals: Record<string, SignalPortValue[]> | null | undefined,
  signalTypes?: Record<string, string[]> | null,
  signalDims?: Record<string, Array<[number, number]>> | null,
  signalComplex?: Record<string, boolean[]> | null,
  signalBus?: Record<string, Array<unknown | null>> | null
): void {
  values = signals && typeof signals === 'object' ? signals : {};
  types = signalTypes && typeof signalTypes === 'object' ? signalTypes : {};
  dims = signalDims && typeof signalDims === 'object' ? signalDims : {};
  complexFlags = signalComplex && typeof signalComplex === 'object' ? signalComplex : {};
  busInfos = signalBus && typeof signalBus === 'object' ? signalBus : {};
}
//=============================================================================
export function clearSignalValues(): void {
  values = {};
  types = {};
  dims = {};
  complexFlags = {};
  busInfos = {};
  clearVectorWireStyles();
}
//=============================================================================
/** Bus descriptor of the wire from (block, port), or null for plain wires. */
export function getSignalBusInfo(from: unknown, fromIndex: unknown): unknown | null {
  const arr = busInfos[String(from)];
  if (!Array.isArray(arr)) return null;
  const b = arr[Number(fromIndex ?? 0)];
  return b && typeof b === 'object' ? b : null;
}
//=============================================================================
/** Whether the wire from (block, port) carries a complex signal. */
export function isSignalComplex(from: unknown, fromIndex: unknown): boolean {
  const arr = complexFlags[String(from)];
  if (!Array.isArray(arr)) return false;
  return arr[Number(fromIndex ?? 0)] === true;
}
//=============================================================================
/** Element count of the wire from (block, port); 1 for scalar/unknown. */
export function getSignalWidth(from: unknown, fromIndex: unknown): number {
  const arr = dims[String(from)];
  if (!Array.isArray(arr)) return 1;
  const d = arr[Number(fromIndex ?? 0)];
  if (Array.isArray(d) && d.length >= 2) {
    // Product over every dimension (N-D signals emit [d1, d2, d3, ...]).
    let w = 1;
    for (const e of d) w *= Number(e) || 1;
    return Math.max(1, w);
  }
  return 1;
}
/** Shape label of the wire from (block, port): "3" for a vector, "2x3" for a
 *  matrix, "2x3x2" for N-D; '' for scalar/unknown. Trailing singleton
 *  dimensions are dropped. */
export function getSignalShapeLabel(from: unknown, fromIndex: unknown): string {
  const arr = dims[String(from)];
  if (!Array.isArray(arr)) return '';
  const d = arr[Number(fromIndex ?? 0)];
  if (!Array.isArray(d) || d.length < 1) return '';
  const shape = d.map((e) => Math.max(1, Number(e) || 1));
  while (shape.length > 1 && shape[shape.length - 1] === 1) shape.pop();
  if (shape.length === 1 && shape[0] <= 1) return '';
  return shape.join('x');
}
/** Toggle the .wire-vector class on each connection whose source signal is a
 *  vector/matrix (width > 1). Called after a simulation resolves dimensions. */
export function applyVectorWireStyles(): void {
  const conns = state.connections as Array<{
    path?: Element | null;
    from?: unknown;
    fromIndex?: unknown;
  }>;
  for (const conn of conns) {
    const el = conn.path as (Element & { classList?: DOMTokenList }) | null;
    if (!el || !el.classList) continue;
    const isBus = getSignalBusInfo(conn.from, conn.fromIndex) !== null;
    el.classList.toggle('wire-bus', isBus);
    el.classList.toggle('wire-vector', !isBus && getSignalWidth(conn.from, conn.fromIndex) > 1);
  }
}
//=============================================================================
export function clearVectorWireStyles(): void {
  const conns = state.connections as Array<{ path?: Element | null }>;
  for (const conn of conns) {
    const el = conn.path as (Element & { classList?: DOMTokenList }) | null;
    el?.classList?.remove('wire-vector');
    el?.classList?.remove('wire-bus');
  }
}
//=============================================================================
/** Signal type name of a wire ("int8", "boolean", ...), '' for double/unknown. */
export function getSignalTypeName(from: unknown, fromIndex: unknown): string {
  const arr = types[String(from)];
  if (!Array.isArray(arr)) return '';
  const t = arr[Number(fromIndex ?? 0)];
  return typeof t === 'string' && t !== 'double' ? t : '';
}
//=============================================================================
export function getSignalValue(from: unknown, fromIndex: unknown): SignalPortValue | null {
  const arr = values[String(from)];
  if (!Array.isArray(arr)) return null;
  const v = arr[Number(fromIndex ?? 0)];
  if (typeof v === 'number') return Number.isFinite(v) ? v : null;
  if (Array.isArray(v)) return v;
  return null;
}
//=============================================================================
function formatScalar(v: number): string {
  if (Math.abs(v) >= 1e4 || (Math.abs(v) > 0 && Math.abs(v) < 1e-3)) {
    return v.toExponential(3);
  }
  return String(Math.round(v * 1e6) / 1e6);
}
//=============================================================================
export function formatSignalValue(v: SignalPortValue): string {
  if (Array.isArray(v)) {
    const maxShown = 6;
    const shown = v.slice(0, maxShown).map(formatScalar).join(', ');
    const suffix = v.length > maxShown ? `, … (${v.length})` : '';
    return `[${shown}${suffix}]`;
  }
  return formatScalar(v);
}
//=============================================================================
function renderBadge(): void {
  renderSignalBadgeView({
    visible: badgeVisible,
    x: badgeX,
    y: badgeY,
    text: badgeText,
  });
}
//=============================================================================
function hideBadge(): void {
  if (!badgeVisible) return;
  badgeVisible = false;
  renderBadge();
}
//=============================================================================
function findConnectionByPath(target: Element): { from?: unknown; fromIndex?: unknown } | null {
  for (const conn of state.connections as Array<{ path?: unknown; hitPath?: unknown }>) {
    if (conn.path === target || conn.hitPath === target) {
      return conn as { from?: unknown; fromIndex?: unknown };
    }
  }
  return null;
}
//=============================================================================
/** Attach wire hover badges. Idempotent. */
export function initSignalBadges(svg: SVGSVGElement | null): void {
  if (!svg || (svg as unknown as { _signalBadgesBound?: boolean })._signalBadgesBound) return;
  (svg as unknown as { _signalBadgesBound?: boolean })._signalBadgesBound = true;

  svg.addEventListener('pointerover', (ev) => {
    const target = ev.target as Element | null;
    if (!target || !target.closest('#wireLayer')) return;
    const conn = findConnectionByPath(target);
    if (!conn) return;
    const value = getSignalValue(conn.from, conn.fromIndex);
    if (value === null) return;
    badgeText = formatSignalValue(value);
    const bus = getSignalBusInfo(conn.from, conn.fromIndex) as {
      name?: string;
      members?: Array<{ name?: string }>;
    } | null;
    if (bus && Array.isArray(bus.members)) {
      const memberNames = bus.members.map((m) => m?.name ?? '?').join(', ');
      const busLabel = bus.name ? `bus ${bus.name}` : 'bus';
      badgeText += ` (${busLabel} {${memberNames}})`;
    } else {
      const complexLabel = isSignalComplex(conn.from, conn.fromIndex) ? 'complex' : '';
      const typeName = getSignalTypeName(conn.from, conn.fromIndex);
      const shape = getSignalShapeLabel(conn.from, conn.fromIndex);
      if (typeName || shape || complexLabel) {
        const dimLabel = shape ? `[${shape}]` : '';
        badgeText += ` (${[complexLabel, typeName, dimLabel].filter(Boolean).join(' ')})`;
      }
    }
    badgeX = ev.clientX;
    badgeY = ev.clientY;
    badgeVisible = true;
    renderBadge();
  });
  svg.addEventListener('pointermove', (ev) => {
    if (!badgeVisible) return;
    if (!(ev.target as Element | null)?.closest?.('#wireLayer')) {
      hideBadge();
      return;
    }
    badgeX = ev.clientX;
    badgeY = ev.clientY;
    renderBadge();
  });
  svg.addEventListener('pointerout', (ev) => {
    if ((ev.target as Element | null)?.closest?.('#wireLayer')) {
      hideBadge();
    }
  });
}
//=============================================================================
export default initSignalBadges;
//=============================================================================
