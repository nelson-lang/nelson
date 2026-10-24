//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Deterministic schematic mini-map of a .nflow model, generated purely from the
// diagram JSON (no renderer, no templates, no async icon loads). Each block is a
// rounded rectangle tinted by a coarse type family; connections reuse the saved
// routing polyline when present, otherwise a straight link between block centers.
// The result is a self-contained <svg> string suitable for an <img src> data URI
// or direct innerHTML, fitted to a padded viewBox. Malformed input yields
// null and the caller shows an icon fallback.
//=============================================================================
interface RawBlock {
  id?: string;
  type?: string;
  x?: number;
  y?: number;
  width?: number;
  height?: number;
}
//=============================================================================
interface RawPoint {
  x?: number;
  y?: number;
}
//=============================================================================
interface RawConnection {
  from?: string;
  to?: string;
  points?: RawPoint[];
}
//=============================================================================
interface RawDiagram {
  blocks?: RawBlock[];
  connections?: RawConnection[];
}
//=============================================================================
const DEFAULT_W = 72;
const DEFAULT_H = 48;
const PAD = 24;
// Coarse type -> CSS variable family. Keeps the palette in sync with the app
// theme (light/dark) instead of hard-coding hex values.
function familyColor(type: string): string {
  const t = (type || '').toLowerCase();
  const has = (...keys: string[]): boolean => keys.some((k) => t.includes(k));
  if (has('scope', 'display', 'sink', 'toworkspace', 'terminator', 'gauge', 'lamp'))
    return 'var(--nf-thumb-sink, #6b7280)';
  if (has('step', 'sine', 'ramp', 'constant', 'source', 'signal', 'fromworkspace', 'clock', 'in'))
    return 'var(--nf-thumb-source, #3b8f5a)';
  if (has('sum', 'gain', 'product', 'math', 'abs', 'trig', 'sqrt', 'add', 'mul'))
    return 'var(--nf-thumb-math, #2f6fb0)';
  if (has('integrator', 'tf', 'transfer', 'statespace', 'derivative', 'pid', 'delay', 'filter'))
    return 'var(--nf-thumb-dynamic, #7a5bd0)';
  if (has('and', 'or', 'not', 'xor', 'logic', 'relational', 'switch', 'compare'))
    return 'var(--nf-thumb-logic, #c98a1a)';
  if (has('pin', 'flange', 'node', 'resistor', 'capacitor', 'inductor', 'mass', 'spring', 'emf'))
    return 'var(--nf-thumb-acausal, #1d9e8a)';
  return 'var(--nf-thumb-default, #8a8f98)';
}
//=============================================================================
function num(value: unknown, fallback = 0): number {
  const n = Number(value);
  return Number.isFinite(n) ? n : fallback;
}
//=============================================================================
function escapeAttr(value: string): string {
  return value
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}
/**
 * Build a schematic mini-map SVG string for a parsed .nflow diagram.
 * Returns null when there are no positioned blocks to draw.
 */
export function renderThumbnailSvg(diagram: unknown, width = 260, height = 150): string | null {
  const doc = (diagram || {}) as RawDiagram;
  const blocks = Array.isArray(doc.blocks) ? doc.blocks : [];
  if (blocks.length === 0) return null;

  const boxes = blocks
    .filter((b) => b && (Number.isFinite(Number(b.x)) || Number.isFinite(Number(b.y))))
    .map((b) => ({
      id: String(b.id ?? ''),
      type: String(b.type ?? ''),
      x: num(b.x),
      y: num(b.y),
      w: Math.max(12, num(b.width, DEFAULT_W)),
      h: Math.max(12, num(b.height, DEFAULT_H)),
    }));
  if (boxes.length === 0) return null;

  const byId = new Map(boxes.map((b) => [b.id, b]));

  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;
  for (const b of boxes) {
    minX = Math.min(minX, b.x);
    minY = Math.min(minY, b.y);
    maxX = Math.max(maxX, b.x + b.w);
    maxY = Math.max(maxY, b.y + b.h);
  }
  const connections = Array.isArray(doc.connections) ? doc.connections : [];
  for (const c of connections) {
    for (const p of c.points || []) {
      if (Number.isFinite(Number(p.x)) && Number.isFinite(Number(p.y))) {
        minX = Math.min(minX, num(p.x));
        minY = Math.min(minY, num(p.y));
        maxX = Math.max(maxX, num(p.x));
        maxY = Math.max(maxY, num(p.y));
      }
    }
  }
  if (!Number.isFinite(minX) || !Number.isFinite(minY)) return null;

  const vbX = minX - PAD;
  const vbY = minY - PAD;
  const vbW = Math.max(1, maxX - minX + PAD * 2);
  const vbH = Math.max(1, maxY - minY + PAD * 2);

  const parts: string[] = [];
  // Connections first, under the blocks.
  for (const c of connections) {
    let pts = (c.points || [])
      .filter((p) => Number.isFinite(Number(p.x)) && Number.isFinite(Number(p.y)))
      .map((p) => `${num(p.x)},${num(p.y)}`);
    if (pts.length < 2) {
      const a = c.from ? byId.get(String(c.from)) : undefined;
      const b = c.to ? byId.get(String(c.to)) : undefined;
      if (a && b) {
        pts = [`${a.x + a.w},${a.y + a.h / 2}`, `${b.x},${b.y + b.h / 2}`];
      }
    }
    if (pts.length >= 2) {
      parts.push(
        `<polyline points="${pts.join(
          ' '
        )}" fill="none" stroke="var(--nf-thumb-wire, #9aa0aa)" stroke-width="2" stroke-linejoin="round" stroke-linecap="round" opacity="0.75"/>`
      );
    }
  }
  for (const b of boxes) {
    const color = familyColor(b.type);
    parts.push(
      `<rect x="${b.x}" y="${b.y}" width="${b.w}" height="${b.h}" rx="6" ry="6" fill="${escapeAttr(
        color
      )}" fill-opacity="0.85" stroke="var(--nf-thumb-stroke, rgba(0,0,0,0.25))" stroke-width="1.5"/>`
    );
  }

  return (
    `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${vbX} ${vbY} ${vbW} ${vbH}" ` +
    `width="${width}" height="${height}" preserveAspectRatio="xMidYMid meet" ` +
    `role="img" aria-label="diagram preview">${parts.join('')}</svg>`
  );
}
//=============================================================================
