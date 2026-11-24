//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Marker geometry in screen pixels (y-down, apex up), kept in lockstep with the
// C++ software rasterizer (markerShapeGeometry in RenderWebRaster.cpp) so the
// canvas, the SVG export and the server-side raster draw identical markers. The
// shape ids are RenderInterface::SymbolType values.
export const SYMBOL = {
  Plus: 0,
  Circle: 1,
  Star: 2,
  Dot: 3,
  Times: 4,
  Square: 5,
  Diamond: 6,
  Up: 7,
  Down: 8,
  Right: 9,
  Left: 10,
  Pentagram: 11,
  Hexagram: 12,
  None: 13,
} as const;
//=============================================================================
export type MarkerGeometry =
  | { kind: 'disc' }
  | { kind: 'polygon'; points: [number, number][] }
  | { kind: 'strokes'; segments: [number, number, number, number][] };
// Glyph proportions chosen so a filled marker has a consistent area and bounding
// box for a given SizeData. Values are multiples of the circle radius r. The
// square is smaller than its bounding box, the diamond is a tall rhombus, and the
// triangles are equilateral and anchored at the data point (their centroid,
// circumradius 4/3 r). Kept in step with markerShapeGeometry (RenderWebRaster.cpp)
// and DrawSymbol (graphics module).
const SQUARE_HALF = 0.7778;
const DIAMOND_HALF_W = 1.1021;
const DIAMOND_HALF_H = 1.4685;
const TRI_APEX = 4 / 3;
const TRI_BASE = 2 / 3;
const TRI_HALF = 1.1547005; // 2/sqrt(3)
const HEXAGRAM_INNER = 0.5773503; // 1/sqrt(3)
// Geometry for one marker centred at (cx, cy) with outer radius r.
export function markerGeometry(shape: number, cx: number, cy: number, r: number): MarkerGeometry {
  switch (shape) {
    case SYMBOL.Square: {
      const s = SQUARE_HALF * r;
      return {
        kind: 'polygon',
        points: [
          [cx - s, cy - s],
          [cx + s, cy - s],
          [cx + s, cy + s],
          [cx - s, cy + s],
        ],
      };
    }
    case SYMBOL.Diamond: {
      const dw = DIAMOND_HALF_W * r;
      const dh = DIAMOND_HALF_H * r;
      return {
        kind: 'polygon',
        points: [
          [cx, cy - dh],
          [cx + dw, cy],
          [cx, cy + dh],
          [cx - dw, cy],
        ],
      };
    }
    case SYMBOL.Up: {
      const ax = TRI_HALF * r;
      const ap = TRI_APEX * r;
      const bs = TRI_BASE * r;
      return {
        kind: 'polygon',
        points: [
          [cx, cy - ap],
          [cx + ax, cy + bs],
          [cx - ax, cy + bs],
        ],
      };
    }
    case SYMBOL.Down: {
      const ax = TRI_HALF * r;
      const ap = TRI_APEX * r;
      const bs = TRI_BASE * r;
      return {
        kind: 'polygon',
        points: [
          [cx, cy + ap],
          [cx + ax, cy - bs],
          [cx - ax, cy - bs],
        ],
      };
    }
    case SYMBOL.Left: {
      const ay = TRI_HALF * r;
      const ap = TRI_APEX * r;
      const bs = TRI_BASE * r;
      return {
        kind: 'polygon',
        points: [
          [cx - ap, cy],
          [cx + bs, cy - ay],
          [cx + bs, cy + ay],
        ],
      };
    }
    case SYMBOL.Right: {
      const ay = TRI_HALF * r;
      const ap = TRI_APEX * r;
      const bs = TRI_BASE * r;
      return {
        kind: 'polygon',
        points: [
          [cx + ap, cy],
          [cx - bs, cy - ay],
          [cx - bs, cy + ay],
        ],
      };
    }
    case SYMBOL.Pentagram:
    case SYMBOL.Hexagram: {
      const spikes = shape === SYMBOL.Pentagram ? 5 : 6;
      const inner = r * (shape === SYMBOL.Pentagram ? 0.38 : HEXAGRAM_INNER);
      const points: [number, number][] = [];
      for (let i = 0; i < spikes * 2; i++) {
        const ang = -Math.PI / 2 + (i * Math.PI) / spikes;
        const rad = i % 2 === 0 ? r : inner;
        points.push([cx + rad * Math.cos(ang), cy + rad * Math.sin(ang)]);
      }
      return { kind: 'polygon', points };
    }
    case SYMBOL.Plus:
      return {
        kind: 'strokes',
        segments: [
          [cx - r, cy, cx + r, cy],
          [cx, cy - r, cx, cy + r],
        ],
      };
    case SYMBOL.Times:
      return {
        kind: 'strokes',
        segments: [
          [cx - r, cy - r, cx + r, cy + r],
          [cx + r, cy - r, cx - r, cy + r],
        ],
      };
    case SYMBOL.Star: {
      const d = r / Math.SQRT2;
      return {
        kind: 'strokes',
        segments: [
          [cx - r, cy, cx + r, cy],
          [cx, cy - r, cx, cy + r],
          [cx - d, cy - d, cx + d, cy + d],
          [cx - d, cy + d, cx + d, cy - d],
        ],
      };
    }
    case SYMBOL.Circle:
    case SYMBOL.Dot:
    default:
      return { kind: 'disc' };
  }
}
//=============================================================================
// Decode the 20-byte-per-point marker batch payload into a flat reader.
export interface MarkerBatchPoint {
  x: number;
  y: number;
  r: number;
  fill: string | null;
  edge: string | null;
}
//=============================================================================
const MAX_DECODE_CACHE_ENTRIES = 128;
const pointPairsCache = new Map<string, [number, number][]>();
const rectBatchCache = new Map<string, [number, number, number, number][]>();
const markerBatchCache = new Map<string, MarkerBatchPoint[]>();
function rememberCachedDecode<T>(cache: Map<string, T>, key: string, value: T): T {
  cache.set(key, value);
  if (cache.size > MAX_DECODE_CACHE_ENTRIES) {
    const oldest = cache.keys().next().value;
    if (oldest !== undefined) cache.delete(oldest);
  }
  return value;
}
//=============================================================================
function cachedDecode<T>(cache: Map<string, T>, key: string): T | undefined {
  const value = cache.get(key);
  if (value === undefined) return undefined;
  cache.delete(key);
  cache.set(key, value);
  return value;
}
//=============================================================================
function bytesFromBase64(base64: string, expectedBytes: number): Uint8Array | null {
  let binary: string;
  try {
    binary = atob(base64);
  } catch {
    return null;
  }
  if (binary.length < expectedBytes) {
    return null;
  }
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
  return bytes;
}
//=============================================================================
export function clearFigureMarkerDecodeCaches(): void {
  pointPairsCache.clear();
  rectBatchCache.clear();
  markerBatchCache.clear();
}
//=============================================================================
// Decode a polyline/polygon/segments vertex blob (8 bytes per point: x, y as
// little-endian float32) into a points array. Returns [] on a short/invalid blob.
export function decodePointPairs(base64: string, count: number): [number, number][] {
  const key = `${count}:${base64}`;
  const cached = cachedDecode(pointPairsCache, key);
  if (cached) return cached;
  const bytes = bytesFromBase64(base64, count * 8);
  if (!bytes) return [];
  const view = new DataView(bytes.buffer);
  const out: [number, number][] = new Array(count);
  for (let i = 0; i < count; i++) {
    out[i] = [view.getFloat32(i * 8, true), view.getFloat32(i * 8 + 4, true)];
  }
  return rememberCachedDecode(pointPairsCache, key, out);
}
//=============================================================================
// Decode the 16-byte-per-rect "rects" batch payload (x, y, w, h float32) into a
// flat array. All rects share one fill/outline (carried on the primitive).
export function decodeRectBatch(
  base64: string,
  count: number
): [number, number, number, number][] | null {
  const key = `${count}:${base64}`;
  const cached = cachedDecode(rectBatchCache, key);
  if (cached) return cached;
  const bytes = bytesFromBase64(base64, count * 16);
  if (!bytes) return null;
  const view = new DataView(bytes.buffer);
  const out: [number, number, number, number][] = [];
  for (let i = 0; i < count; i++) {
    const o = i * 16;
    out.push([
      view.getFloat32(o, true),
      view.getFloat32(o + 4, true),
      view.getFloat32(o + 8, true),
      view.getFloat32(o + 12, true),
    ]);
  }
  return rememberCachedDecode(rectBatchCache, key, out);
}
//=============================================================================
export function decodeMarkerBatch(
  base64: string,
  count: number,
  hasFace: boolean,
  hasEdge: boolean
): MarkerBatchPoint[] | null {
  const key = `${count}:${hasFace ? 1 : 0}:${hasEdge ? 1 : 0}:${base64}`;
  const cached = cachedDecode(markerBatchCache, key);
  if (cached) return cached;
  const bytes = bytesFromBase64(base64, count * 20);
  if (!bytes) return null;
  const view = new DataView(bytes.buffer);
  const rgba = (o: number): string =>
    `rgba(${view.getUint8(o)},${view.getUint8(o + 1)},${view.getUint8(o + 2)},${
      view.getUint8(o + 3) / 255
    })`;
  const out: MarkerBatchPoint[] = [];
  for (let i = 0; i < count; i++) {
    const o = i * 20;
    out.push({
      x: view.getFloat32(o, true),
      y: view.getFloat32(o + 4, true),
      r: view.getFloat32(o + 8, true),
      fill: hasFace ? rgba(o + 12) : null,
      edge: hasEdge ? rgba(o + 16) : null,
    });
  }
  return rememberCachedDecode(markerBatchCache, key, out);
}
//=============================================================================
