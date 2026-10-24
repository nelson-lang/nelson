//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DrawPrimitive } from '../rpc/types';
import { base64Bytes } from './figureRasterBytes';
import {
  decodeMarkerBatch,
  decodePointPairs,
  decodeRectBatch,
  markerGeometry,
} from './figureMarkers';
//=============================================================================
// Native renderer: replays the backend RenderWeb draw list on a 2D canvas, in
// list order. The backend already did the projection (same code path as the Qt
// renderer) and emitted the primitives painter-sorted, so honouring that order
// is what makes the web figure match Qt; a split WebGL-geometry + text-overlay
// pass could not interleave rasters (image/imagesc) with the axes at all.
export type FigureCanvas2DContext = CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;
export type CachedImageSource = {
  source: CanvasImageSource;
  sourceId?: string;
  rgbaImageData?: ImageData;
};
//=============================================================================
export function closeCachedImageSource(cached?: CachedImageSource): void {
  const source = cached?.source as { close?: () => void } | undefined;
  source?.close?.();
}
//=============================================================================
export function imageCacheKey(primitive: DrawPrimitive, index = 0): string | null {
  if (primitive.kind !== 'image') return null;
  if (primitive.bitmap || primitive.rgba || primitive.rgbaBytes) {
    if (primitive.imageId) {
      return `raster:${primitive.imageId}:${primitive.imageWidth ?? 0}x${
        primitive.imageHeight ?? 0
      }`;
    }
    return `raster:${index}:${primitive.imageWidth ?? 0}x${primitive.imageHeight ?? 0}:${
      primitive.x
    },${primitive.y},${primitive.w},${primitive.h}`;
  }
  return primitive.imageId ?? primitive.image ?? null;
}
//=============================================================================
export function figureImageCacheKeys(primitives: DrawPrimitive[]): string[] {
  const keys: string[] = [];
  let imageIndex = 0;
  for (const primitive of primitives) {
    if (primitive.kind !== 'image') continue;
    const key = imageCacheKey(primitive, imageIndex);
    imageIndex++;
    if (key) keys.push(key);
  }
  return keys;
}
//=============================================================================
export function shouldRefreshRgbaImage(
  primitive: Extract<DrawPrimitive, { kind: 'image' }>,
  cachedSourceId?: string
): boolean {
  if (!primitive.rgba && !primitive.rgbaBytes) return false;
  return primitive.imageId === undefined || cachedSourceId !== primitive.imageId;
}
//=============================================================================
export { base64Bytes } from './figureRasterBytes';
//=============================================================================
export function rgbaBytes(primitive: Extract<DrawPrimitive, { kind: 'image' }>): Uint8Array | null {
  if (!primitive.rgba && !primitive.rgbaBytes) return null;
  return primitive.rgbaBytes ? new Uint8Array(primitive.rgbaBytes) : base64Bytes(primitive.rgba!);
}
//=============================================================================
export function rgbaImageData(
  primitive: Extract<DrawPrimitive, { kind: 'image' }>
): ImageData | null {
  if (!primitive.imageWidth || !primitive.imageHeight || typeof ImageData === 'undefined') {
    return null;
  }
  const bytes = rgbaBytes(primitive);
  const expected = primitive.imageWidth * primitive.imageHeight * 4;
  if (!bytes || bytes.byteLength !== expected) return null;
  const pixels =
    bytes.buffer instanceof ArrayBuffer
      ? new Uint8ClampedArray(bytes.buffer, bytes.byteOffset, bytes.byteLength)
      : new Uint8ClampedArray(bytes);
  return new ImageData(pixels, primitive.imageWidth, primitive.imageHeight);
}
//=============================================================================
export function canCreateRgbaImageBitmap(): boolean {
  return typeof createImageBitmap === 'function' && typeof ImageData !== 'undefined';
}
//=============================================================================
export function isRasterTextureFrameCandidate(primitives: DrawPrimitive[]): boolean {
  let hasRasterImage = false;
  let overlaySeen = false;
  for (const primitive of primitives) {
    switch (primitive.kind) {
      case 'image':
        if (
          (!primitive.rgba && !primitive.rgbaBytes) ||
          !primitive.imageWidth ||
          !primitive.imageHeight
        ) {
          return false;
        }
        if (overlaySeen) return false;
        hasRasterImage = true;
        break;
      case 'rect':
        if (overlaySeen) return false;
        break;
      case 'clip':
      case 'clipSave':
      case 'clipRestore':
        break;
      case 'line':
      case 'segments':
      case 'polyline':
      case 'polygon':
      case 'points':
      case 'rects':
      case 'text':
        overlaySeen = true;
        break;
      default:
        return false;
    }
  }
  return hasRasterImage;
}
//=============================================================================
export function paintFigure2D(
  ctx: FigureCanvas2DContext,
  primitives: DrawPrimitive[],
  width: number,
  height: number,
  dpr: number,
  images: Map<string, CachedImageSource>,
  panOffset: { dx: number; dy: number } | null
): void {
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, width, height);
  ctx.lineJoin = 'round';
  ctx.lineCap = 'butt';

  // Clip state, mirroring RenderQt's QPainter: `clip` REPLACES the current clip
  // rect (bounding axes children to the axes box), while clipSave/clipRestore
  // bracket direct-draw so widening the clip to the whole figure for
  // ticks/labels is undone afterwards. Canvas clip only intersects, so a replace
  // is emulated by restoring to the base and re-applying the single current rect.
  type Rect = [number, number, number, number];
  const rectStack: (Rect | null)[] = [];
  let clipRect: Rect | null = null;
  let clipSaved = false;
  const applyClip = () => {
    if (clipSaved) {
      ctx.restore();
      clipSaved = false;
    }
    if (clipRect) {
      ctx.save();
      clipSaved = true;
      ctx.beginPath();
      ctx.rect(clipRect[0], clipRect[1], clipRect[2], clipRect[3]);
      ctx.clip();
      // Pan preview: translate only the axes-box content (a narrow clip, the
      // plot area, not the full-figure clip used for ticks/labels), so the data
      // slides under the cursor while the box, ticks and labels stay put, just
      // the crisp server re-render lands on mouse-up.
      if (panOffset && clipRect[2] < width - 1) {
        ctx.translate(panOffset.dx, panOffset.dy);
      }
    }
  };

  const trace = (points: [number, number][]) => {
    ctx.beginPath();
    ctx.moveTo(points[0][0], points[0][1]);
    for (let i = 1; i < points.length; i++) ctx.lineTo(points[i][0], points[i][1]);
  };

  let imageIndex = 0;
  for (const p of primitives) {
    switch (p.kind) {
      case 'rect':
        ctx.fillStyle = p.fill;
        ctx.fillRect(p.x, p.y, p.w, p.h);
        break;
      case 'image': {
        const key = imageCacheKey(p, imageIndex);
        imageIndex++;
        const img = key ? images.get(key)?.source : null;
        if (img) {
          // Sample data is shown as discrete cells,
          // so keep the raster crisp rather than smoothed.
          ctx.imageSmoothingEnabled = false;
          ctx.drawImage(img, p.x, p.y, p.w, p.h);
          ctx.imageSmoothingEnabled = true;
        }
        break;
      }
      case 'polygon': {
        const pts = decodePointPairs(p.pointsB64, p.count);
        if (pts.length < 3) break;
        trace(pts);
        ctx.closePath();
        ctx.fillStyle = p.fill;
        ctx.fill();
        if (p.stroke) {
          ctx.strokeStyle = p.stroke;
          ctx.lineWidth = p.width || 1;
          ctx.stroke();
        }
        break;
      }
      case 'line':
        ctx.beginPath();
        ctx.moveTo(p.x1, p.y1);
        ctx.lineTo(p.x2, p.y2);
        ctx.strokeStyle = p.stroke;
        ctx.lineWidth = p.width || 1;
        ctx.setLineDash(p.dash ?? []);
        ctx.stroke();
        ctx.setLineDash([]);
        break;
      case 'segments': {
        const pts = decodePointPairs(p.pointsB64, p.count);
        if (pts.length < 2) break;
        ctx.beginPath();
        for (let i = 0; i + 1 < pts.length; i += 2) {
          ctx.moveTo(pts[i][0], pts[i][1]);
          ctx.lineTo(pts[i + 1][0], pts[i + 1][1]);
        }
        ctx.strokeStyle = p.stroke;
        ctx.lineWidth = p.width || 1;
        ctx.setLineDash(p.dash ?? []);
        ctx.stroke();
        ctx.setLineDash([]);
        break;
      }
      case 'polyline': {
        const pts = decodePointPairs(p.pointsB64, p.count);
        if (pts.length < 2) break;
        trace(pts);
        ctx.strokeStyle = p.stroke;
        ctx.lineWidth = p.width || 1;
        ctx.setLineDash(p.dash ?? []);
        ctx.stroke();
        ctx.setLineDash([]);
        break;
      }
      case 'text':
        ctx.fillStyle = p.color;
        ctx.font = `${p.italic ? 'italic ' : ''}${p.bold ? 'bold ' : ''}${p.size}px ${
          p.font ?? 'sans-serif'
        }`;
        ctx.textAlign = p.align;
        // The backend anchors text the way Qt does (alignment and descent are
        // already applied), so draw on the alphabetic baseline from (x, y).
        ctx.textBaseline = 'alphabetic';
        if (p.rotation) {
          ctx.save();
          ctx.translate(p.x, p.y);
          ctx.rotate((-p.rotation * Math.PI) / 180);
          ctx.fillText(p.text, 0, 0);
          ctx.restore();
        } else {
          ctx.fillText(p.text, p.x, p.y);
        }
        break;
      case 'points': {
        const points = decodeMarkerBatch(p.data, p.count, p.hasFace, p.hasEdge);
        if (!points) break;
        ctx.lineWidth = p.edgeWidth || 1;
        for (const pt of points) {
          if (pt.r <= 0) continue;
          const geom = markerGeometry(p.shape, pt.x, pt.y, pt.r);
          if (geom.kind === 'disc') {
            ctx.beginPath();
            ctx.arc(pt.x, pt.y, pt.r, 0, Math.PI * 2);
            if (pt.fill) {
              ctx.fillStyle = pt.fill;
              ctx.fill();
            }
            if (pt.edge) {
              ctx.strokeStyle = pt.edge;
              ctx.stroke();
            }
          } else if (geom.kind === 'polygon') {
            trace(geom.points);
            ctx.closePath();
            if (pt.fill) {
              ctx.fillStyle = pt.fill;
              ctx.fill();
            }
            if (pt.edge) {
              ctx.strokeStyle = pt.edge;
              ctx.stroke();
            }
          } else {
            // stroke-only glyph (+, x, *): drawn in the edge colour (fill fallback)
            ctx.strokeStyle = pt.edge ?? pt.fill ?? '#000';
            ctx.beginPath();
            for (const s of geom.segments) {
              ctx.moveTo(s[0], s[1]);
              ctx.lineTo(s[2], s[3]);
            }
            ctx.stroke();
          }
        }
        break;
      }
      case 'rects': {
        const rects = decodeRectBatch(p.data, p.count);
        if (!rects) break;
        if (p.fill) ctx.fillStyle = p.fill;
        if (p.stroke) {
          ctx.strokeStyle = p.stroke;
          ctx.lineWidth = p.width || 1;
        }
        for (const [x, y, w, h] of rects) {
          if (p.fill) ctx.fillRect(x, y, w, h);
          if (p.stroke) ctx.strokeRect(x, y, w, h);
        }
        break;
      }
      case 'clip':
        clipRect = [p.x, p.y, p.w, p.h];
        applyClip();
        break;
      case 'clipSave':
        rectStack.push(clipRect);
        break;
      case 'clipRestore':
        clipRect = rectStack.pop() ?? null;
        applyClip();
        break;
      default:
        break;
    }
  }
  if (clipSaved) ctx.restore();
}
//=============================================================================
export function figureBaseLayerPrimitives(primitives: DrawPrimitive[]): DrawPrimitive[] {
  return primitives.filter((primitive) => primitive.kind !== 'text');
}
//=============================================================================
function isRasterOverlayPrimitive(primitive: DrawPrimitive): boolean {
  return !['image', 'rect', 'clip', 'clipSave', 'clipRestore'].includes(primitive.kind);
}
//=============================================================================
export function figureRasterBaseLayerPrimitives(primitives: DrawPrimitive[]): DrawPrimitive[] {
  return primitives.filter((primitive) => !isRasterOverlayPrimitive(primitive));
}
//=============================================================================
export function figureRasterOverlayPrimitives(primitives: DrawPrimitive[]): DrawPrimitive[] {
  const overlay: DrawPrimitive[] = [];
  type ClipPrimitive = Extract<DrawPrimitive, { kind: 'clip' }>;
  let clipRect: ClipPrimitive | null = null;
  const stack: (ClipPrimitive | null)[] = [];
  const pushWithClip = (primitive: DrawPrimitive) => {
    if (clipRect) {
      overlay.push({ kind: 'clipSave' });
      overlay.push(clipRect);
      overlay.push(primitive);
      overlay.push({ kind: 'clipRestore' });
    } else {
      overlay.push(primitive);
    }
  };
  for (const primitive of primitives) {
    switch (primitive.kind) {
      case 'clip':
        clipRect = primitive;
        break;
      case 'clipSave':
        stack.push(clipRect);
        break;
      case 'clipRestore':
        clipRect = stack.pop() ?? null;
        break;
      default:
        if (isRasterOverlayPrimitive(primitive)) pushWithClip(primitive);
        break;
    }
  }
  return overlay;
}
//=============================================================================
// WebGL owns the 3D depth buffer and is composited above the ordinary 2D
// canvas. Repaint text once above that layer so figure titles and axes labels
// remain annotations instead of being hidden by an opaque surface. Keep only the
// active clip required by each text primitive; replaying the complete clipping
// stream is expensive in animation-heavy figures and does not draw anything.
export function figureTextOverlayPrimitives(primitives: DrawPrimitive[]): DrawPrimitive[] {
  const overlay: DrawPrimitive[] = [];
  type ClipPrimitive = Extract<DrawPrimitive, { kind: 'clip' }>;
  let clipRect: ClipPrimitive | null = null;
  const stack: (ClipPrimitive | null)[] = [];
  for (const primitive of primitives) {
    switch (primitive.kind) {
      case 'clip':
        clipRect = primitive;
        break;
      case 'clipSave':
        stack.push(clipRect);
        break;
      case 'clipRestore':
        clipRect = stack.pop() ?? null;
        break;
      case 'text':
        if (clipRect) {
          overlay.push({ kind: 'clipSave' });
          overlay.push(clipRect);
          overlay.push(primitive);
          overlay.push({ kind: 'clipRestore' });
        } else {
          overlay.push(primitive);
        }
        break;
      default:
        break;
    }
  }
  return overlay;
}
//=============================================================================
