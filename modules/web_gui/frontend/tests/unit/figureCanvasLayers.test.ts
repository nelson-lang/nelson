//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { DrawPrimitive } from '../../src/rpc/types';
import {
  base64Bytes,
  canCreateRgbaImageBitmap,
  figureBaseLayerPrimitives,
  figureImageCacheKeys,
  figureRasterBaseLayerPrimitives,
  figureRasterOverlayPrimitives,
  figureTextOverlayPrimitives,
  isRasterTextureFrameCandidate,
  rgbaImageData,
  requestedFigureCanvasTransferMode,
  requestedFigureCanvas2DRenderer,
  requestedFigureRasterRenderer,
  requestedFigureTextOverlayRenderer,
  shouldRefreshRgbaImage,
} from '../../src/panels/FigureCanvas';
import { bitmapTransferFrame, clonedTransferFrame } from '../../src/panels/figureCanvasTransfer';
import {
  addFigureCanvasValue,
  getFigureCanvasProfile,
  resetFigureCanvasProfile,
} from '../../src/panels/figureCanvasProfile';
import { rasterTextureCacheKey } from '../../src/panels/figureRasterWebGlRenderer';
import { shouldRetryWebGlPresentation } from '../../src/panels/FigureWebGlCanvas';
//=============================================================================
describe('figure canvas layers', () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it('selects the optional 2D canvas worker renderer from query or storage', () => {
    expect(requestedFigureCanvas2DRenderer(null, null)).toBe('auto');
    expect(requestedFigureCanvas2DRenderer('main', 'worker')).toBe('main');
    expect(requestedFigureCanvas2DRenderer('worker', null)).toBe('worker');
    expect(requestedFigureCanvas2DRenderer('unknown', 'main')).toBe('main');
    expect(requestedFigureCanvas2DRenderer('unknown', 'unknown')).toBe('auto');
  });

  it('uses the raster WebGL renderer by default while keeping explicit overrides', () => {
    expect(requestedFigureRasterRenderer(null, null)).toBe('webgl');
    expect(requestedFigureRasterRenderer('webgl', null)).toBe('webgl');
    expect(requestedFigureRasterRenderer('canvas', 'webgl')).toBe('canvas');
    expect(requestedFigureRasterRenderer('unknown', 'webgl')).toBe('webgl');
    expect(requestedFigureRasterRenderer('unknown', 'unknown')).toBe('webgl');
  });

  it('limits the first-frame WebGL presentation retry to non-Chromium engines', () => {
    expect(
      shouldRetryWebGlPresentation(
        'Mozilla/5.0 AppleWebKit/537.36 Chrome/140.0.0.0 Safari/537.36',
        'Google Inc.'
      )
    ).toBe(false);
    expect(
      shouldRetryWebGlPresentation(
        'Mozilla/5.0 AppleWebKit/537.36 Chrome/140.0.0.0 Safari/537.36 Edg/140.0.0.0',
        'Google Inc.'
      )
    ).toBe(false);
    expect(shouldRetryWebGlPresentation('Mozilla/5.0 Gecko/20100101 Firefox/142.0', '')).toBe(true);
    expect(
      shouldRetryWebGlPresentation(
        'Mozilla/5.0 Version/18.0 Safari/605.1.15',
        'Apple Computer, Inc.'
      )
    ).toBe(true);
  });

  it('exposes raster WebGL texture reuse counters to the bench harness', () => {
    window.__nelsonFigureCanvasProfileEnabled = true;
    resetFigureCanvasProfile();

    addFigureCanvasValue('rasterWebGlTextureHits', 3);
    addFigureCanvasValue('rasterWebGlTextureMisses', 1);
    addFigureCanvasValue('rasterWebGlRetainedBytes', 4096);

    expect(getFigureCanvasProfile()).toMatchObject({
      rasterWebGlTextureHits: 3,
      rasterWebGlTextureMisses: 1,
      rasterWebGlRetainedBytes: 4096,
    });
    window.__nelsonFigureCanvasProfileEnabled = false;
  });

  it('keeps worker-painted text overlays opt-in', () => {
    expect(requestedFigureTextOverlayRenderer(null, null)).toBe('main');
    expect(requestedFigureTextOverlayRenderer('worker', null)).toBe('worker');
    expect(requestedFigureTextOverlayRenderer('main', 'worker')).toBe('main');
    expect(requestedFigureTextOverlayRenderer('unknown', 'worker')).toBe('worker');
    expect(requestedFigureTextOverlayRenderer('unknown', 'unknown')).toBe('main');
  });

  it('keeps cloned transferable canvas frames opt-in', () => {
    expect(requestedFigureCanvasTransferMode(null, null)).toBe('structured');
    expect(requestedFigureCanvasTransferMode('clone', null)).toBe('clone');
    expect(requestedFigureCanvasTransferMode('bitmap', null)).toBe('bitmap');
    expect(requestedFigureCanvasTransferMode('structured', 'clone')).toBe('structured');
    expect(requestedFigureCanvasTransferMode('unknown', 'clone')).toBe('clone');
    expect(requestedFigureCanvasTransferMode('unknown', 'unknown')).toBe('structured');
  });

  it('keeps text and its clip state in the WebGL overlay', () => {
    const primitives = [
      { kind: 'rect', x: 0, y: 0, w: 100, h: 80, fill: '#000000' },
      { kind: 'clip', x: 0, y: 0, w: 100, h: 80 },
      {
        kind: 'polygon',
        count: 3,
        pointsB64: '',
        fill: '#111',
      },
      { kind: 'clipSave' },
      { kind: 'text', text: 'Title', x: 50, y: 10, color: '#fff', size: 12, align: 'center' },
      { kind: 'clipRestore' },
    ] as DrawPrimitive[];

    expect(figureTextOverlayPrimitives(primitives).map((primitive) => primitive.kind)).toEqual([
      'clipSave',
      'clip',
      'text',
      'clipRestore',
    ]);
  });

  it('removes text from the base layer when an overlay redraws labels', () => {
    const primitives = [
      { kind: 'rect', x: 0, y: 0, w: 100, h: 80, fill: '#000000' },
      { kind: 'text', text: 'Title', x: 50, y: 10, color: '#fff', size: 12, align: 'center' },
      { kind: 'line', x1: 0, y1: 0, x2: 1, y2: 1, stroke: '#fff', width: 1 },
    ] as DrawPrimitive[];

    expect(figureBaseLayerPrimitives(primitives).map((primitive) => primitive.kind)).toEqual([
      'rect',
      'line',
    ]);
  });

  it('does not duplicate shapes or images above the 3D depth layer', () => {
    const primitives = [
      { kind: 'rect', x: 0, y: 0, w: 10, h: 10, fill: '#000' },
      { kind: 'image', image: 'data:image/png;base64,AA==', x: 0, y: 0, w: 10, h: 10 },
    ] as DrawPrimitive[];

    expect(figureTextOverlayPrimitives(primitives)).toEqual([]);
  });

  it('identifies texture-backed raster animation frames conservatively', () => {
    const primitives = [
      { kind: 'clip', x: 0, y: 0, w: 100, h: 80 },
      {
        kind: 'image',
        x: 0,
        y: 0,
        w: 100,
        h: 80,
        imageWidth: 10,
        imageHeight: 8,
        rgbaBytes: new Uint8Array(10 * 8 * 4).buffer,
      },
      { kind: 'clipSave' },
      { kind: 'text', text: '60 fps', x: 4, y: 12, color: '#fff', size: 12, align: 'left' },
      { kind: 'clipRestore' },
    ] as DrawPrimitive[];

    expect(isRasterTextureFrameCandidate(primitives)).toBe(true);
  });

  it('splits raster frames with vector overlays for the WebGL texture path', () => {
    const primitives = [
      { kind: 'rect', x: 0, y: 0, w: 100, h: 80, fill: '#000' },
      { kind: 'clip', x: 0, y: 0, w: 100, h: 80 },
      {
        kind: 'image',
        x: 0,
        y: 0,
        w: 100,
        h: 80,
        imageWidth: 10,
        imageHeight: 8,
        rgbaBytes: new Uint8Array(10 * 8 * 4).buffer,
      },
      { kind: 'line', x1: 0, y1: 0, x2: 1, y2: 1, stroke: '#fff', width: 1 },
      { kind: 'text', text: '60 fps', x: 4, y: 12, color: '#fff', size: 12, align: 'left' },
    ] as DrawPrimitive[];

    expect(isRasterTextureFrameCandidate(primitives)).toBe(true);
    expect(figureRasterBaseLayerPrimitives(primitives).map((primitive) => primitive.kind)).toEqual([
      'rect',
      'clip',
      'image',
    ]);
    expect(figureRasterOverlayPrimitives(primitives).map((primitive) => primitive.kind)).toEqual([
      'clipSave',
      'clip',
      'line',
      'clipRestore',
      'clipSave',
      'clip',
      'text',
      'clipRestore',
    ]);
  });

  it('rejects raster frames that would reorder vectors below later images', () => {
    const primitives = [
      {
        kind: 'image',
        x: 0,
        y: 0,
        w: 100,
        h: 80,
        imageWidth: 10,
        imageHeight: 8,
        rgbaBytes: new Uint8Array(10 * 8 * 4).buffer,
      },
      { kind: 'line', x1: 0, y1: 0, x2: 1, y2: 1, stroke: '#fff', width: 1 },
      {
        kind: 'image',
        x: 0,
        y: 0,
        w: 100,
        h: 80,
        imageWidth: 10,
        imageHeight: 8,
        rgbaBytes: new Uint8Array(10 * 8 * 4).buffer,
      },
    ] as DrawPrimitive[];

    expect(isRasterTextureFrameCandidate(primitives)).toBe(false);
  });

  it('rejects URL-only images for the raster texture path', () => {
    const primitives = [
      { kind: 'image', image: 'data:image/png;base64,AA==', x: 0, y: 0, w: 10, h: 10 },
    ] as DrawPrimitive[];

    expect(isRasterTextureFrameCandidate(primitives)).toBe(false);
  });

  it('keeps raster WebGL texture cache keys stable across animated pixel revisions', () => {
    const frame = (imageId: string): Extract<DrawPrimitive, { kind: 'image' }> => ({
      kind: 'image',
      imageId,
      x: 10,
      y: 20,
      w: 100,
      h: 80,
      imageWidth: 10,
      imageHeight: 8,
      rgbaBytes: new Uint8Array(10 * 8 * 4).buffer,
    });

    expect(rasterTextureCacheKey(frame('frame-1'), 0)).toBe(
      rasterTextureCacheKey(frame('frame-2'), 0)
    );
    expect(rasterTextureCacheKey(frame('frame-2'), 1)).not.toBe(
      rasterTextureCacheKey(frame('frame-2'), 0)
    );
  });

  it('decodes base64 raster bytes through the portable fallback', () => {
    const bytes = base64Bytes('AQIDBA==');
    expect([...bytes]).toEqual([1, 2, 3, 4]);
  });

  it('uses the native Uint8Array base64 decoder when available', () => {
    const constructor = Uint8Array as Uint8ArrayConstructor & {
      fromBase64?: (encoded: string) => Uint8Array;
    };
    const previous = constructor.fromBase64;
    try {
      constructor.fromBase64 = (encoded: string) =>
        encoded === 'ignored' ? new Uint8Array([5, 6, 7]) : new Uint8Array();
      expect([...base64Bytes('ignored')]).toEqual([5, 6, 7]);
    } finally {
      if (previous) {
        constructor.fromBase64 = previous;
      } else {
        delete constructor.fromBase64;
      }
    }
  });

  it('refreshes anonymous RGBA frames even when the canvas cache key is stable', () => {
    const anonymousFrame = {
      kind: 'image',
      x: 0,
      y: 0,
      w: 2,
      h: 2,
      imageWidth: 1,
      imageHeight: 1,
      rgbaBytes: new Uint8Array([1, 2, 3, 4]).buffer,
    } as Extract<DrawPrimitive, { kind: 'image' }>;
    const retainedFrame = { ...anonymousFrame, imageId: 'stable-frame' };

    expect(shouldRefreshRgbaImage(anonymousFrame, undefined)).toBe(true);
    expect(shouldRefreshRgbaImage(anonymousFrame, 'previous')).toBe(true);
    expect(shouldRefreshRgbaImage(retainedFrame, 'stable-frame')).toBe(false);
    expect(shouldRefreshRgbaImage(retainedFrame, 'old-frame')).toBe(true);
  });

  it('keeps separate cache keys for multiple anonymous RGBA images', () => {
    const primitives = [
      {
        kind: 'image',
        x: 0,
        y: 0,
        w: 2,
        h: 2,
        imageWidth: 1,
        imageHeight: 1,
        rgbaBytes: new Uint8Array([1, 2, 3, 4]).buffer,
      },
      {
        kind: 'image',
        x: 0,
        y: 0,
        w: 2,
        h: 2,
        imageWidth: 1,
        imageHeight: 1,
        rgbaBytes: new Uint8Array([5, 6, 7, 8]).buffer,
      },
    ] as DrawPrimitive[];

    expect(figureImageCacheKeys(primitives)).toEqual([
      'raster:0:1x1:0,0,2,2',
      'raster:1:1x1:0,0,2,2',
    ]);
  });

  it('uses a stable content cache key for identified RGBA images', () => {
    const primitives = [
      {
        kind: 'image',
        imageId: 'sprite-1',
        x: 0,
        y: 0,
        w: 2,
        h: 2,
        imageWidth: 1,
        imageHeight: 1,
        rgbaBytes: new Uint8Array([1, 2, 3, 4]).buffer,
      },
      {
        kind: 'image',
        imageId: 'sprite-1',
        x: 10,
        y: 20,
        w: 4,
        h: 4,
        imageWidth: 1,
        imageHeight: 1,
        rgbaBytes: new Uint8Array([1, 2, 3, 4]).buffer,
      },
    ] as DrawPrimitive[];

    expect(figureImageCacheKeys(primitives)).toEqual([
      'raster:sprite-1:1x1',
      'raster:sprite-1:1x1',
    ]);
  });

  it('deduplicates repeated image URLs through stable cache keys', () => {
    const primitives = [
      { kind: 'image', image: 'data:image/png;base64,AA==', x: 0, y: 0, w: 10, h: 10 },
      { kind: 'line', x1: 0, y1: 0, x2: 1, y2: 1, stroke: '#fff', width: 1 },
      { kind: 'image', image: 'data:image/png;base64,AA==', x: 0, y: 0, w: 10, h: 10 },
    ] as DrawPrimitive[];

    expect([...new Set(figureImageCacheKeys(primitives))]).toEqual(['data:image/png;base64,AA==']);
  });

  it('builds ImageData directly from a transferred RGBA buffer', () => {
    class TestImageData {
      data: Uint8ClampedArray;
      width: number;
      height: number;

      constructor(data: Uint8ClampedArray, width: number, height: number) {
        this.data = data;
        this.width = width;
        this.height = height;
      }
    }
    vi.stubGlobal('ImageData', TestImageData);

    const bytes = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8]);
    const primitive = {
      kind: 'image',
      x: 0,
      y: 0,
      w: 2,
      h: 1,
      imageWidth: 2,
      imageHeight: 1,
      rgbaBytes: bytes.buffer,
    } as Extract<DrawPrimitive, { kind: 'image' }>;

    const imageData = rgbaImageData(primitive);

    expect(imageData?.width).toBe(2);
    expect(imageData?.height).toBe(1);
    expect([...(imageData?.data ?? [])]).toEqual([...bytes]);
  });

  it('clones RGBA buffers before using transferables for the canvas worker', () => {
    const originalBytes = new Uint8Array([1, 2, 3, 4]).buffer;
    const primitives = [
      {
        kind: 'image',
        x: 0,
        y: 0,
        w: 1,
        h: 1,
        imageWidth: 1,
        imageHeight: 1,
        rgbaBytes: originalBytes,
      },
    ] as DrawPrimitive[];

    const frame = clonedTransferFrame(primitives, []);
    const clonedPrimitive = frame.primitives[0] as Extract<DrawPrimitive, { kind: 'image' }>;

    expect(frame.transferList).toHaveLength(1);
    expect(clonedPrimitive.rgbaBytes).not.toBe(originalBytes);
    expect(originalBytes.byteLength).toBe(4);
    expect([...new Uint8Array(clonedPrimitive.rgbaBytes ?? new ArrayBuffer(0))]).toEqual([
      1, 2, 3, 4,
    ]);
  });

  it('can build transferable ImageBitmap frames without detaching RGBA state', async () => {
    class TestImageData {
      constructor(
        readonly data: Uint8ClampedArray,
        readonly width: number,
        readonly height: number
      ) {}
    }
    const bitmap = { close: vi.fn() };
    vi.stubGlobal('ImageData', TestImageData);
    vi.stubGlobal(
      'createImageBitmap',
      vi.fn(() => Promise.resolve(bitmap))
    );
    const originalBytes = new Uint8Array([1, 2, 3, 4]).buffer;
    const primitives = [
      {
        kind: 'image',
        x: 0,
        y: 0,
        w: 1,
        h: 1,
        imageWidth: 1,
        imageHeight: 1,
        rgbaBytes: originalBytes,
      },
    ] as DrawPrimitive[];

    const frame = await bitmapTransferFrame(primitives, []);
    const primitive = frame?.primitives[0] as Extract<DrawPrimitive, { kind: 'image' }>;

    expect(frame?.transferList).toEqual([bitmap]);
    expect(primitive.bitmap).toBe(bitmap);
    expect(primitive.rgbaBytes).toBeUndefined();
    expect(originalBytes.byteLength).toBe(4);
  });

  it('rejects RGBA buffers that do not match the declared dimensions', () => {
    class TestImageData {
      constructor(
        readonly data: Uint8ClampedArray,
        readonly width: number,
        readonly height: number
      ) {}
    }
    vi.stubGlobal('ImageData', TestImageData);

    const primitive = {
      kind: 'image',
      x: 0,
      y: 0,
      w: 2,
      h: 1,
      imageWidth: 2,
      imageHeight: 1,
      rgbaBytes: new Uint8Array([1, 2, 3, 4]).buffer,
    } as Extract<DrawPrimitive, { kind: 'image' }>;

    expect(rgbaImageData(primitive)).toBeNull();
  });

  it('uses the async ImageBitmap path only when browser primitives are available', () => {
    vi.stubGlobal('ImageData', undefined);
    vi.stubGlobal('createImageBitmap', () => Promise.resolve({}));

    expect(canCreateRgbaImageBitmap()).toBe(false);

    vi.stubGlobal(
      'ImageData',
      class TestImageData {
        constructor(
          readonly data: Uint8ClampedArray,
          readonly width: number,
          readonly height: number
        ) {}
      }
    );

    expect(canCreateRgbaImageBitmap()).toBe(true);
  });
});
//=============================================================================
