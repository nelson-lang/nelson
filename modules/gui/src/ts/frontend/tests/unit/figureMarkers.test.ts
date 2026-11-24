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
import {
  clearFigureMarkerDecodeCaches,
  decodeMarkerBatch,
  decodePointPairs,
  decodeRectBatch,
} from '../../src/panels/figureMarkers';
//=============================================================================
function bytesToBase64(bytes: Uint8Array): string {
  let binary = '';
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary);
}
//=============================================================================
function floatBytes(values: number[]): Uint8Array {
  const bytes = new Uint8Array(values.length * Float32Array.BYTES_PER_ELEMENT);
  const view = new DataView(bytes.buffer);
  for (let i = 0; i < values.length; i++) {
    view.setFloat32(i * Float32Array.BYTES_PER_ELEMENT, values[i], true);
  }
  return bytes;
}
//=============================================================================
function markerBytes(): Uint8Array {
  const bytes = new Uint8Array(20);
  const view = new DataView(bytes.buffer);
  view.setFloat32(0, 1, true);
  view.setFloat32(4, 2, true);
  view.setFloat32(8, 3, true);
  bytes.set([10, 20, 30, 255], 12);
  bytes.set([40, 50, 60, 128], 16);
  return bytes;
}
//=============================================================================
describe('figure marker decoders', () => {
  afterEach(() => {
    clearFigureMarkerDecodeCaches();
    vi.unstubAllGlobals();
  });

  it('reuses decoded point pairs for repeated display-list paints', () => {
    const encoded = bytesToBase64(floatBytes([1, 2, 3, 4]));
    const originalAtob = globalThis.atob;
    const atobSpy = vi.fn((value: string) => originalAtob(value));
    vi.stubGlobal('atob', atobSpy);

    const first = decodePointPairs(encoded, 2);
    const second = decodePointPairs(encoded, 2);

    expect(second).toBe(first);
    expect(first).toEqual([
      [1, 2],
      [3, 4],
    ]);
    expect(atobSpy).toHaveBeenCalledTimes(1);
  });

  it('reuses decoded rectangle batches independently from point batches', () => {
    const encoded = bytesToBase64(floatBytes([1, 2, 3, 4]));
    const originalAtob = globalThis.atob;
    const atobSpy = vi.fn((value: string) => originalAtob(value));
    vi.stubGlobal('atob', atobSpy);

    const first = decodeRectBatch(encoded, 1);
    const second = decodeRectBatch(encoded, 1);

    expect(second).toBe(first);
    expect(first).toEqual([[1, 2, 3, 4]]);
    expect(atobSpy).toHaveBeenCalledTimes(1);
  });

  it('keeps marker decode cache entries separate by face and edge flags', () => {
    const encoded = bytesToBase64(markerBytes());
    const faceAndEdge = decodeMarkerBatch(encoded, 1, true, true);
    const edgeOnly = decodeMarkerBatch(encoded, 1, false, true);

    expect(decodeMarkerBatch(encoded, 1, true, true)).toBe(faceAndEdge);
    expect(decodeMarkerBatch(encoded, 1, false, true)).toBe(edgeOnly);
    expect(faceAndEdge?.[0].fill).toBe('rgba(10,20,30,1)');
    expect(edgeOnly?.[0].fill).toBeNull();
  });
});
//=============================================================================
