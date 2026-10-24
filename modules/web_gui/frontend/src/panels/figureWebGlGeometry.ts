//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { FigureDisplayListResult, FigureWebGlIndexType, FigureWebGlMesh } from '../rpc/types';
//=============================================================================
type FigureWebGlIndexTypedArray = Uint16Array | Uint32Array;
type FigureWebGlLineColor = [number, number, number, number];
export type FigureWebGlGeometry = {
  revision: string;
  positions: Float32Array;
  vertexColors?: Float32Array;
  triangleIndices: FigureWebGlIndexTypedArray;
  lineIndices: FigureWebGlIndexTypedArray;
  lineColor: FigureWebGlLineColor;
  lineWidth: number;
  drawFaces: boolean;
};
//=============================================================================
const pendingWebGlFrames = new Map<number, FigureWebGlMesh[]>();
export function queueFigureWebGlAnimationFrameForHandle(
  handle: number,
  frame: FigureWebGlMesh[]
): void {
  pendingWebGlFrames.set(handle, frame);
  if (typeof window !== 'undefined') {
    window.dispatchEvent(new CustomEvent(`nelson:figure-webgl-frame:${handle}`, { detail: frame }));
  }
}
//=============================================================================
export function takePendingFigureWebGlAnimationFrame(handle: number): FigureWebGlMesh[] | null {
  const frame = pendingWebGlFrames.get(handle);
  if (!frame) return null;
  pendingWebGlFrames.delete(handle);
  return frame;
}
//=============================================================================
function base64ToBytes(encoded: string): Uint8Array {
  if (typeof globalThis.atob === 'function') {
    const decoded = globalThis.atob(encoded);
    const bytes = new Uint8Array(decoded.length);
    for (let i = 0; i < decoded.length; i++) bytes[i] = decoded.charCodeAt(i);
    return bytes;
  }
  const maybeBuffer = (
    globalThis as unknown as {
      Buffer?: { from(input: string, encoding: 'base64'): Uint8Array };
    }
  ).Buffer;
  if (maybeBuffer) return maybeBuffer.from(encoded, 'base64');
  throw new Error('Cannot decode WebGL mesh buffer');
}
//=============================================================================
function bytesBuffer(bytes: Uint8Array): ArrayBuffer {
  if (
    bytes.byteOffset === 0 &&
    bytes.byteLength === bytes.buffer.byteLength &&
    bytes.buffer instanceof ArrayBuffer
  ) {
    return bytes.buffer;
  }
  const copy = new Uint8Array(bytes.byteLength);
  copy.set(bytes);
  return copy.buffer;
}
//=============================================================================
function decodeFloat32Array(encoded: string): Float32Array {
  return new Float32Array(bytesBuffer(base64ToBytes(encoded)));
}
//=============================================================================
// IEEE 754 half -> float. WebGL mesh positions arrive as normalized float16 to
// halve the payload; decode and un-normalize with the per-axis bounding box to
// restore the float32 coordinates the GPU pipeline expects.
function halfToFloat(h: number): number {
  const sign = (h & 0x8000) >> 15;
  const exp = (h & 0x7c00) >> 10;
  const frac = h & 0x03ff;
  if (exp === 0) return (sign ? -1 : 1) * Math.pow(2, -14) * (frac / 1024);
  if (exp === 0x1f) return frac ? NaN : (sign ? -1 : 1) * Infinity;
  return (sign ? -1 : 1) * Math.pow(2, exp - 15) * (1 + frac / 1024);
}
//=============================================================================
function decodePositionsHalf(encoded: string, bbox: number[]): Float32Array {
  const halves = decodeUint16Array(encoded);
  const out = new Float32Array(halves.length);
  const minX = bbox[0] ?? 0;
  const minY = bbox[1] ?? 0;
  const minZ = bbox[2] ?? 0;
  const extX = (bbox[3] ?? 0) - minX;
  const extY = (bbox[4] ?? 0) - minY;
  const extZ = (bbox[5] ?? 0) - minZ;
  for (let i = 0; i + 2 < out.length; i += 3) {
    out[i] = minX + halfToFloat(halves[i]) * extX;
    out[i + 1] = minY + halfToFloat(halves[i + 1]) * extY;
    out[i + 2] = minZ + halfToFloat(halves[i + 2]) * extZ;
  }
  return out;
}
//=============================================================================
function decodeColorsUint8(encoded: string): Float32Array {
  const bytes = base64ToBytes(encoded);
  const out = new Float32Array(bytes.length);
  for (let i = 0; i < bytes.length; i++) out[i] = bytes[i] / 255;
  return out;
}
//=============================================================================
function decodeUint32Array(encoded: string): Uint32Array {
  return new Uint32Array(bytesBuffer(base64ToBytes(encoded)));
}
//=============================================================================
function decodeUint16Array(encoded: string): Uint16Array {
  return new Uint16Array(bytesBuffer(base64ToBytes(encoded)));
}
//=============================================================================
function toFloat32Array(
  data: number[] | Float32Array | undefined,
  encoded: string | undefined
): Float32Array | null {
  if (encoded !== undefined) return decodeFloat32Array(encoded);
  if (data instanceof Float32Array) return data;
  if (data) return new Float32Array(data);
  return null;
}
//=============================================================================
function toIndexArray(
  data: number[] | FigureWebGlIndexTypedArray | undefined,
  encoded: string | undefined,
  type: FigureWebGlIndexType | undefined
): FigureWebGlIndexTypedArray | null {
  if (encoded !== undefined) {
    return type === 'uint16' ? decodeUint16Array(encoded) : decodeUint32Array(encoded);
  }
  if (data instanceof Uint16Array) return data;
  if (data instanceof Uint32Array) return data;
  if (data) return type === 'uint16' ? new Uint16Array(data) : new Uint32Array(data);
  return null;
}
//=============================================================================
function normalizedGeometry(mesh: FigureWebGlMesh): FigureWebGlGeometry | null {
  const positions =
    mesh.positionsF16 !== undefined && mesh.positionBBox
      ? decodePositionsHalf(mesh.positionsF16, mesh.positionBBox)
      : toFloat32Array(mesh.positions, mesh.positionsBase64);
  const vertexColors =
    mesh.vertexColorsU8 !== undefined
      ? decodeColorsUint8(mesh.vertexColorsU8)
      : toFloat32Array(mesh.vertexColors, mesh.vertexColorsBase64);
  const triangleIndices = toIndexArray(
    mesh.triangleIndices,
    mesh.triangleIndicesBase64,
    mesh.triangleIndexType
  );
  const lineIndices = toIndexArray(mesh.lineIndices, mesh.lineIndicesBase64, mesh.lineIndexType);
  if (!positions || !triangleIndices || !lineIndices) return null;
  if (mesh.drawFaces && !vertexColors) return null;
  return {
    revision: mesh.revision,
    positions,
    vertexColors: vertexColors ?? undefined,
    triangleIndices,
    lineIndices,
    lineColor: mesh.lineColor,
    lineWidth: mesh.lineWidth,
    drawFaces: mesh.drawFaces,
  };
}
//=============================================================================
function meshHasGeometry(mesh: FigureWebGlMesh): boolean {
  return (
    mesh.positions !== undefined ||
    mesh.vertexColors !== undefined ||
    mesh.triangleIndices !== undefined ||
    mesh.lineIndices !== undefined ||
    mesh.positionsBase64 !== undefined ||
    mesh.vertexColorsBase64 !== undefined ||
    mesh.positionsF16 !== undefined ||
    mesh.vertexColorsU8 !== undefined ||
    mesh.triangleIndicesBase64 !== undefined ||
    mesh.lineIndicesBase64 !== undefined
  );
}
//=============================================================================
export function retainFigureWebGlGeometry(
  meshes: FigureWebGlMesh[],
  retained: Map<number, FigureWebGlGeometry>
): FigureWebGlMesh[] {
  const activeIds = new Set(meshes.map((mesh) => mesh.id));
  for (const id of retained.keys()) {
    if (!activeIds.has(id)) {
      retained.delete(id);
    }
  }
  return meshes.map((mesh) => {
    const freshGeometry = normalizedGeometry(mesh);
    if (freshGeometry) {
      retained.set(mesh.id, freshGeometry);
      return {
        ...mesh,
        positions: freshGeometry.positions,
        vertexColors: freshGeometry.vertexColors,
        triangleIndices: freshGeometry.triangleIndices,
        lineIndices: freshGeometry.lineIndices,
        lineColor: freshGeometry.lineColor,
        lineWidth: freshGeometry.lineWidth,
        drawFaces: freshGeometry.drawFaces,
        positionsBase64: undefined,
        vertexColorsBase64: undefined,
        positionsF16: undefined,
        positionBBox: undefined,
        vertexColorsU8: undefined,
        triangleIndicesBase64: undefined,
        lineIndicesBase64: undefined,
      };
    }
    const geometry = retained.get(mesh.id);
    if (!geometry || geometry.revision !== mesh.revision) return mesh;
    return {
      ...mesh,
      positions: geometry.positions,
      vertexColors: geometry.vertexColors,
      triangleIndices: geometry.triangleIndices,
      lineIndices: geometry.lineIndices,
      lineColor: mesh.lineColor ?? geometry.lineColor,
      lineWidth: mesh.lineWidth ?? geometry.lineWidth,
      drawFaces: mesh.drawFaces ?? geometry.drawFaces,
    };
  });
}
//=============================================================================
export function retainFigureDisplayListWebGlGeometry(
  displayList: FigureDisplayListResult,
  retained: Map<number, FigureWebGlGeometry>
): FigureDisplayListResult {
  if (!displayList.webglMeshes?.length) return displayList;
  return {
    ...displayList,
    webglMeshes: retainFigureWebGlGeometry(displayList.webglMeshes, retained),
  };
}
//=============================================================================
export function hasCompleteFigureWebGlGeometry(meshes: FigureWebGlMesh[]): boolean {
  return meshes.every(
    (mesh) =>
      (mesh.positions !== undefined ||
        mesh.positionsBase64 !== undefined ||
        mesh.positionsF16 !== undefined) &&
      (mesh.triangleIndices !== undefined || mesh.triangleIndicesBase64 !== undefined) &&
      (mesh.lineIndices !== undefined || mesh.lineIndicesBase64 !== undefined) &&
      (!mesh.drawFaces ||
        mesh.vertexColors !== undefined ||
        mesh.vertexColorsBase64 !== undefined ||
        mesh.vertexColorsU8 !== undefined)
  );
}
//=============================================================================
export function isMatrixOnlyWebGlFrame(
  current: FigureDisplayListResult | null,
  next: FigureDisplayListResult
): boolean {
  if (!current || !next.webglMeshes?.length) return false;
  if (next.matrixOnly === true)
    return current.width === next.width && current.height === next.height;
  if (next.webglMeshes.some((mesh) => meshHasGeometry(mesh))) {
    return false;
  }
  if (current.displaySignature && next.displaySignature) {
    return (
      current.width === next.width &&
      current.height === next.height &&
      current.displaySignature === next.displaySignature
    );
  }
  return (
    current.width === next.width &&
    current.height === next.height &&
    JSON.stringify(current.primitives) === JSON.stringify(next.primitives) &&
    JSON.stringify(current.controls ?? []) === JSON.stringify(next.controls ?? [])
  );
}
//=============================================================================
// Single-slot queue: an animation stream only ever needs the newest frame,
// older undisplayed frames are superseded rather than replayed.
export function queueLatestWebGlAnimationFrame(
  queue: FigureWebGlMesh[][],
  frame: FigureWebGlMesh[]
): FigureWebGlMesh[][] {
  void queue;
  return [frame];
}
//=============================================================================
