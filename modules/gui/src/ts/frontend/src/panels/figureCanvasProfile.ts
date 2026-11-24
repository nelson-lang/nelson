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
//=============================================================================
export type FigureCanvasProfileCounter =
  | 'effectRuns'
  | 'paintCalls'
  | 'workerFramePosts'
  | 'workerPostCalls'
  | 'workerRefreshCalls'
  | 'workerPaintCalls'
  | 'webGlRenderCalls'
  | 'rasterWebGlCalls'
  | 'rasterWebGlTextureHits'
  | 'rasterWebGlTextureMisses'
  | 'rasterWebGlRetainedBytes'
  | 'rasterWebGlMaxLiveBytes'
  | 'rasterWebGlUploadCalls'
  | 'rasterWebGlUploadBytes'
  | 'rgbaBitmapCalls'
  | 'rgbaCanvasCalls'
  | 'urlImageLoads'
  | 'primitiveFrames'
  | 'webGlMeshFrames'
  | 'webGlMeshes'
  | 'rasterCandidateFrames'
  | 'primitiveImages'
  | 'primitiveImageBytes'
  | 'primitiveRects'
  | 'primitiveLines'
  | 'primitiveSegments'
  | 'primitivePolylines'
  | 'primitivePolygons'
  | 'primitiveTexts'
  | 'primitivePoints'
  | 'primitiveRectBatches'
  | 'primitiveClips'
  | 'primitiveOthers';
export type FigureCanvasProfileTimer =
  | 'effectMs'
  | 'paintMs'
  | 'workerPostMs'
  | 'workerRefreshMs'
  | 'workerPaintMs'
  | 'webGlRenderMs'
  | 'rasterWebGlMs'
  | 'rasterWebGlUploadMs'
  | 'rgbaBitmapMs'
  | 'rgbaCanvasMs'
  | 'urlImageLoadMs';
export type FigureCanvasProfile = Record<
  FigureCanvasProfileCounter | FigureCanvasProfileTimer,
  number
>;
const defaultProfile: FigureCanvasProfile = {
  effectRuns: 0,
  effectMs: 0,
  paintCalls: 0,
  paintMs: 0,
  workerFramePosts: 0,
  workerPostCalls: 0,
  workerPostMs: 0,
  workerRefreshCalls: 0,
  workerRefreshMs: 0,
  workerPaintCalls: 0,
  workerPaintMs: 0,
  webGlRenderCalls: 0,
  webGlRenderMs: 0,
  rasterWebGlCalls: 0,
  rasterWebGlMs: 0,
  rasterWebGlTextureHits: 0,
  rasterWebGlTextureMisses: 0,
  rasterWebGlRetainedBytes: 0,
  rasterWebGlMaxLiveBytes: 0,
  rasterWebGlUploadCalls: 0,
  rasterWebGlUploadMs: 0,
  rasterWebGlUploadBytes: 0,
  rgbaBitmapCalls: 0,
  rgbaBitmapMs: 0,
  rgbaCanvasCalls: 0,
  rgbaCanvasMs: 0,
  urlImageLoads: 0,
  urlImageLoadMs: 0,
  primitiveFrames: 0,
  webGlMeshFrames: 0,
  webGlMeshes: 0,
  rasterCandidateFrames: 0,
  primitiveImages: 0,
  primitiveImageBytes: 0,
  primitiveRects: 0,
  primitiveLines: 0,
  primitiveSegments: 0,
  primitivePolylines: 0,
  primitivePolygons: 0,
  primitiveTexts: 0,
  primitivePoints: 0,
  primitiveRectBatches: 0,
  primitiveClips: 0,
  primitiveOthers: 0,
};
//=============================================================================
declare global {
  interface Window {
    __nelsonFigureCanvasProfileEnabled?: boolean;
    __nelsonFigureCanvasProfile?: FigureCanvasProfile;
    __nelsonResetFigureCanvasProfile?: () => FigureCanvasProfile;
    __nelsonGetFigureCanvasProfile?: () => FigureCanvasProfile;
  }
}
//=============================================================================
function emptyProfile(): FigureCanvasProfile {
  return { ...defaultProfile };
}
//=============================================================================
function isBrowser(): boolean {
  return typeof window !== 'undefined';
}
//=============================================================================
export function resetFigureCanvasProfile(): FigureCanvasProfile {
  const profile = emptyProfile();
  if (isBrowser()) window.__nelsonFigureCanvasProfile = profile;
  return profile;
}
//=============================================================================
export function getFigureCanvasProfile(): FigureCanvasProfile {
  if (!isBrowser()) return emptyProfile();
  if (!window.__nelsonFigureCanvasProfile) window.__nelsonFigureCanvasProfile = emptyProfile();
  return { ...window.__nelsonFigureCanvasProfile };
}
//=============================================================================
export function figureCanvasProfileEnabled(): boolean {
  if (!isBrowser()) return false;
  if (window.__nelsonFigureCanvasProfileEnabled === true) return true;
  try {
    return new URLSearchParams(window.location.search).get('figureProfile') === '1';
  } catch {
    return false;
  }
}
//=============================================================================
function mutableProfile(): FigureCanvasProfile | null {
  if (!figureCanvasProfileEnabled()) return null;
  if (!window.__nelsonFigureCanvasProfile) window.__nelsonFigureCanvasProfile = emptyProfile();
  return window.__nelsonFigureCanvasProfile;
}
//=============================================================================
export function incrementFigureCanvasCounter(counter: FigureCanvasProfileCounter): void {
  const profile = mutableProfile();
  if (!profile) return;
  profile[counter] += 1;
}
//=============================================================================
export function addFigureCanvasValue(counter: FigureCanvasProfileCounter, value: number): void {
  const profile = mutableProfile();
  if (!profile || !Number.isFinite(value)) return;
  profile[counter] += value;
}
//=============================================================================
export function recordFigureCanvasMaximum(
  counter: FigureCanvasProfileCounter,
  value: number
): void {
  const profile = mutableProfile();
  if (!profile || !Number.isFinite(value)) return;
  profile[counter] = Math.max(profile[counter], value);
}
//=============================================================================
export function recordFigureCanvasMeasurement(
  counter: FigureCanvasProfileCounter,
  timer: FigureCanvasProfileTimer,
  milliseconds: number
): void {
  const profile = mutableProfile();
  if (!profile) return;
  profile[counter] += 1;
  profile[timer] += milliseconds;
}
//=============================================================================
export function recordFigurePrimitiveProfile(
  primitives: DrawPrimitive[],
  rasterCandidate: boolean,
  webGlMeshCount = 0
): void {
  const profile = mutableProfile();
  if (!profile) return;
  profile.primitiveFrames += 1;
  if (webGlMeshCount > 0) profile.webGlMeshFrames += 1;
  profile.webGlMeshes += webGlMeshCount;
  if (rasterCandidate) profile.rasterCandidateFrames += 1;
  for (const primitive of primitives) {
    switch (primitive.kind) {
      case 'image':
        profile.primitiveImages += 1;
        if (primitive.rgbaBytes instanceof ArrayBuffer) {
          profile.primitiveImageBytes += primitive.rgbaBytes.byteLength;
        } else if (typeof primitive.rgba === 'string') {
          profile.primitiveImageBytes += Math.floor((primitive.rgba.length * 3) / 4);
        }
        break;
      case 'rect':
        profile.primitiveRects += 1;
        break;
      case 'line':
        profile.primitiveLines += 1;
        break;
      case 'segments':
        profile.primitiveSegments += 1;
        break;
      case 'polyline':
        profile.primitivePolylines += 1;
        break;
      case 'polygon':
        profile.primitivePolygons += 1;
        break;
      case 'text':
        profile.primitiveTexts += 1;
        break;
      case 'points':
        profile.primitivePoints += 1;
        break;
      case 'rects':
        profile.primitiveRectBatches += 1;
        break;
      case 'clip':
      case 'clipSave':
      case 'clipRestore':
        profile.primitiveClips += 1;
        break;
      default:
        profile.primitiveOthers += 1;
        break;
    }
  }
}
//=============================================================================
export function measureFigureCanvas<T>(
  counter: FigureCanvasProfileCounter,
  timer: FigureCanvasProfileTimer,
  callback: () => T
): T {
  if (!figureCanvasProfileEnabled()) return callback();
  const start = performance.now();
  try {
    return callback();
  } finally {
    const profile = mutableProfile();
    if (profile) {
      profile[counter] += 1;
      profile[timer] += performance.now() - start;
    }
  }
}
//=============================================================================
export async function measureFigureCanvasAsync<T>(
  counter: FigureCanvasProfileCounter,
  timer: FigureCanvasProfileTimer,
  callback: () => Promise<T>
): Promise<T> {
  if (!figureCanvasProfileEnabled()) return callback();
  const start = performance.now();
  try {
    return await callback();
  } finally {
    const profile = mutableProfile();
    if (profile) {
      profile[counter] += 1;
      profile[timer] += performance.now() - start;
    }
  }
}
//=============================================================================
export function installFigureCanvasProfileHooks(): void {
  if (!isBrowser()) return;
  window.__nelsonResetFigureCanvasProfile = resetFigureCanvasProfile;
  window.__nelsonGetFigureCanvasProfile = getFigureCanvasProfile;
}
//=============================================================================
installFigureCanvasProfileHooks();
//=============================================================================
