//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export interface WasmFigureProfile {
  receivedFrames: number;
  flushedFrames: number;
  coalescedFrames: number;
  retainedImagePayloads: number;
  flushDelayMs: number;
  maxPendingFrames: number;
}
//=============================================================================
const defaultProfile: WasmFigureProfile = {
  receivedFrames: 0,
  flushedFrames: 0,
  coalescedFrames: 0,
  retainedImagePayloads: 0,
  flushDelayMs: 0,
  maxPendingFrames: 0,
};
//=============================================================================
declare global {
  interface Window {
    __nelsonWasmFigureProfileEnabled?: boolean;
    __nelsonWasmFigureProfile?: WasmFigureProfile;
    __nelsonResetWasmFigureProfile?: () => WasmFigureProfile;
    __nelsonGetWasmFigureProfile?: () => WasmFigureProfile;
  }
}
//=============================================================================
function isBrowser(): boolean {
  return typeof window !== 'undefined';
}
//=============================================================================
function emptyProfile(): WasmFigureProfile {
  return { ...defaultProfile };
}
//=============================================================================
function mutableProfile(): WasmFigureProfile | null {
  if (!isBrowser() || window.__nelsonWasmFigureProfileEnabled !== true) return null;
  if (!window.__nelsonWasmFigureProfile) window.__nelsonWasmFigureProfile = emptyProfile();
  return window.__nelsonWasmFigureProfile;
}
//=============================================================================
export function resetWasmFigureProfile(): WasmFigureProfile {
  const profile = emptyProfile();
  if (isBrowser()) window.__nelsonWasmFigureProfile = profile;
  return profile;
}
//=============================================================================
export function getWasmFigureProfile(): WasmFigureProfile {
  if (!isBrowser()) return emptyProfile();
  if (!window.__nelsonWasmFigureProfile) window.__nelsonWasmFigureProfile = emptyProfile();
  return { ...window.__nelsonWasmFigureProfile };
}
//=============================================================================
export function recordWasmFigureReceived(wasPending: boolean, pendingFrames: number): void {
  const profile = mutableProfile();
  if (!profile) return;
  profile.receivedFrames += 1;
  if (wasPending) profile.coalescedFrames += 1;
  profile.maxPendingFrames = Math.max(profile.maxPendingFrames, pendingFrames);
}
//=============================================================================
export function recordWasmFigureFlush(delayMs: number): void {
  const profile = mutableProfile();
  if (!profile) return;
  profile.flushedFrames += 1;
  profile.flushDelayMs += delayMs;
}
//=============================================================================
export function recordWasmFigureImagePayload(): void {
  const profile = mutableProfile();
  if (!profile) return;
  profile.retainedImagePayloads += 1;
}
//=============================================================================
export function installWasmFigureProfileHooks(): void {
  if (!isBrowser()) return;
  window.__nelsonResetWasmFigureProfile = resetWasmFigureProfile;
  window.__nelsonGetWasmFigureProfile = getWasmFigureProfile;
}
//=============================================================================
installWasmFigureProfileHooks();
//=============================================================================
