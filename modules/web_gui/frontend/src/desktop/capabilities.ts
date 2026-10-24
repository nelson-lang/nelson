//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { rpc } from '../rpc/client';
import type { AppCapabilitiesResult } from '../rpc/types';
//=============================================================================
const unavailable: AppCapabilitiesResult = { nflow: false, packageManager: false };
let capabilities = unavailable;
const listeners = new Set<() => void>();
export function getDesktopCapabilities(): AppCapabilitiesResult {
  return capabilities;
}
//=============================================================================
export function subscribeDesktopCapabilities(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}
//=============================================================================
export function setDesktopCapabilities(next: AppCapabilitiesResult): void {
  // The launch mode is fixed for the session, so never let a poll that comes
  // back without it (or as 'none' during a reconnect) clobber a concrete value
  // already learned from client.role or an earlier poll.
  const launchMode =
    next.launchMode && next.launchMode !== 'none' ? next.launchMode : capabilities.launchMode;
  capabilities = {
    nflow: !!next.nflow,
    packageManager: !!next.packageManager,
    packageUpdates: updatesCount(next.packageUpdates),
    launchMode,
  };
  listeners.forEach((listener) => listener());
}
//=============================================================================
// client.role can report the launch mode before app.capabilities does. Missing
// the native webview mode would dock new figures that should open in windows.
export function setLaunchMode(launchMode: AppCapabilitiesResult['launchMode']): void {
  if (!launchMode || capabilities.launchMode === launchMode) return;
  capabilities = { ...capabilities, launchMode };
  listeners.forEach((listener) => listener());
}
//=============================================================================
export function setPackageUpdates(count: unknown): void {
  capabilities = { ...capabilities, packageUpdates: updatesCount(count) };
  listeners.forEach((listener) => listener());
}
//=============================================================================
export function packageManagerTitle(count: number | undefined): string {
  return count ? `Package Manager (${count})` : 'Package Manager';
}
//=============================================================================
export function packageUpdatesBadge(count: number | undefined): string {
  if (!count) return '';
  return count > 9 ? '9+' : String(count);
}
//=============================================================================
function updatesCount(value: unknown): number {
  const count = Math.floor(Number(value));
  return Number.isFinite(count) && count > 0 ? count : 0;
}
//=============================================================================
export async function refreshDesktopCapabilities(): Promise<boolean> {
  try {
    setDesktopCapabilities(await rpc<AppCapabilitiesResult>('app.capabilities'));
    return true;
  } catch {
    setDesktopCapabilities(unavailable);
    return false;
  }
}
//=============================================================================
