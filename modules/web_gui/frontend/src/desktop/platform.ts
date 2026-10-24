//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// True in the browser (WebAssembly) bundle, where the engine runs in a worker.
// The native desktop (WebView window or `--web` server) leaves the build flag
// undefined. Evaluated on each call so unit tests can toggle the global.
export function isWasmRuntime(): boolean {
  return typeof __NELSON_WASM__ !== 'undefined' && __NELSON_WASM__;
}
//=============================================================================
export function isMacPlatform(): boolean {
  const platform = navigator.userAgentData?.platform || navigator.platform || navigator.userAgent;
  return /mac/i.test(platform);
}
//=============================================================================
export function primaryModifierLabel(): string {
  return isMacPlatform() ? '⌘' : 'Ctrl';
}
//=============================================================================
export function formatPrimaryShortcut(key: string, shift: boolean, mac: boolean): string {
  const normalized = key.toUpperCase();
  if (mac) return `${shift ? '⇧' : ''}⌘${normalized}`;
  return `Ctrl+${shift ? 'Shift+' : ''}${normalized}`;
}
//=============================================================================
export function primaryShortcut(key: string, shift = false): string {
  return formatPrimaryShortcut(key, shift, isMacPlatform());
}
//=============================================================================
export function hasPrimaryModifier(event: KeyboardEvent | React.KeyboardEvent): boolean {
  return isMacPlatform() ? event.metaKey && !event.ctrlKey : event.ctrlKey && !event.metaKey;
}
//=============================================================================
