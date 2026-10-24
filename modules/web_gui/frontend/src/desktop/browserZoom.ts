//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
function hasZoomModifier(event: KeyboardEvent | WheelEvent): boolean {
  return event.ctrlKey || event.metaKey;
}
//=============================================================================
function isZoomKey(event: KeyboardEvent): boolean {
  const key = event.key.toLowerCase();
  return (
    key === '+' ||
    key === '-' ||
    key === '=' ||
    key === '0' ||
    key === '_' ||
    key === 'add' ||
    key === 'subtract'
  );
}
//=============================================================================
function stop(event: Event): void {
  event.preventDefault();
  event.stopImmediatePropagation();
}
//=============================================================================
// Browser zoom is locked, but the zoom gestures are re-emitted as this app
// event so panels can implement font zoom (accessibility): detail.delta is
// +1 / -1, or 0 for reset (Ctrl+0). detail.origin carries the original event
// target so wheel listeners can scope themselves to their own panel.
export const FONT_ZOOM_EVENT = 'nelson:font-zoom';
export interface FontZoomDetail {
  delta: number;
  origin: EventTarget | null;
}
//=============================================================================
function emitFontZoom(target: Window, delta: number, origin: EventTarget | null): void {
  target.dispatchEvent(
    new CustomEvent<FontZoomDetail>(FONT_ZOOM_EVENT, { detail: { delta, origin } })
  );
}
//=============================================================================
const installedLocks = new WeakSet<Window>();
export function installBrowserZoomLock(target: Window = window): () => void {
  // Idempotent: the entry page can be loaded twice (webui show + navigate);
  // stacking capture handlers would emit duplicated font zoom events.
  if (installedLocks.has(target)) {
    return () => undefined;
  }
  installedLocks.add(target);
  const onKeyDown = (event: KeyboardEvent) => {
    if (hasZoomModifier(event) && isZoomKey(event)) {
      stop(event);
      const key = event.key.toLowerCase();
      const delta = key === '0' ? 0 : key === '-' || key === '_' || key === 'subtract' ? -1 : 1;
      emitFontZoom(target, delta, event.target);
    }
  };
  const onWheel = (event: WheelEvent) => {
    if (hasZoomModifier(event)) {
      stop(event);
      emitFontZoom(target, event.deltaY < 0 ? 1 : -1, event.target);
    }
  };
  const onTouchMove = (event: TouchEvent) => {
    if (event.touches.length > 1) stop(event);
  };
  const onGesture = (event: Event) => stop(event);

  target.addEventListener('keydown', onKeyDown, true);
  target.addEventListener('wheel', onWheel, { capture: true, passive: false });
  target.addEventListener('touchmove', onTouchMove, { capture: true, passive: false });
  target.addEventListener('gesturestart', onGesture, { capture: true, passive: false });
  target.addEventListener('gesturechange', onGesture, { capture: true, passive: false });

  return () => {
    installedLocks.delete(target);
    target.removeEventListener('keydown', onKeyDown, true);
    target.removeEventListener('wheel', onWheel, true);
    target.removeEventListener('touchmove', onTouchMove, true);
    target.removeEventListener('gesturestart', onGesture, true);
    target.removeEventListener('gesturechange', onGesture, true);
  };
}
//=============================================================================
