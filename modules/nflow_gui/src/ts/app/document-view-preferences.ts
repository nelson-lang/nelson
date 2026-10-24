//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getCurrentFilePath } from './state.js';
import {
  getViewBox,
  getZoomScale,
  setViewBox,
  setZoomScale,
  type ViewBox,
} from './workspace-handlers.js';
import { getUiPref, setUiPref } from './ui-prefs.js';
import { onAppEvent } from './app-events.js';
//=============================================================================
type SavedView = ViewBox & { zoom: number };
let bound = false;
let restoring = false;
function preferenceKey(path: string): string {
  const normalized = path.replace(/\\/g, '/').toLocaleLowerCase();
  let hash = 2166136261;
  for (let index = 0; index < normalized.length; index += 1) {
    hash ^= normalized.charCodeAt(index);
    hash = Math.imul(hash, 16777619);
  }
  return `nf-document-view-${(hash >>> 0).toString(16)}`;
}
//=============================================================================
function isFinitePositive(value: unknown): value is number {
  return typeof value === 'number' && Number.isFinite(value) && value > 0;
}
//=============================================================================
function readSavedView(path: string): SavedView | null {
  try {
    const value = JSON.parse(getUiPref(preferenceKey(path), '') || '') as Partial<SavedView>;
    if (
      typeof value.x !== 'number' ||
      !Number.isFinite(value.x) ||
      typeof value.y !== 'number' ||
      !Number.isFinite(value.y) ||
      !isFinitePositive(value.w) ||
      !isFinitePositive(value.h) ||
      !isFinitePositive(value.zoom) ||
      value.zoom < 0.1 ||
      value.zoom > 3
    ) {
      return null;
    }
    return value as SavedView;
  } catch {
    return null;
  }
}
//=============================================================================
function saveCurrentView(): void {
  if (restoring) return;
  const path = getCurrentFilePath();
  if (!path) return;
  const view = getViewBox();
  if (!isFinitePositive(view.w) || !isFinitePositive(view.h)) return;
  setUiPref(preferenceKey(path), JSON.stringify({ ...view, zoom: getZoomScale() }));
}
//=============================================================================
function restoreView(path: string | null): void {
  if (!path) return;
  const saved = readSavedView(path);
  if (!saved) return;
  restoring = true;
  setZoomScale(saved.zoom);
  setViewBox({ x: saved.x, y: saved.y, w: saved.w, h: saved.h });
  restoring = false;
}
//=============================================================================
let unbind: Array<() => void> = [];
export function bindDocumentViewPreferences(): void {
  if (bound || typeof window === 'undefined') return;
  bound = true;
  unbind = [
    onAppEvent('nf-viewbox-changed', saveCurrentView),
    onAppEvent('nf-current-file-change', (detail) => restoreView(detail?.path ?? null)),
  ];
}
//=============================================================================
export function _resetDocumentViewPreferencesForTests(): void {
  for (const off of unbind) off();
  unbind = [];
  bound = false;
  restoring = false;
}
//=============================================================================
