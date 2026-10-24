//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { addDiagnostic } from './diagnostics-service.js';
//=============================================================================
/**
 * Status-bar service. Modules write messages with `pushStatus()` instead of
 * setting `statusEl.textContent`. The service holds the current item and
 * propagates it to the registered DOM element and to all `subscribeStatus`
 * listeners. Legacy modules holding a `statusEl` keep working because `app.ts`
 * calls `setStatusElement(statusBarInfo)`, which writes through to it.
 */
export type StatusLevel = 'info' | 'warn' | 'error' | 'progress' | 'success';
export interface StatusItem {
  id: string;
  level: StatusLevel;
  message: string;
  /** 0-100, present only for `level === 'progress'` */
  progress?: number | null;
  /** Auto-clear after this many ms. null/undefined = persist until replaced. */
  durationMs?: number | null;
  /** Diagram block related to this message (makes console entries clickable). */
  blockId?: string | null;
  diagnosticSource?: 'editor' | 'model' | 'simulation' | 'application' | 'library';
  createdAt: number;
}
//=============================================================================
type Subscriber = (current: StatusItem | null) => void;
let _current: StatusItem | null = null;
let _element: HTMLElement | null = null;
let _progressEl: HTMLElement | null = null;
const _subscribers: Set<Subscriber> = new Set();
let _clearTimer: ReturnType<typeof setTimeout> | null = null;
let _idCounter = 0;
function _notify(): void {
  _subscribers.forEach((fn) => {
    try {
      fn(_current);
    } catch (_e) {
      /* subscriber errors must not crash the service */
    }
  });

  if (_element) {
    _element.textContent = _current ? _current.message : '';
  }

  if (_progressEl) {
    const pct =
      _current?.level === 'progress' && _current.progress != null ? _current.progress : null;
    if (pct != null) {
      _progressEl.setAttribute('aria-valuenow', String(Math.round(pct)));
      (_progressEl as HTMLElement).style.setProperty('--progress', `${Math.round(pct)}%`);
      _progressEl.removeAttribute('hidden');
    } else {
      _progressEl.setAttribute('hidden', '');
      _progressEl.setAttribute('aria-valuenow', '0');
      (_progressEl as HTMLElement).style.setProperty('--progress', '0%');
    }
  }
}
//=============================================================================
function _scheduleAutoClear(id: string, durationMs: number): void {
  if (_clearTimer !== null) clearTimeout(_clearTimer);
  _clearTimer = setTimeout(() => {
    _clearTimer = null;
    if (_current?.id === id) {
      _current = null;
      _notify();
    }
  }, durationMs);
}
/**
 * Push a new status message. Returns the generated id so the caller can
 * clear it explicitly if needed.
 */
export function pushStatus(item: Omit<StatusItem, 'id' | 'createdAt'>): string {
  if (_clearTimer !== null) {
    clearTimeout(_clearTimer);
    _clearTimer = null;
  }
  const id = `s${++_idCounter}`;
  _current = { ...item, id, createdAt: Date.now() };
  if (item.level === 'error' || item.level === 'warn') {
    addDiagnostic({
      severity: item.level === 'error' ? 'error' : 'warning',
      message: item.message,
      source: item.diagnosticSource ?? 'application',
      blockId: item.blockId ?? null,
    });
  }
  _notify();
  if (item.durationMs != null && item.durationMs > 0) {
    _scheduleAutoClear(id, item.durationMs);
  }
  return id;
}
/**
 * Clear the current status. Pass an `id` to only clear if the active item
 * matches (avoids races where a newer message replaced the one being cleared).
 */
export function clearStatus(id?: string): void {
  if (id !== undefined && _current?.id !== id) return;
  if (_clearTimer !== null) {
    clearTimeout(_clearTimer);
    _clearTimer = null;
  }
  _current = null;
  _notify();
}
/**
 * Subscribe to status changes. The callback is invoked immediately with the
 * current value, then on every subsequent change. Returns an unsubscribe fn.
 */
export function subscribeStatus(fn: Subscriber): () => void {
  _subscribers.add(fn);
  try {
    fn(_current);
  } catch (_e) {
    /* ignore */
  }
  return () => _subscribers.delete(fn);
}
/**
 * Register the primary DOM element that should receive status text.
 * Call this once from `app.ts` after `mountStatusBar()`.
 */
export function setStatusElement(el: HTMLElement | null): void {
  _element = el;
  if (_element) _element.textContent = _current ? _current.message : '';
}
/**
 * Register the progress DOM element (role="progressbar").
 * Call this once from `app.ts` after `mountStatusBar()`.
 */
export function setProgressElement(el: HTMLElement | null): void {
  _progressEl = el;
  _notify();
}
//=============================================================================
export function getCurrentStatus(): StatusItem | null {
  return _current;
}
/**
 * Best-effort level classification for legacy messages migrated from direct
 * `statusEl.textContent` writes. Only used for console coloring - a wrong
 * guess is cosmetic.
 */
export function inferStatusLevel(message: string): StatusLevel {
  const m = String(message);
  if (/error|erreur|failed|échec|échoué|cannot|impossible|invalid/i.test(m)) return 'error';
  if (/warning|attention/i.test(m)) return 'warn';
  if (
    /saved|succeeded|success|done|generated|loaded|enregistré|réussi|terminé|généré|chargé/i.test(m)
  ) {
    return 'success';
  }
  return 'info';
}
/**
 * Dual-write helper for legacy call sites: writes to the element the module
 * holds (unit tests inspect it) AND notifies the service so subscribers such
 * as the console panel receive the message.
 */
export function writeStatus(
  el: HTMLElement | null | undefined,
  message: string,
  level?: StatusLevel,
  durationMs?: number | null,
  blockId?: string | null
): void {
  try {
    if (el) el.textContent = message;
  } catch {
    /* ignore */
  }
  pushStatus({
    level: level ?? inferStatusLevel(message),
    message,
    durationMs: durationMs ?? null,
    blockId: blockId ?? null,
  });
}
//=============================================================================
// Test-only reset, not part of the public runtime API.
export function _resetForTests(): void {
  if (_clearTimer !== null) clearTimeout(_clearTimer);
  _clearTimer = null;
  _current = null;
  _element = null;
  _progressEl = null;
  _subscribers.clear();
  _idCounter = 0;
}
//=============================================================================
