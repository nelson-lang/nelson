//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Typed application event bus. Every `nf-*` event is a CustomEvent on
// `document`: one target, so a listener can never miss an event dispatched on
// another one. head-init.js (a classic script, no imports) and the e2e WebUI
// mock listen or dispatch on `document` directly and stay compatible.
import type { DashboardUpdate } from '../sim/parse-results.js';
import type { SimPhase } from '../sim/sim-controller.js';
//=============================================================================
type BlockId = string | number;
export interface SimTimeDetail {
  time?: number;
  progress?: number;
  done?: boolean;
}
//=============================================================================
export interface AppEventMap {
  'nf-ready': undefined;
  'nf-libraries-start': undefined;
  'nf-libraries-ready': undefined;
  'nf-sim-start': undefined;
  'nf-sim-stop': undefined;
  'nf-sim-state': { running: boolean; phase?: SimPhase };
  'nf-sim-time': SimTimeDetail;
  'nf-error-blocks': { ids: string[] | null };
  'nf-select-block': { blockId: BlockId; blockIds?: BlockId[] };
  'nf-block-selected': { blockId: BlockId | null };
  'nf-inspector-show': { tab: string };
  'nf-open-diagnostics': undefined;
  'nf-diagnostics-change': { items: unknown[] };
  'nf-diagram-name-change': { name: string };
  'nf-current-file-change': { path: string | null };
  'nf-model-metadata-change': undefined;
  'nf-viewbox-changed': undefined;
  'nf-dashboard-update': DashboardUpdate;
  'nf-dashboard-control-change': { id: string; binding: unknown; value: number };
  'nf-ui-prefs-error': { error: unknown };
  'nf-ui-prefs-reset': undefined;
}
//=============================================================================
export type AppEventType = keyof AppEventMap;
type DetailArgs<K extends AppEventType> = AppEventMap[K] extends undefined
  ? []
  : [detail: AppEventMap[K]];
export function emitAppEvent<K extends AppEventType>(type: K, ...args: DetailArgs<K>): void {
  if (typeof document === 'undefined') return; // headless result processing
  document.dispatchEvent(new CustomEvent(type, { detail: args[0] }));
}
//=============================================================================
export function onAppEvent<K extends AppEventType>(
  type: K,
  handler: (detail: AppEventMap[K]) => void,
  options?: { once?: boolean }
): () => void {
  if (typeof document === 'undefined') return () => undefined;
  // CustomEvent.detail defaults to null: events without detail pass undefined.
  const listener = (event: Event) =>
    handler(((event as CustomEvent).detail ?? undefined) as AppEventMap[K]);
  document.addEventListener(type, listener, options);
  return () => document.removeEventListener(type, listener);
}
//=============================================================================
