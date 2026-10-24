//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { emitAppEvent } from './app-events.js';
import { t } from '../utils/i18n.js';
//=============================================================================
export type DiagnosticSeverity = 'error' | 'warning' | 'info';
export interface NFlowDiagnostic {
  id: string;
  severity: DiagnosticSeverity;
  message: string;
  source: 'editor' | 'model' | 'simulation' | 'application' | 'library';
  blockId?: string | null;
  connectionId?: string | null;
  helpTopic?: string | null;
  createdAt: number;
}
//=============================================================================
type DiagnosticInput = Omit<NFlowDiagnostic, 'id' | 'createdAt'>;
type Subscriber = (items: readonly NFlowDiagnostic[]) => void;
const MAX_DIAGNOSTICS = 200;
const SEVERITY_ORDER: Record<DiagnosticSeverity, number> = {
  error: 0,
  warning: 1,
  info: 2,
};
//=============================================================================
let diagnostics: NFlowDiagnostic[] = [];
let idCounter = 0;
const subscribers = new Set<Subscriber>();
function normalizeInput(input: DiagnosticInput): DiagnosticInput {
  return {
    ...input,
    message: input.message.trim(),
    blockId: input.blockId || undefined,
    connectionId: input.connectionId || undefined,
    helpTopic: input.helpTopic || undefined,
  };
}
//=============================================================================
function sortDiagnostics(items: NFlowDiagnostic[]): NFlowDiagnostic[] {
  return items.sort(
    (left, right) =>
      SEVERITY_ORDER[left.severity] - SEVERITY_ORDER[right.severity] ||
      left.createdAt - right.createdAt
  );
}
//=============================================================================
function updateCounters(): void {
  const errors = diagnostics.filter((item) => item.severity === 'error').length;
  const warnings = diagnostics.filter((item) => item.severity === 'warning').length;
  const errorEl = document.getElementById('diagnosticErrorCount');
  const warningEl = document.getElementById('diagnosticWarningCount');
  // A single count-aware tooltip on both counters, so the bare "3 / 1" reads
  // as "3 errors, 1 warning"; falls back to the action hint when empty.
  const summary =
    errors + warnings > 0
      ? `${t('Errors')}: ${errors}, ${t('Warnings')}: ${warnings}`
      : t('Open diagnostics');
  if (errorEl) {
    errorEl.textContent = String(errors);
    const counter = errorEl.closest('.diagnostic-counter');
    counter?.classList.toggle('active', errors > 0);
    counter?.setAttribute('title', summary);
  }
  if (warningEl) {
    warningEl.textContent = String(warnings);
    const counter = warningEl.closest('.diagnostic-counter');
    counter?.classList.toggle('active', warnings > 0);
    counter?.setAttribute('title', summary);
  }
  // The bottom-panel tab badge (#diagnosticTabCount) is owned by the React
  // <DiagnosticTabBadge/>, which subscribes to this service. Writing it here too raced
  // that render and left it stale.
}
//=============================================================================
function notify(): void {
  const snapshot = diagnostics.slice();
  subscribers.forEach((subscriber) => {
    try {
      subscriber(snapshot);
    } catch (_error) {
      /* subscriber failures must not interrupt diagnostics */
    }
  });
  updateCounters();
  emitAppEvent('nf-diagnostics-change', { items: snapshot });
}
//=============================================================================
export function addDiagnostic(input: DiagnosticInput): string {
  input = normalizeInput(input);
  const duplicate = diagnostics.find(
    (item) =>
      item.severity === input.severity &&
      item.message === input.message &&
      item.blockId === input.blockId &&
      item.connectionId === input.connectionId &&
      item.helpTopic === input.helpTopic
  );
  if (duplicate) return duplicate.id;

  const diagnostic: NFlowDiagnostic = {
    ...input,
    id: `d${++idCounter}`,
    createdAt: Date.now(),
  };
  diagnostics = sortDiagnostics([...diagnostics, diagnostic].slice(-MAX_DIAGNOSTICS));
  notify();
  return diagnostic.id;
}
//=============================================================================
export function replaceDiagnostics(items: DiagnosticInput[]): void {
  diagnostics = sortDiagnostics(
    items.slice(-MAX_DIAGNOSTICS).map((item) => ({
      ...normalizeInput(item),
      id: `d${++idCounter}`,
      createdAt: Date.now(),
    }))
  );
  notify();
}
//=============================================================================
export function clearDiagnostics(): void {
  diagnostics = [];
  notify();
}
//=============================================================================
// Called when the editor switches document. Diagnostics of the previous model would
// outlive it, and their blockId / connectionId would point at nothing or at a
// homonymous block of the new file. Library discovery warnings are session-wide and
// reported once at startup, so they are kept.
export function clearDocumentDiagnostics(): void {
  const kept = diagnostics.filter((item) => item.source === 'library');
  if (kept.length === diagnostics.length) return;
  diagnostics = kept;
  notify();
}
//=============================================================================
export function getDiagnostics(): readonly NFlowDiagnostic[] {
  return diagnostics.slice();
}
//=============================================================================
export function subscribeDiagnostics(subscriber: Subscriber): () => void {
  subscribers.add(subscriber);
  subscriber(diagnostics.slice());
  return () => subscribers.delete(subscriber);
}
//=============================================================================
export function refreshDiagnosticCounters(): void {
  updateCounters();
}
//=============================================================================
export function _resetDiagnosticsForTests(): void {
  diagnostics = [];
  idCounter = 0;
  subscribers.clear();
  updateCounters();
}
//=============================================================================
