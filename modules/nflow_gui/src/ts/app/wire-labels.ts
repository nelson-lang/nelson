//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Signal (wire) labels: double-click a wire to name it; the label is drawn
// near the wire midpoint and persisted in the .nflow file.
//=============================================================================
import { state, signalDiagramChanged } from './state.js';
import { t } from '../utils/i18n.js';
import { closeWireLabelEditor, renderWireLabelEditor } from './components/wire-label-editor.js';
import { renderWireLabelsView, type WireLabelViewModel } from './components/wire-labels-view.js';
//=============================================================================
type Conn = {
  from?: unknown;
  to?: unknown;
  label?: string;
  points?: Array<{ x: number; y: number }>;
  path?: SVGPathElement | null;
  hitPath?: Element | null;
};
//=============================================================================
// Window within which two clicks on the same wire count as a double-click when
// the browser cannot fire a native `dblclick` (the clicks hit different grips).
const DOUBLE_CLICK_MS = 500;
//=============================================================================
function midpointOf(conn: Conn): { x: number; y: number } | null {
  const path = conn.path;
  try {
    if (path && typeof path.getTotalLength === 'function') {
      const len = path.getTotalLength();
      if (Number.isFinite(len) && len > 0) {
        const p = path.getPointAtLength(len / 2);
        return { x: p.x, y: p.y };
      }
    }
  } catch {
    /* jsdom or detached path - fall through */
  }
  const pts = Array.isArray(conn.points) ? conn.points : [];
  if (pts.length >= 2) {
    const mid = pts[Math.floor(pts.length / 2)];
    const prev = pts[Math.floor(pts.length / 2) - 1] ?? mid;
    return { x: (mid.x + prev.x) / 2, y: (mid.y + prev.y) / 2 };
  }
  return null;
}
//=============================================================================
export function refreshWireLabels(): void {
  if (typeof document === 'undefined') return;
  const overlay = document.getElementById('overlayLayer');
  if (!overlay) return;
  overlay.querySelectorAll('.wire-label').forEach((el) => {
    if (!el.closest('[data-nflow-wire-labels-root="true"]')) el.remove();
  });
  const labels: WireLabelViewModel[] = [];
  for (const [index, connUnknown] of (state.connections as unknown as Conn[]).entries()) {
    const conn = connUnknown;
    const label = typeof conn.label === 'string' ? conn.label.trim() : '';
    if (!label) continue;
    const mid = midpointOf(conn);
    if (!mid) continue;
    labels.push({
      key: `${index}:${label}:${mid.x}:${mid.y}`,
      x: mid.x,
      y: mid.y - 6,
      text: label,
    });
  }
  renderWireLabelsView(overlay, labels);
}
//=============================================================================
function findConnectionByPath(target: Element): Conn | null {
  for (const conn of state.connections as unknown as Conn[]) {
    if (conn.path === target || conn.hitPath === target) return conn;
  }
  return null;
}
//=============================================================================
// Resolves the connection a click belongs to. A direct hit on the wire (or its
// fat hit path) matches by element; a hit on one of the overlay grips a selected
// wire draws over itself (the mid branch dot, the endpoint re-target handles)
// belongs to the currently selected connection.
function connForClickTarget(target: Element): Conn | null {
  const direct = findConnectionByPath(target);
  if (direct) return direct;
  if (target.closest('.wire-branch-handle') || target.closest('.wire-endpoint-handle')) {
    return (state as unknown as { selectedConnection?: Conn | null }).selectedConnection ?? null;
  }
  return null;
}
//=============================================================================
function openLabelEditor(conn: Conn, clientX: number, clientY: number): void {
  closeWireLabelEditor();
  let committed = false;
  const commit = (save: boolean, rawValue: string): void => {
    if (committed) return;
    committed = true;
    if (save) {
      const value = rawValue.trim();
      if (value) conn.label = value;
      else delete conn.label;
      refreshWireLabels();
      signalDiagramChanged();
    }
    closeWireLabelEditor();
  };

  const input = renderWireLabelEditor({
    x: clientX,
    y: clientY,
    value: conn.label ?? '',
    placeholder: t('Signal name...'),
    onCommit: commit,
  });
  input?.focus();
  input?.select();
}
//=============================================================================
export function initWireLabels(svg: SVGSVGElement | null): void {
  if (!svg || (svg as unknown as { _wireLabelsBound?: boolean })._wireLabelsBound) return;
  (svg as unknown as { _wireLabelsBound?: boolean })._wireLabelsBound = true;
  // Capture phase: connection click handlers stopPropagation() in bubble.
  svg.addEventListener(
    'dblclick',
    (ev) => {
      if (state.locked) return;
      const target = ev.target as Element | null;
      if (!target || !target.closest('#wireLayer')) return;
      const conn = findConnectionByPath(target);
      if (!conn) return;
      ev.stopPropagation();
      openLabelEditor(conn, ev.clientX, ev.clientY);
    },
    true
  );
  // Once a wire is selected it draws grips over itself (mid branch dot, endpoint
  // re-target handles), so the second click of a double-click lands on a grip
  // rather than the wire and the browser fires no `dblclick` (its two clicks have
  // different targets). Recover the name-the-wire gesture by spotting the double
  // click at the connection level: two clicks on the same wire or its grips,
  // within the double-click window, open the label editor. A pair that shares a
  // target is left to the native `dblclick` above, so the editor opens once.
  let lastClick: { conn: Conn; target: Element; time: number } | null = null;
  svg.addEventListener(
    'click',
    (ev) => {
      if (state.locked) return;
      const target = ev.target as Element | null;
      const conn = target ? connForClickTarget(target) : null;
      if (!target || !conn) {
        lastClick = null;
        return;
      }
      const now = (ev as MouseEvent).timeStamp;
      const prev = lastClick;
      lastClick = { conn, target, time: now };
      if (
        prev &&
        prev.conn === conn &&
        prev.target !== target &&
        now - prev.time <= DOUBLE_CLICK_MS
      ) {
        lastClick = null;
        openLabelEditor(conn, ev.clientX, ev.clientY);
      }
    },
    true
  );
  window.addEventListener('diagramChanged', () => refreshWireLabels());
  window.addEventListener('resize', () => refreshWireLabels());
}
//=============================================================================
export default initWireLabels;
//=============================================================================
