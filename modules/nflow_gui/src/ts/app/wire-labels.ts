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
import { onAppEvent } from './app-events.js';
import { state, signalDiagramChanged } from './state.js';
import { t } from '../utils/i18n.js';
import { closeWireLabelEditor, renderWireLabelEditor } from './components/wire-label-editor.js';
import { renderWireLabelsView, type WireLabelViewModel } from './components/wire-labels-view.js';
//=============================================================================
type Conn = {
  from?: unknown;
  to?: unknown;
  fromIndex?: number;
  label?: string;
  labelOffset?: { x: number; y: number };
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
function startLabelDrag(
  event: React.PointerEvent<SVGTextElement>,
  conn: Conn,
  overlay: Element
): void {
  if (state.locked || event.button !== 0) return;
  event.preventDefault();
  event.stopPropagation();
  const matrix = (overlay as SVGGraphicsElement).getScreenCTM?.();
  const inverse = matrix?.inverse();
  const point = (x: number, y: number) =>
    inverse
      ? {
          x: inverse.a * x + inverse.c * y + inverse.e,
          y: inverse.b * x + inverse.d * y + inverse.f,
        }
      : { x, y };
  const start = point(event.clientX, event.clientY);
  const initial = conn.labelOffset ?? { x: 0, y: 0 };
  let moved = false;
  const move = (e: PointerEvent) => {
    const current = point(e.clientX, e.clientY);
    conn.labelOffset = { x: initial.x + current.x - start.x, y: initial.y + current.y - start.y };
    moved = true;
    refreshWireLabels();
  };
  const up = () => {
    window.removeEventListener('pointermove', move);
    window.removeEventListener('pointerup', up);
    window.removeEventListener('pointercancel', up);
    if (moved) signalDiagramChanged();
  };
  window.addEventListener('pointermove', move);
  window.addEventListener('pointerup', up);
  window.addEventListener('pointercancel', up);
}
//=============================================================================
export function setSignalName(conn: Conn, label: string): void {
  const matching = (state.connections as unknown as Conn[]).filter(
    (other) =>
      other.from === conn.from && Number(other.fromIndex ?? 0) === Number(conn.fromIndex ?? 0)
  );
  if (!matching.includes(conn)) matching.push(conn);
  for (const other of matching) {
    if (label.trim()) other.label = label;
    else delete other.label;
  }
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
      x: mid.x + (conn.labelOffset?.x ?? 0),
      y: mid.y - 6 + (conn.labelOffset?.y ?? 0),
      text: label,
      onPointerDown: (event) => startLabelDrag(event, conn, overlay),
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
      setSignalName(conn, value);
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
  // Name-the-wire double-click. Once a wire is selected it draws grips over
  // itself (the mid branch dot, the endpoint re-target handles), so the second
  // click of a double-click may land on a grip instead of the wire. Whether the
  // native `dblclick` fires then is a race: the browser only fires it when both
  // clicks share a target, i.e. only when the grips happen not to be laid down
  // between the two clicks. So we never rely on `dblclick`; we detect the gesture
  // at the connection level instead. Two clicks that both resolve to the same
  // wire (directly, or through one of its grips) within the double-click window
  // open the label editor, regardless of whether the two hit the same element.
  // Capture phase: the connection click handlers stopPropagation() in bubble.
  let lastClick: { conn: Conn; time: number } | null = null;
  svg.addEventListener(
    'click',
    (ev) => {
      if (state.locked) {
        lastClick = null;
        return;
      }
      const target = ev.target as Element | null;
      const conn = target ? connForClickTarget(target) : null;
      if (!conn) {
        lastClick = null;
        return;
      }
      const now = (ev as MouseEvent).timeStamp;
      const prev = lastClick;
      if (prev && prev.conn === conn && now - prev.time <= DOUBLE_CLICK_MS) {
        lastClick = null;
        ev.stopPropagation();
        openLabelEditor(conn, ev.clientX, ev.clientY);
        return;
      }
      lastClick = { conn, time: now };
    },
    true
  );
  onAppEvent('nf-wire-paths-rendered', () => refreshWireLabels());
  window.addEventListener('diagramChanged', () => refreshWireLabels());
  window.addEventListener('resize', () => refreshWireLabels());
}
//=============================================================================
export default initWireLabels;
//=============================================================================
