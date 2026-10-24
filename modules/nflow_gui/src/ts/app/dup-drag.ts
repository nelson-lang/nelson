//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Ctrl+drag duplication: while Ctrl is held, dragging a
// block leaves a copy (same type/params/rotation) at the original position;
// the dragged block moves away with the pointer.
// A plain Ctrl+click (no movement) is left to quick-connect.
//=============================================================================
import { state, signalDiagramChanged } from './state.js';
import { writeStatus } from './status-service.js';
import { t } from '../utils/i18n.js';
//=============================================================================
const DRAG_THRESHOLD_PX = 5;
interface RendererLike {
  createBlock?: (type: string, x: number, y: number, options?: unknown) => unknown;
  updateConnections?: (fast?: boolean) => void;
}
//=============================================================================
let rendererRef: { current: RendererLike | null } | null = null;
let lastDuplicateAt = 0;
export function setRendererRef(ref: { current: RendererLike | null } | null): void {
  rendererRef = ref;
}
/** True right after a Ctrl+drag duplication (lets quick-connect ignore the
 * synthetic click fired at pointerup). */
export function wasRecentDuplicate(now = Date.now()): boolean {
  return now - lastDuplicateAt < 500;
}
//=============================================================================
function deepClone<T>(value: T): T {
  try {
    return JSON.parse(JSON.stringify(value));
  } catch {
    return value;
  }
}
//=============================================================================
type BlockLike = {
  id?: unknown;
  type?: string;
  x?: number;
  y?: number;
  rotation?: number;
  params?: unknown;
};
//=============================================================================
export function duplicateBlockAt(original: BlockLike): unknown {
  const renderer = rendererRef?.current;
  const clone = renderer?.createBlock?.(
    String(original.type),
    Number(original.x) || 0,
    Number(original.y) || 0,
    {
      params: deepClone(original.params || {}),
      rotation: original.rotation ?? 0,
    }
  ) as BlockLike | null;
  if (clone) {
    lastDuplicateAt = Date.now();
    state.routingDirty = true;
    renderer?.updateConnections?.(true);
    signalDiagramChanged();
    writeStatus(
      null,
      t('Duplicated {type}', { type: String(original.type) }),
      'success',
      2000,
      clone.id != null ? String(clone.id) : null
    );
  }
  return clone;
}
//=============================================================================
/** Attach the Ctrl+drag duplication trigger. Idempotent. */
export function initCtrlDragDuplicate(svg: SVGSVGElement | null): void {
  if (!svg || (svg as unknown as { _dupDragBound?: boolean })._dupDragBound) return;
  (svg as unknown as { _dupDragBound?: boolean })._dupDragBound = true;

  svg.addEventListener(
    'pointerdown',
    (ev) => {
      if (!ev.ctrlKey || state.locked || ev.button !== 0) return;
      const target = ev.target as Element | null;
      if (target?.closest('.port-group')) return;
      const group = target?.closest('#blockLayer g.svg-block') as SVGGElement | null;
      if (!group) return;
      const raw = group.getAttribute('data-block-id');
      if (!raw) return;
      let original = state.blocks.get(raw as never) as BlockLike | undefined;
      if (!original) {
        const asNumber = Number(raw);
        if (Number.isFinite(asNumber)) {
          original = state.blocks.get(asNumber as never) as BlockLike | undefined;
        }
      }
      if (!original) return;

      // Snapshot the source position now: the renderer will move the block.
      const snapshot: BlockLike = {
        type: original.type,
        x: Number(original.x) || 0,
        y: Number(original.y) || 0,
        rotation: (original as { rotation?: number }).rotation ?? 0,
        params: deepClone(original.params || {}),
      };
      const startX = ev.clientX;
      const startY = ev.clientY;
      let duplicated = false;

      const onMove = (moveEv: PointerEvent): void => {
        if (duplicated) return;
        const dist = Math.hypot(moveEv.clientX - startX, moveEv.clientY - startY);
        if (dist < DRAG_THRESHOLD_PX) return;
        duplicated = true;
        cleanup();
        duplicateBlockAt(snapshot);
      };
      const cleanup = (): void => {
        window.removeEventListener('pointermove', onMove);
        window.removeEventListener('pointerup', onUp);
      };
      const onUp = (): void => cleanup();
      window.addEventListener('pointermove', onMove);
      window.addEventListener('pointerup', onUp);
    },
    true
  );
}
//=============================================================================
export default initCtrlDragDuplicate;
//=============================================================================
