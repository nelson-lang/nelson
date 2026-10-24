//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Quick-connect: select a source block, then Ctrl+click a
// target block to create an auto-routed connection to its first free input.
//=============================================================================
import { state, signalDiagramChanged } from './state.js';
import { writeStatus } from './status-service.js';
import { wasRecentDuplicate } from './dup-drag.js';
import { t } from '../utils/i18n.js';
//=============================================================================
interface RendererLike {
  createConnection?: (from: unknown, to: unknown, toIndex?: number, fromIndex?: number) => unknown;
  updateConnections?: (fast?: boolean) => void;
  getLastConnectionError?: () => { message?: string } | null;
}
//=============================================================================
let rendererRef: { current: RendererLike | null } | null = null;
export function setRendererRef(ref: { current: RendererLike | null } | null): void {
  rendererRef = ref;
}
//=============================================================================
type BlockLike = { id?: unknown; type?: string; inputs?: number; outputs?: number };
function resolveBlockId(raw: string): unknown | null {
  if (state.blocks.has(raw as never)) return raw;
  const asNumber = Number(raw);
  if (Number.isFinite(asNumber) && state.blocks.has(asNumber as never)) return asNumber;
  return null;
}
//=============================================================================
/** First input index of `targetId` without an incoming connection, or -1. */
export function firstFreeInput(targetId: unknown, inputCount: number): number {
  const occupied = new Set(
    (state.connections as Array<{ to?: unknown; toIndex?: unknown }>)
      .filter((c) => String(c.to) === String(targetId))
      .map((c) => Number(c.toIndex ?? 0))
  );
  for (let i = 0; i < inputCount; i++) {
    if (!occupied.has(i)) return i;
  }
  return -1;
}
//=============================================================================
/** Try to connect the selected block to `targetId`. Returns true on success. */
export function quickConnectTo(targetRaw: string): boolean {
  const sourceId = (state as { selectedId?: unknown }).selectedId;
  const targetId = resolveBlockId(targetRaw);
  if (sourceId === null || sourceId === undefined || targetId === null) return false;
  if (String(sourceId) === String(targetId)) return false;
  const source = state.blocks.get(sourceId as never) as BlockLike | undefined;
  const target = state.blocks.get(targetId as never) as BlockLike | undefined;
  if (!source || !target) return false;

  if (!Number(source.outputs)) {
    writeStatus(null, t('Quick connect: source block has no output'), 'warn');
    return false;
  }
  const toIndex = firstFreeInput(targetId, Number(target.inputs) || 0);
  if (toIndex < 0) {
    writeStatus(null, t('Quick connect: no free input on target block'), 'warn');
    return false;
  }
  const renderer = rendererRef?.current;
  const created = renderer?.createConnection?.(sourceId, targetId, toIndex, 0);
  if (!created) {
    const err = renderer?.getLastConnectionError?.();
    writeStatus(null, String(err?.message || t('Quick connect failed')), 'error');
    return false;
  }
  state.routingDirty = true;
  renderer?.updateConnections?.();
  signalDiagramChanged();
  writeStatus(
    null,
    t('Connected {from} to {to}', { from: String(source.type), to: String(target.type) }),
    'success',
    2000,
    String(targetId)
  );
  return true;
}
//=============================================================================
export function initQuickConnect(svg: SVGSVGElement | null): void {
  if (!svg || (svg as unknown as { _quickConnectBound?: boolean })._quickConnectBound) return;
  (svg as unknown as { _quickConnectBound?: boolean })._quickConnectBound = true;
  // Capture phase: block-level click handlers stopPropagation() in bubble.
  svg.addEventListener(
    'click',
    (ev) => {
      if (!(ev.ctrlKey || ev.metaKey) || state.locked) return;
      // A Ctrl+drag duplication fires a click on pointerup - not a connect.
      if (wasRecentDuplicate()) return;
      const group = (ev.target as Element | null)?.closest?.(
        '#blockLayer g.svg-block'
      ) as SVGGElement | null;
      if (!group) return;
      const targetRaw = group.getAttribute('data-block-id');
      if (!targetRaw) return;
      quickConnectTo(targetRaw);
    },
    true
  );
}
//=============================================================================
export default initQuickConnect;
//=============================================================================
