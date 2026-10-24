//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { alignBlockToGrid, blockPortAnchor, normalizeWorldPosition } from '../geometry.js';
import { state } from '../../app/state.js';
import type { RendererCtx, Block, RendererState } from '../core/renderer-types.js';
//=============================================================================
const DRAG_THRESHOLD = 8; // px (client space) before drag begins
const MAGNETIC_RADIUS = 12; // px (SVG space) for snap-to-block edges/centers
const _COLLISION_ROLLBACK_MS = 120; // ms for the spring-back animation
void _COLLISION_ROLLBACK_MS;
const GUIDE_SNAP_RADIUS = 10; // px (SVG space) to trigger smart guide display
const GUIDE_EXTENT = 20000; // large enough to span any visible diagram
interface ExtCtx {
  clientToSvg: (x: number, y: number) => { x: number; y: number };
  updateBlockTransform?: (b: Block, animated?: boolean) => void;
  updateConnections?: (force?: boolean) => void;
  detectBlockOverlap?: (ids: string[]) => boolean;
  updateSelectionBox?: () => void;
  signalDiagramChanged?: () => void;
}
//=============================================================================
interface ExtState extends RendererState {
  deleteMode?: boolean;
  isPinching?: boolean;
  fastRouting?: boolean;
  routingDirty?: boolean;
  dirtyBlocks?: Set<string>;
  dirtyConnections?: Set<unknown>;
  suppressNextPortClick?: boolean;
  pendingPort?: { blockId: string; portType?: 'in' | 'out'; portIndex?: number } | null;
  deferRouting?: boolean;
  deferRoutingIds?: Set<string>;
  selectedIds?: Set<string>;
  isDragging?: boolean;
  simulationActive?: boolean;
}
//=============================================================================
interface DragGroup {
  startPositions: Map<string, { x: number; y: number }>;
}
/**
 * Snap the raw SVG position of the dragged block, per axis. An edge or center of
 * another block within MAGNETIC_RADIUS wins, otherwise the grid snap applies.
 */
function magneticSnap(
  rawX: number,
  rawY: number,
  draggedIds: Set<string>,
  allBlocks: Map<string, Block>,
  blockWidth: number, // width of the block being dragged
  blockHeight: number,
  anchor: { x: number; y: number } | null = null // port offset the grid snap lands on
): { x: number; y: number } {
  const _defaultSnapped = alignBlockToGrid({ x: rawX, y: rawY }, anchor);
  let bestX = _defaultSnapped.x;
  let bestY = _defaultSnapped.y;
  let minDX = Math.abs(bestX - rawX);
  let minDY = Math.abs(bestY - rawY);

  for (const [id, b] of allBlocks) {
    if (draggedIds.has(id)) continue;

    const bRight = b.x + (b.width ?? blockWidth);
    const bBottom = b.y + (b.height ?? blockHeight);
    const bCX = b.x + (b.width ?? blockWidth) / 2;
    const bCY = b.y + (b.height ?? blockHeight) / 2;

    // X candidates: left edge, right edge, center, right-of-block, left-of-block
    const xCandidates = [b.x, bRight, bCX, bRight, b.x - blockWidth];
    // The dragged block's corresponding X value to align with each candidate
    const xSelf = [rawX, rawX + blockWidth, rawX + blockWidth / 2, rawX, rawX + blockWidth];

    for (let i = 0; i < xCandidates.length; i++) {
      const d = Math.abs(xSelf[i] - xCandidates[i]);
      if (d < MAGNETIC_RADIUS && d < minDX) {
        minDX = d;
        bestX = xCandidates[i] - (xSelf[i] - rawX);
      }
    }

    const yCandidates = [b.y, bBottom, bCY, bBottom, b.y - blockHeight];
    const ySelf = [rawY, rawY + blockHeight, rawY + blockHeight / 2, rawY, rawY + blockHeight];

    for (let i = 0; i < yCandidates.length; i++) {
      const d = Math.abs(ySelf[i] - yCandidates[i]);
      if (d < MAGNETIC_RADIUS && d < minDY) {
        minDY = d;
        bestY = yCandidates[i] - (ySelf[i] - rawY);
      }
    }
  }

  return { x: bestX, y: bestY };
}
//=============================================================================
const SVG_NS = 'http://www.w3.org/2000/svg';
const GUIDE_GROUP_ID = '_nflow-smart-guides';
function clearSmartGuides(overlayLayer: Element | null | undefined): void {
  if (!overlayLayer) return;
  overlayLayer.querySelector(`#${GUIDE_GROUP_ID}`)?.remove();
}
//=============================================================================
function drawSmartGuides(
  blockX: number,
  blockY: number,
  blockW: number,
  blockH: number,
  draggedIds: Set<string>,
  allBlocks: Map<string, Block>,
  overlayLayer: Element
): void {
  clearSmartGuides(overlayLayer);

  const dxs = [blockX, blockX + blockW / 2, blockX + blockW];
  const dys = [blockY, blockY + blockH / 2, blockY + blockH];

  const vGuides = new Set<number>(); // vertical lines (x = const)
  const hGuides = new Set<number>(); // horizontal lines (y = const)

  for (const [id, b] of allBlocks) {
    if (draggedIds.has(id)) continue;
    const bW = b.width ?? 120;
    const bH = b.height ?? 60;
    const bxs = [b.x, b.x + bW / 2, b.x + bW];
    const bys = [b.y, b.y + bH / 2, b.y + bH];

    for (const bx of bxs) {
      for (const dx of dxs) {
        if (Math.abs(dx - bx) < GUIDE_SNAP_RADIUS) vGuides.add(bx);
      }
    }
    for (const by of bys) {
      for (const dy of dys) {
        if (Math.abs(dy - by) < GUIDE_SNAP_RADIUS) hGuides.add(by);
      }
    }
  }

  if (vGuides.size === 0 && hGuides.size === 0) return;

  const group = document.createElementNS(SVG_NS, 'g');
  group.setAttribute('id', GUIDE_GROUP_ID);
  group.setAttribute('pointer-events', 'none');

  const addLine = (x1: number, y1: number, x2: number, y2: number) => {
    const line = document.createElementNS(SVG_NS, 'line');
    line.setAttribute('x1', String(x1));
    line.setAttribute('y1', String(y1));
    line.setAttribute('x2', String(x2));
    line.setAttribute('y2', String(y2));
    line.setAttribute('stroke', '#00bcd4');
    line.setAttribute('stroke-width', '1');
    line.setAttribute('stroke-dasharray', '6 3');
    line.setAttribute('opacity', '0.85');
    group.appendChild(line);
  };

  for (const x of vGuides) addLine(x, -GUIDE_EXTENT, x, GUIDE_EXTENT);
  for (const y of hGuides) addLine(-GUIDE_EXTENT, y, GUIDE_EXTENT, y);

  overlayLayer.appendChild(group);
}
//=============================================================================
export function makeDragHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as ExtCtx;
  const S = ctx.state as unknown as ExtState;

  function enableDrag(block: Block, handle: Element) {
    const dashboardBlock = String(block.type ?? '').startsWith('dashboard');

    let dragging = false;
    let didDrag = false;
    let startClient: { x: number; y: number } | null = null;
    let startedFromPort = false;
    let activePointerId: number | null = null;

    // Offset captured at the precise grab point (startClient -> SVG space)
    // so the block stays anchored under the pointer from the very first frame.
    let grabOffsetX = 0;
    let grabOffsetY = 0;

    // SVG position of the lead block at drag-start - used as the group-delta
    // reference so snap rounding on the lead block does NOT compound into followers.
    let originX = 0;
    let originY = 0;

    // Positions of all blocks before the drag started - used for collision rollback.
    let prePositions: Map<string, { x: number; y: number }> | null = null;

    let dragGroup: DragGroup | null = null;
    let rafId = 0;

    function markDirty(ids: Iterable<string>) {
      if (!S.dirtyBlocks) return;
      for (const id of ids) S.dirtyBlocks.add(id);
    }

    function draggedIds(): Set<string> {
      return dragGroup ? new Set(dragGroup.startPositions.keys()) : new Set([block.id as string]);
    }

    /** Resets all per-drag state without touching block positions. */
    function resetState() {
      window.removeEventListener('pointerup', finishPointer, true);
      window.removeEventListener('pointercancel', cancelPointer, true);
      clearSmartGuides(ctx.overlayLayer);
      dragging = false;
      didDrag = false;
      startClient = null;
      startedFromPort = false;
      activePointerId = null;
      dragGroup = null;
      prePositions = null;
      S.fastRouting = false;
      cancelAnimationFrame(rafId);
      rafId = 0;
    }

    /**
     * Animate blocks back to their pre-drag positions after a collision, through
     * the `animated` flag of updateBlockTransform (CSS transition).
     */
    function rollbackPositions() {
      if (!prePositions) return;
      prePositions.forEach((pos, id) => {
        const b = S.blocks.get(id) as Block | undefined;
        if (!b) return;
        b.x = pos.x;
        b.y = pos.y;
        C.updateBlockTransform?.(b, true /* animated */);
      });
    }

    function beginPointer(event: PointerEvent, fromPort: boolean) {
      if (state.locked) return;
      if (S.simulationActive && dashboardBlock) return;
      if (S.deleteMode || S.isPinching) return;
      if (!fromPort) event.preventDefault();

      dragging = false;
      didDrag = false;
      startedFromPort = fromPort;
      startClient = { x: event.clientX, y: event.clientY };
      activePointerId = event.pointerId;
      window.addEventListener('pointerup', finishPointer, true);
      window.addEventListener('pointercancel', cancelPointer, true);

      // Capture pointer on the drag handle when starting a drag from the handle.
      // Do NOT set pointer capture when the pointer started from a port because
      // that prevents the subsequent click event on the port from firing.
      if (!fromPort) {
        (handle as HTMLElement).setPointerCapture?.(event.pointerId);
      }
    }

    handle.addEventListener('pointerdown', (evt) => beginPointer(evt as PointerEvent, false));

    // Do not start a drag on pointerdown originating from a port-group.
    // Starting drags should only occur when the user presses the block handle.
    block.group?.addEventListener('pointerdown', (evt) => {
      const event = evt as PointerEvent;
      const fromPort = Boolean((event.target as Element)?.closest?.('.port-group'));
      if (fromPort) {
        // Let port click handlers manage interactions; don't call beginPointer here.
        return;
      }
      if (dashboardBlock && event.target !== handle) return;
      beginPointer(event, false);
    });

    handle.addEventListener('pointermove', (evt) => {
      const event = evt as PointerEvent;

      if (S.isPinching) {
        resetState();
        return;
      }
      if (!startClient) return;
      // A pending port means a connection is being made: do not drag while the
      // pointer moves between ports.
      if (S.pendingPort) return;

      if (!dragging) {
        const dx = event.clientX - startClient.x;
        const dy = event.clientY - startClient.y;
        if (Math.hypot(dx, dy) < DRAG_THRESHOLD) return;

        dragging = true;
        // Mark global state so history can avoid taking snapshots mid-drag
        S.isDragging = true;
        didDrag = true;

        // Compute grab offset from the ORIGINAL startClient so the block
        // stays anchored at the grab point - not at where the pointer is now.
        const grabSvg = C.clientToSvg(startClient.x, startClient.y);
        grabOffsetX = grabSvg.x - block.x;
        grabOffsetY = grabSvg.y - block.y;
        originX = block.x;
        originY = block.y;

        prePositions = new Map();
        S.blocks.forEach((b: Block, id: string) => {
          prePositions!.set(id, { x: b.x, y: b.y });
        });

        const selectedIds = S.selectedIds ?? new Set<string>();
        if (selectedIds.size > 1 && selectedIds.has(block.id ?? '')) {
          const startPositions = new Map<string, { x: number; y: number }>();
          selectedIds.forEach((id) => {
            const b = S.blocks.get(id) as Block | undefined;
            if (b) startPositions.set(id, { x: b.x, y: b.y });
          });
          dragGroup = { startPositions };
        } else {
          dragGroup = null;
        }
      }

      // Throttle all DOM writes to one per animation frame.
      if (rafId) return;
      rafId = requestAnimationFrame(() => {
        rafId = 0;
        if (!dragging) return;

        const point = C.clientToSvg(event.clientX, event.clientY);
        const rawX = point.x - grabOffsetX;
        const rawY = point.y - grabOffsetY;
        const ids = draggedIds();
        const snapped = magneticSnap(
          rawX,
          rawY,
          ids,
          S.blocks as Map<string, Block>,
          block.width ?? 120,
          block.height ?? 60,
          blockPortAnchor(block)
        );
        const finalPos = normalizeWorldPosition(snapped, { x: block.x, y: block.y });

        if (dragGroup) {
          // Delta in RAW (pre-snap) space from the lead block's origin so
          // every follower moves by the same logical distance without
          // accumulating snap error from the lead block.
          const rawDx = rawX - originX;
          const rawDy = rawY - originY;

          dragGroup.startPositions.forEach((pos, id) => {
            const b = S.blocks.get(id) as Block | undefined;
            if (!b) return;
            const snappedMember = magneticSnap(
              pos.x + rawDx,
              pos.y + rawDy,
              ids,
              S.blocks as Map<string, Block>,
              b.width ?? 120,
              b.height ?? 60,
              blockPortAnchor(b)
            );
            const memberPos = normalizeWorldPosition(snappedMember, pos);
            b.x = memberPos.x;
            b.y = memberPos.y;
            C.updateBlockTransform?.(b);
          });
          markDirty(ids);
        } else {
          block.x = finalPos.x;
          block.y = finalPos.y;
          C.updateBlockTransform?.(block);
          S.dirtyBlocks?.add(block.id as string);
        }

        if (ctx.overlayLayer) {
          drawSmartGuides(
            block.x,
            block.y,
            block.width ?? 120,
            block.height ?? 60,
            draggedIds(),
            S.blocks as Map<string, Block>,
            ctx.overlayLayer
          );
        }

        S.fastRouting = true;
        S.routingDirty = true;
        C.updateConnections?.();
      });
    });

    function finishPointer(evt: Event) {
      const event = evt as PointerEvent;
      if (activePointerId === null || event.pointerId !== activePointerId) return;
      if (dragging) event.preventDefault();

      const moved = didDrag;
      const snapshot = dragGroup;

      if (startedFromPort && didDrag) S.suppressNextPortClick = true;

      cancelAnimationFrame(rafId);
      rafId = 0;
      const movedIds = snapshot ? Array.from(snapshot.startPositions.keys()) : [block.id as string];
      const hasCollision = moved && (C.detectBlockOverlap?.(movedIds) ?? false);

      if (hasCollision) {
        rollbackPositions();
      }

      resetState();
      S.isDragging = false;

      if (!moved) return;

      if (hasCollision) {
        // Do not re-route while positions are restored after a collision.
        S.deferRouting = false;
        S.deferRoutingIds = new Set();
        S.routingDirty = false;
        S.dirtyBlocks?.clear?.();
        S.dirtyConnections?.clear?.();
        C.updateSelectionBox?.();
        return;
      }

      S.deferRouting = false;
      S.deferRoutingIds = new Set();
      markDirty(movedIds);
      S.routingDirty = true;
      C.updateConnections?.(true);
      try {
        void movedIds.map((id) => {
          const b = S.blocks.get(id) as Block | undefined;
          return b ? { id: b.id, x: b.x, y: b.y } : { id };
        });
      } catch (_e) {}
      C.signalDiagramChanged?.();
      try {
        window.dispatchEvent(new Event('diagramCommitted'));
      } catch (_e) {
        // ignore in non-browser contexts
      }
    }

    handle.addEventListener('pointerup', finishPointer);

    function cancelPointer(evt: Event) {
      const event = evt as PointerEvent;
      if (activePointerId === null || event.pointerId !== activePointerId) return;
      // Roll back to pre-drag snapshot on cancel (e.g. browser gesture takeover).
      rollbackPositions();
      resetState();
      S.isDragging = false;
    }

    handle.addEventListener('pointercancel', cancelPointer);
  }

  return { enableDrag };
}
//=============================================================================
