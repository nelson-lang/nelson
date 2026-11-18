//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { RendererCtx, Conn } from '../core/renderer-types.js';
//=============================================================================
export function setupSvgListeners(ctx: RendererCtx) {
  const C = ctx as unknown as {
    svg: SVGSVGElement | null;
    wireCornerDrag?: {
      active?: boolean;
      pointerId?: number | null;
      conn?: Conn | null;
      index?: number;
    };
    marqueeState?: { active?: boolean; pointerId?: number | null; start?: unknown };
    clientToSvg: (x: number, y: number) => { x: number; y: number };
    moveWireCorner?: (conn: Conn, index: number, p: { x: number; y: number }) => boolean;
    applyWirePaths?: (m: Map<unknown, unknown>) => void;
    updateSelectionBox?: () => void;
    updateWireCornerHandles?: () => void;
    finishMarqueeSelection?: (e: PointerEvent) => void;
    marqueeRect?: Element | null;
    updateMarqueeRect?: (a: unknown, b: { x: number; y: number }) => void;
    armWireSegmentDrag?: (e: PointerEvent) => boolean;
    updateWireSegmentDrag?: (e: PointerEvent) => boolean;
    commitWireSegmentDrag?: (e: PointerEvent) => boolean;
    cancelWireSegmentDrag?: (e?: PointerEvent | null) => boolean;
    handleWireDoubleClick?: (e: MouseEvent) => boolean;
    handleWireSegmentHover?: (e: PointerEvent) => void;
    armWireEndpointDrag?: (e: PointerEvent) => boolean;
    cancelWireEndpointDrag?: () => boolean;
    handleWireEndpointHover?: (e: PointerEvent) => boolean;
    armWireBranchDrag?: (e: PointerEvent) => boolean;
    cancelWireBranchDrag?: () => boolean;
    refreshAlignHints?: () => void;
    clearAlignHints?: () => void;
  };

  C.svg?.addEventListener('pointerdown', (event: PointerEvent) => {
    // Any press that is not on the align-connect hint itself dismisses it, so a
    // stale hint never lingers once the user starts another interaction.
    if (!(event.target as Element | null)?.closest?.('#_nflow-align-hint')) C.clearAlignHints?.();
    // Ctrl or Cmd drag on a wire branches from its source. A press near a wire
    // end re-targets that endpoint. Neither stops a plain click from selecting.
    if (C.armWireBranchDrag?.(event)) return;
    if (C.armWireEndpointDrag?.(event)) return;
    C.armWireSegmentDrag?.(event);
  });

  C.svg?.addEventListener('dblclick', (event: MouseEvent) => {
    // Double-click on a pinned wire hands it back to the auto-router.
    C.handleWireDoubleClick?.(event);
  });

  if (typeof window !== 'undefined') {
    window.addEventListener('keydown', (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        C.cancelWireBranchDrag?.();
        C.cancelWireEndpointDrag?.();
        C.cancelWireSegmentDrag?.(null);
      }
    });
    // After a block move, insert or route commits, show an align to connect
    // hint for the best newly lined up output and input pair.
    window.addEventListener('diagramCommitted', () => C.refreshAlignHints?.());
  }

  C.svg?.addEventListener('pointermove', (event: PointerEvent) => {
    if (C.updateWireSegmentDrag?.(event)) return;
    if (C.wireCornerDrag?.active && event.pointerId === C.wireCornerDrag.pointerId) {
      const point = C.clientToSvg(event.clientX, event.clientY);
      if (
        C.moveWireCorner &&
        C.wireCornerDrag.conn &&
        C.wireCornerDrag.index !== undefined &&
        C.moveWireCorner(C.wireCornerDrag.conn, C.wireCornerDrag.index, point)
      ) {
        C.applyWirePaths?.(new Map());
        C.updateSelectionBox?.();
      }
      return;
    }
    if (C.marqueeState?.active) {
      if (event.pointerId !== C.marqueeState.pointerId) return;
      const point = C.clientToSvg(event.clientX, event.clientY);
      C.updateMarqueeRect?.(C.marqueeState.start, point);
      return;
    }
    // Idle pointer move: wire-end re-target hint takes priority over segment hover.
    if (C.handleWireEndpointHover?.(event)) return;
    C.handleWireSegmentHover?.(event);
  });

  C.svg?.addEventListener('pointerup', (event: PointerEvent) => {
    if (C.commitWireSegmentDrag?.(event)) return;
    if (C.wireCornerDrag?.active && event.pointerId === C.wireCornerDrag.pointerId) {
      C.wireCornerDrag.active = false;
      C.wireCornerDrag.conn = null;
      C.wireCornerDrag.index = -1;
      C.wireCornerDrag.pointerId = null;
      C.updateWireCornerHandles?.();
      if (typeof window !== 'undefined') window.dispatchEvent(new CustomEvent('diagramChanged'));
      return;
    }
    if (!C.marqueeState?.active) return;
    if (event.pointerId !== C.marqueeState.pointerId) return;
    C.finishMarqueeSelection?.(event);
  });

  C.svg?.addEventListener('pointercancel', (event: PointerEvent) => {
    if (C.cancelWireSegmentDrag?.(event)) return;
    if (C.wireCornerDrag?.active && event.pointerId === C.wireCornerDrag.pointerId) {
      C.wireCornerDrag.active = false;
      C.wireCornerDrag.conn = null;
      C.wireCornerDrag.index = -1;
      C.wireCornerDrag.pointerId = null;
      C.updateWireCornerHandles?.();
      return;
    }
    if (!C.marqueeState?.active) return;
    if (event.pointerId !== C.marqueeState.pointerId) return;
    C.marqueeRect?.setAttribute('display', 'none');
    if (C.marqueeState) {
      C.marqueeState.active = false;
      C.marqueeState.pointerId = null;
      C.marqueeState.start = null;
    }
  });
}
//=============================================================================
