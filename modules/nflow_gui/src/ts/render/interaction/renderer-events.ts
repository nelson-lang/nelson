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
  };

  C.svg?.addEventListener('pointerdown', (event: PointerEvent) => {
    // Arm a wire-segment drag when pressing on a wire (no propagation stop:
    // a plain click still selects the wire).
    C.armWireSegmentDrag?.(event);
  });

  C.svg?.addEventListener('dblclick', (event: MouseEvent) => {
    // Double-click on a pinned wire hands it back to the auto-router.
    C.handleWireDoubleClick?.(event);
  });

  if (typeof window !== 'undefined') {
    window.addEventListener('keydown', (event: KeyboardEvent) => {
      if (event.key === 'Escape') C.cancelWireSegmentDrag?.(null);
    });
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
    // Idle pointer move: hover feedback for draggable wire segments.
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
