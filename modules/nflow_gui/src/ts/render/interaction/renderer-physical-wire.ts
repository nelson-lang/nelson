//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Interactive acausal (physical) wiring: press a physical pin and drag to a pin
// on another component of the same physical domain to create an undirected
// physical connection. Pins are `.physical-pin` circles registered on the
// block's `ports` as type 'phys'; createConnection makes the undirected wire.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { createSvgElement } from '../svg-elements.js';
import type { RendererCtx } from '../core/renderer-types.js';
//=============================================================================
type Pin = { blockId: string; index: number; domain: string };
type Active = {
  src: Pin;
  pinEl: Element;
  pointerId: number;
  startX: number;
  startY: number;
  armed: boolean;
};
//=============================================================================
const ARM_THRESHOLD_PX = 4;
//=============================================================================
export function makePhysicalWireHandlers(ctx: RendererCtx) {
  const C = ctx as any;
  const S = ctx.state as any;
  let active: Active | null = null;
  let preview: SVGPathElement | null = null;
  let srcPoint: { x: number; y: number } | null = null;

  const editingAllowed = (): boolean => !S.locked;

  // Resolve a `.physical-pin` element to its block, pin index and domain.
  function resolvePin(pinEl: Element): Pin | null {
    const blockId = pinEl.closest?.('[data-block-id]')?.getAttribute('data-block-id') ?? null;
    if (!blockId) return null;
    const block = S.blocks.get(blockId) as { ports?: any[] } | undefined;
    const ports = block?.ports;
    if (!Array.isArray(ports)) return null;
    const name = pinEl.getAttribute('data-pin');
    const port = ports.find((p) => p.type === 'phys' && (p.el === pinEl || p.name === name));
    if (!port) return null;
    return {
      blockId,
      index: Number(port.index ?? 0),
      domain: String(port.domain ?? pinEl.getAttribute('data-domain') ?? ''),
    };
  }

  function pinElFrom(target: Element | null): Element | null {
    if (!target) return null;
    if (target.classList?.contains('physical-pin')) return target;
    return (target.closest?.('.physical-pin') as Element | null) ?? null;
  }

  function startPreview(pinEl: Element): void {
    const rect = pinEl.getBoundingClientRect();
    srcPoint = C.clientToSvg(rect.left + rect.width / 2, rect.top + rect.height / 2);
    preview = createSvgElement('path', {
      class: 'connection-preview physical-wire-preview',
    }) as SVGPathElement;
    C.overlayLayer?.appendChild(preview);
  }

  function updatePreview(event: PointerEvent): void {
    if (!preview || !srcPoint) return;
    const p = C.clientToSvg(event.clientX, event.clientY);
    preview.setAttribute('d', `M ${srcPoint.x} ${srcPoint.y} L ${p.x} ${p.y}`);
  }

  function stopPreview(): void {
    preview?.remove();
    preview = null;
    srcPoint = null;
  }

  function teardown(): void {
    window.removeEventListener('pointermove', onMove, true);
    window.removeEventListener('pointerup', onUp, true);
    window.removeEventListener('pointercancel', onCancel, true);
  }

  function onMove(event: PointerEvent): void {
    if (!active || event.pointerId !== active.pointerId) return;
    if (!active.armed) {
      if (
        Math.hypot(event.clientX - active.startX, event.clientY - active.startY) < ARM_THRESHOLD_PX
      )
        return;
      active.armed = true;
      startPreview(active.pinEl);
    }
    updatePreview(event);
  }

  function onUp(event: PointerEvent): void {
    if (!active || event.pointerId !== active.pointerId) return;
    const current = active;
    teardown();
    active = null;
    if (!current.armed) {
      stopPreview();
      return;
    }
    const el =
      typeof document !== 'undefined'
        ? document.elementFromPoint(event.clientX, event.clientY)
        : null;
    const targetPin = pinElFrom(el as Element | null);
    if (targetPin) {
      const tgt = resolvePin(targetPin);
      if (tgt && tgt.blockId === current.src.blockId) {
        C.onConnectionError?.('Connect two different components.');
      } else if (tgt && tgt.domain && current.src.domain && tgt.domain !== current.src.domain) {
        C.onConnectionError?.('Connect pins of the same physical domain.');
      } else if (tgt) {
        // Undirected physical connection: createConnection makes it a
        // physical-wire (phys on both ends, no arrowhead, fan-out allowed).
        C.createConnection?.(current.src.blockId, tgt.blockId, tgt.index, current.src.index);
      }
    }
    stopPreview();
  }

  function onCancel(event: PointerEvent): void {
    if (!active || event.pointerId !== active.pointerId) return;
    teardown();
    active = null;
    stopPreview();
  }

  // Arm a physical-wire drag on a press over a physical pin. Returns true when it
  // consumed the press (so causal port / segment handlers are skipped).
  function armPhysicalWireDrag(event: PointerEvent): boolean {
    if (!editingAllowed() || active || event.button !== 0) return false;
    const pinEl = pinElFrom(event.target as Element | null);
    if (!pinEl) return false;
    const src = resolvePin(pinEl);
    if (!src) return false;
    active = {
      src,
      pinEl,
      pointerId: event.pointerId,
      startX: event.clientX,
      startY: event.clientY,
      armed: false,
    };
    window.addEventListener('pointermove', onMove, true);
    window.addEventListener('pointerup', onUp, true);
    window.addEventListener('pointercancel', onCancel, true);
    return true;
  }

  function cancelPhysicalWireDrag(): boolean {
    if (!active) return false;
    teardown();
    active = null;
    stopPreview();
    return true;
  }

  return { armPhysicalWireDrag, cancelPhysicalWireDrag, resolvePhysicalPin: resolvePin };
}
//=============================================================================
