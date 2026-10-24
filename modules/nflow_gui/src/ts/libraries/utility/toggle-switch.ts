//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Toggle Switch renderer
//=============================================================================
import { Block, Helpers, UiBlock } from '../types.js';
import * as FieldNames from '../../FieldNames.js';
import { nflowRpc } from '../../app/native-rpc.js';
//=============================================================================
export function toggleSwitchRenderer(block: Block, helpers: Helpers): void {
  const { createSvgElement } = helpers;
  const group = block.group as Element & { appendChild: (child: Node) => void };

  const bodyRect = createSvgElement!('rect', {
    x: 0,
    y: 0,
    width: block.width,
    height: block.height,
    class: 'nf-block toggle-block',
  });
  group.appendChild(bodyRect);

  // Resize handlers reflow the DOM, so these are initial values only.
  const trackW = 56;
  const trackH = 22;
  const trackX = Math.max(4, Math.floor((block.width - trackW) / 2));
  const trackY = Math.max(6, Math.floor((block.height - trackH) / 2));

  const track = createSvgElement!('rect', {
    x: String(trackX),
    y: String(trackY),
    rx: String(Math.floor(trackH / 2)),
    ry: String(Math.floor(trackH / 2)),
    width: String(trackW),
    height: String(trackH),
    class: 'toggle-track',
  });
  group.appendChild(track);

  const knobR = Math.floor(trackH / 2) - 2;
  const leftCx = trackX + knobR + 2;
  const rightCx = trackX + trackW - knobR - 2;
  const cy = trackY + Math.floor(trackH / 2);

  type ToggleParams = {
    state?: number | boolean | string;
    onValue?: number | string;
    offValue?: number | string;
    onLabel?: string;
    offLabel?: string;
  };

  const bp = (block.params || {}) as ToggleParams;
  const state = Number(bp.state ?? 0) ? 1 : 0;

  const knob = createSvgElement!('circle', {
    cx: String(leftCx),
    cy: String(cy),
    r: String(knobR),
    class: 'toggle-knob',
  });
  group.appendChild(knob);
  const knobEl = knob as unknown as SVGElement & { style: CSSStyleDeclaration };
  const knobDelta = rightCx - leftCx;
  try {
    knobEl.style.transition = 'transform 160ms cubic-bezier(.2,.9,.3,1)';
    knobEl.style.transformBox = 'fill-box';
    knobEl.style.transformOrigin = 'center';
    knobEl.style.transform = state ? `translate(${knobDelta}px, 0)` : 'translate(0, 0)';
  } catch (_e) {}

  const labelText = state ? String(bp.onLabel ?? 'ON') : String(bp.offLabel ?? 'OFF');
  const label = createSvgElement!(
    'text',
    {
      x: String(Math.floor(block.width / 2)),
      y: String(trackY + trackH + 14),
      'text-anchor': 'middle',
      class: 'toggle-label',
    },
    labelText
  );
  group.appendChild(label);

  const uiBlock = block as UiBlock & {
    toggleKnob?: Element;
    toggleLabel?: Element;
    toggleTrack?: Element;
  };
  uiBlock.toggleKnob = knob;
  uiBlock.toggleLabel = label;
  uiBlock.toggleTrack = track;

  try {
    track.setAttribute('style', 'cursor: pointer; pointer-events: all');
    knob.setAttribute('style', 'cursor: pointer; pointer-events: all');
    (label as Element).setAttribute('style', 'cursor: pointer;');
  } catch (_e) {}

  // Reads geometry from the DOM so resizes done by external layout handlers count.
  function updateVisuals(newState: number) {
    const trackEl = (block.group as Element).querySelector(
      '.toggle-track'
    ) as SVGRectElement | null;
    if (trackEl) {
      const tx = Number(trackEl.getAttribute('x') ?? 0);
      const tw = Number(trackEl.getAttribute('width') ?? 0);
      const th = Number(trackEl.getAttribute('height') ?? 0);
      const kr = Math.floor(th / 2) - 2;
      const left = tx + kr + 2;
      const right = tx + tw - kr - 2;
      try {
        knobEl.style.transform = `translate(${(newState ? right : left) - left}px, 0)`;
      } catch (_e) {
        knob.setAttribute('cx', String(newState ? right : left));
      }
    } else {
      try {
        knobEl.style.transform = `translate(${newState ? knobDelta : 0}px, 0)`;
      } catch (_e) {
        knob.setAttribute('cx', String(newState ? rightCx : leftCx));
      }
    }
    label.textContent = newState ? String(bp.onLabel ?? 'ON') : String(bp.offLabel ?? 'OFF');
  }

  // data-state lets CSS style the ON/OFF look.
  function setDataState(newState: number) {
    try {
      (group as Element).setAttribute('data-state', String(newState));
    } catch (_e) {}
  }

  function sendRuntimeEvent(newState: number) {
    try {
      const payload = JSON.stringify({
        type: 'setParam',
        id: block.id,
        key: FieldNames.kState,
        value: newState,
      });
      void nflowRpc('nflow.sim.event', { name: 'SimControl', data: payload });
    } catch (_err) {
      // ignore
    }
  }

  function toggleState() {
    bp.state = bp.state ? 0 : 1;
    const newState = bp.state ? 1 : 0;
    updateVisuals(newState);
    sendRuntimeEvent(newState);
    setDataState(newState);
  }

  // Capture-phase pointerdown detects short taps even when the drag handle
  // overlays the block. Propagation is not stopped, so dragging still works.
  let downX = 0;
  let downY = 0;
  let downId: number | null = null;
  const onPointerDown = (ev: unknown) => {
    const pev = ev as PointerEvent;
    if (pev.button !== 0) return;
    const rect = (group as Element).getBoundingClientRect();
    if (
      pev.clientX < rect.left ||
      pev.clientX > rect.right ||
      pev.clientY < rect.top ||
      pev.clientY > rect.bottom
    )
      return;
    downX = pev.clientX;
    downY = pev.clientY;
    downId = pev.pointerId;

    const onUp = (upEvAny: unknown) => {
      const upEv = upEvAny as PointerEvent;
      if (upEv.pointerId !== downId) return;
      window.removeEventListener('pointerup', onUp as EventListener, true);
      window.removeEventListener('pointercancel', onCancel as EventListener, true);
      const dx = Math.abs(upEv.clientX - downX);
      const dy = Math.abs(upEv.clientY - downY);
      if (dx <= 6 && dy <= 6) {
        toggleState();
      }
      downId = null;
    };
    const onCancel = (_: unknown) => {
      window.removeEventListener('pointerup', onUp as EventListener, true);
      window.removeEventListener('pointercancel', onCancel as EventListener, true);
      downId = null;
    };
    window.addEventListener('pointerup', onUp as EventListener, true);
    window.addEventListener('pointercancel', onCancel as EventListener, true);
  };
  group.addEventListener('pointerdown', onPointerDown as EventListener, true);

  setTimeout(() => updateVisuals(state), 0);
  setDataState(state);
}
//=============================================================================
export default toggleSwitchRenderer;
//=============================================================================
