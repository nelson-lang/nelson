//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type FigureMouseAction = 'press' | 'motion' | 'release';
export type FigureMouseEventPayload = {
  handle: number;
  action: FigureMouseAction;
  x: number;
  y: number;
  // Gesture driving the figure SelectionType on a press. button is 0 (left),
  // 1 (middle) or 2 (right), as in the DOM MouseEvent.
  button?: number;
  shiftKey?: boolean;
  ctrlKey?: boolean;
  doubleClick?: boolean;
};
//=============================================================================
export type FigureMouseEventResult = { ok: boolean; motion?: boolean };
export type FigureSelectionType = 'normal' | 'extend' | 'alt' | 'open';
// Maps a mouse gesture to the figure SelectionType, mirroring the engine
// (resolveSelectionType in GOFigureMouseEvents): a plain left click is
// 'normal', shift-left or the middle button is 'extend', ctrl-left or the right
// button is 'alt', and a double click is 'open'.
export function resolveSelectionType(
  button: number,
  shiftKey: boolean,
  ctrlKey: boolean,
  doubleClick: boolean
): FigureSelectionType {
  if (doubleClick) return 'open';
  if (button === 2) return 'alt';
  if (button === 1) return 'extend';
  if (ctrlKey) return 'alt';
  if (shiftKey) return 'extend';
  return 'normal';
}
//=============================================================================
type Box = Pick<DOMRect, 'left' | 'top' | 'width' | 'height'>;
// Pointer position in figure pixels with a bottom-left origin, as the engine
// expects (figure CurrentPoint). The stage may be CSS-scaled during a resize
// preview, so the client position is mapped through its rendered box.
export function figureMouseEventPayload(
  handle: number,
  action: FigureMouseAction,
  event: Pick<MouseEvent, 'clientX' | 'clientY'> &
    Partial<Pick<MouseEvent, 'button' | 'shiftKey' | 'ctrlKey' | 'detail'>>,
  box: Box,
  width: number,
  height: number
): FigureMouseEventPayload {
  const x = box.width > 0 ? ((event.clientX - box.left) * width) / box.width : 0;
  const top = box.height > 0 ? ((event.clientY - box.top) * height) / box.height : 0;
  return {
    handle,
    action,
    x,
    y: height - top,
    button: event.button ?? 0,
    shiftKey: event.shiftKey ?? false,
    ctrlKey: event.ctrlKey ?? false,
    doubleClick: (event.detail ?? 0) >= 2,
  };
}
//=============================================================================
// Presses and releases are always sent. Motion is sent only while the figure
// listens to it (the engine answers `motion` on every event), with at most one
// request in flight: the latest position wins. While it does not listen, one
// motion per probe interval checks whether a callback was set since.
export const MOTION_PROBE_INTERVAL_MS = 1000;
export function createFigureMouseSender(
  send: (payload: FigureMouseEventPayload) => Promise<FigureMouseEventResult>,
  now: () => number = () => Date.now()
): (payload: FigureMouseEventPayload) => void {
  let listensToMotion = true;
  let lastProbe = -Infinity;
  let inFlight = false;
  let queued: FigureMouseEventPayload | null = null;
  const learn = (result: FigureMouseEventResult) => {
    if (typeof result?.motion === 'boolean') listensToMotion = result.motion;
  };
  const sendMotion = (payload: FigureMouseEventPayload) => {
    inFlight = true;
    send(payload)
      .then(learn, () => undefined)
      .finally(() => {
        inFlight = false;
        const next = queued;
        queued = null;
        if (next && listensToMotion) sendMotion(next);
      });
  };
  return (payload) => {
    if (payload.action !== 'motion') {
      send(payload).then(learn, () => undefined);
      return;
    }
    if (!listensToMotion) {
      if (inFlight || now() - lastProbe < MOTION_PROBE_INTERVAL_MS) return;
      lastProbe = now();
      sendMotion(payload);
      return;
    }
    if (inFlight) {
      queued = payload;
      return;
    }
    sendMotion(payload);
  };
}
//=============================================================================
