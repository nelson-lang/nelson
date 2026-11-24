//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useRef } from 'react';
import type { MouseEvent as ReactMouseEvent, RefObject } from 'react';
import { rpc } from '../rpc/client';
import {
  createFigureMouseSender,
  figureMouseEventPayload,
  type FigureMouseAction,
  type FigureMouseEventResult,
} from './figureMouseEvents';
//=============================================================================
type StageSize = { width: number; height: number } | null;
const isDrawing = (event: ReactMouseEvent) =>
  (event.target as HTMLElement | null)?.tagName === 'CANVAS';
// Pointer over the figure: the engine updates CurrentPoint and runs the
// WindowButton*Fcn callbacks. Motion is reported over the whole figure, as in
// the native window; clicks on uicontrols stay with the controls.
// Returns the handlers to spread on the stage element.
export function useFigureMouse(
  handle: number,
  stageRef: RefObject<HTMLElement | null>,
  size: StageSize
) {
  const sender = useRef<ReturnType<typeof createFigureMouseSender> | null>(null);
  if (!sender.current) {
    sender.current = createFigureMouseSender((payload) =>
      rpc<FigureMouseEventResult>('figure.mouseEvent', payload)
    );
  }
  const send = (action: FigureMouseAction, event: MouseEvent | ReactMouseEvent) => {
    const stage = stageRef.current;
    if (!stage || !size) return;
    const rect = stage.getBoundingClientRect();
    sender.current?.(figureMouseEventPayload(handle, action, event, rect, size.width, size.height));
  };
  const onMouseDown = (event: ReactMouseEvent) => {
    if (!isDrawing(event)) return;
    send('press', event);
    // Like a native window, the figure keeps the pointer until the release.
    const move = (ev: MouseEvent) => send('motion', ev);
    const up = (ev: MouseEvent) => {
      window.removeEventListener('mousemove', move);
      window.removeEventListener('mouseup', up);
      send('release', ev);
    };
    window.addEventListener('mousemove', move);
    window.addEventListener('mouseup', up);
  };
  const onMouseMove = (event: ReactMouseEvent) => {
    if (event.buttons === 0) send('motion', event);
  };
  return { onMouseDown, onMouseMove };
}
//=============================================================================
