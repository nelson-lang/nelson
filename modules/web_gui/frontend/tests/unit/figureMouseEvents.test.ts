//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import {
  createFigureMouseSender,
  figureMouseEventPayload,
  MOTION_PROBE_INTERVAL_MS,
  resolveSelectionType,
  type FigureMouseEventPayload,
  type FigureMouseEventResult,
} from '../../src/panels/figureMouseEvents';
//=============================================================================
const box = { left: 100, top: 50, width: 400, height: 300 };
function deferred() {
  let resolve: (value: FigureMouseEventResult) => void = () => undefined;
  const promise = new Promise<FigureMouseEventResult>((r) => (resolve = r));
  return { promise, resolve };
}
//=============================================================================
const flush = () => new Promise((r) => setTimeout(r, 0));
describe('figure mouse events', () => {
  it('maps the client position to figure pixels with a bottom-left origin', () => {
    expect(
      figureMouseEventPayload(7, 'press', { clientX: 300, clientY: 110 }, box, 400, 300)
    ).toMatchObject({ handle: 7, action: 'press', x: 200, y: 240 });
  });

  it('undoes the CSS scale of the stage during a resize preview', () => {
    const scaled = { left: 0, top: 0, width: 200, height: 150 };
    expect(
      figureMouseEventPayload(7, 'motion', { clientX: 50, clientY: 150 }, scaled, 400, 300)
    ).toMatchObject({ handle: 7, action: 'motion', x: 100, y: 0 });
  });

  it('carries the gesture (button, modifiers, double click) for the SelectionType', () => {
    const payload = figureMouseEventPayload(
      7,
      'press',
      { clientX: 300, clientY: 110, button: 2, shiftKey: true, ctrlKey: false, detail: 2 },
      box,
      400,
      300
    );
    expect(payload).toMatchObject({ button: 2, shiftKey: true, ctrlKey: false, doubleClick: true });
  });

  it('resolves the SelectionType like the engine', () => {
    expect(resolveSelectionType(0, false, false, false)).toBe('normal');
    expect(resolveSelectionType(0, true, false, false)).toBe('extend');
    expect(resolveSelectionType(1, false, false, false)).toBe('extend');
    expect(resolveSelectionType(0, false, true, false)).toBe('alt');
    expect(resolveSelectionType(2, false, false, false)).toBe('alt');
    expect(resolveSelectionType(0, false, false, true)).toBe('open');
    // a double click wins over every modifier
    expect(resolveSelectionType(2, true, true, true)).toBe('open');
  });

  it('keeps one motion in flight and sends the latest position next', async () => {
    const sent: FigureMouseEventPayload[] = [];
    const pending = [deferred(), deferred()];
    const send = createFigureMouseSender((payload) => {
      sent.push(payload);
      return pending[sent.length - 1]?.promise ?? Promise.resolve({ ok: true, motion: true });
    });
    const motion = (x: number): FigureMouseEventPayload => ({
      handle: 1,
      action: 'motion',
      x,
      y: 0,
    });
    send(motion(1));
    send(motion(2));
    send(motion(3));
    expect(sent.map((p) => p.x)).toEqual([1]);
    pending[0].resolve({ ok: true, motion: true });
    await flush();
    expect(sent.map((p) => p.x)).toEqual([1, 3]);
  });

  it('stops sending motion when the figure does not listen, but always sends clicks', async () => {
    let clock = 0;
    const sent: FigureMouseEventPayload[] = [];
    const send = createFigureMouseSender(
      (payload) => {
        sent.push(payload);
        return Promise.resolve({ ok: true, motion: false });
      },
      () => clock
    );
    send({ handle: 1, action: 'press', x: 0, y: 0 });
    await flush();
    send({ handle: 1, action: 'motion', x: 1, y: 0 });
    send({ handle: 1, action: 'release', x: 2, y: 0 });
    expect(sent.map((p) => p.action)).toEqual(['press', 'motion', 'release']);
    await flush();
    send({ handle: 1, action: 'motion', x: 3, y: 0 });
    expect(sent).toHaveLength(3);
    clock += MOTION_PROBE_INTERVAL_MS;
    send({ handle: 1, action: 'motion', x: 4, y: 0 });
    expect(sent.map((p) => p.x)).toEqual([0, 1, 2, 4]);
  });
});
//=============================================================================
