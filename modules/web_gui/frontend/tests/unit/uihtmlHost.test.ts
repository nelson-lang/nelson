//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, describe, expect, it, vi } from 'vitest';
import {
  installUiHtmlMessageBridge,
  releaseUiHtmlHosts,
  retainUiHtmlHosts,
  syncUiHtmlHosts,
  type UiHtmlBridge,
} from '../../src/panels/uihtmlHost';
import type { UIControl } from '../../src/rpc/types';
//=============================================================================
const FIGURE = 7;
// jsdom has no layout, so the stage and its canvas box are given the geometry a
// real figure would have: a 400x300 drawing surface shown at half scale.
function rect(left: number, top: number, width: number, height: number): DOMRect {
  return {
    left,
    top,
    width,
    height,
    right: left + width,
    bottom: top + height,
    x: left,
    y: top,
    toJSON: () => ({}),
  } as DOMRect;
}
//=============================================================================
function mountFigure(): { box: HTMLDivElement; stage: HTMLDivElement } {
  const box = document.createElement('div');
  const stage = document.createElement('div');
  box.appendChild(stage);
  document.body.appendChild(box);
  box.getBoundingClientRect = () => rect(30, 50, 200, 150);
  stage.getBoundingClientRect = () => rect(30, 50, 200, 150);
  Object.defineProperty(stage, 'offsetWidth', { value: 400, configurable: true });
  Object.defineProperty(stage, 'offsetHeight', { value: 300, configurable: true });
  return { box, stage };
}
//=============================================================================
function htmlControl(handle: number, source: string): UIControl {
  return {
    handle,
    style: 'html',
    text: '',
    x: 10,
    y: 20,
    w: 100,
    h: 80,
    value: 0,
    min: 0,
    max: 1,
    data: { source, revision: 0, eventSeq: 0 },
  } as unknown as UIControl;
}
//=============================================================================
function testBridge(): UiHtmlBridge {
  return {
    pull: () => Promise.resolve({ dataJson: 'null', revision: 0, events: [] }),
    push: () => Promise.resolve(),
    fileUrl: (handle) => `/uihtml/${handle}`,
  };
}
//=============================================================================
function flush(): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, 0));
}
//=============================================================================
function hostFrame(handle: number): HTMLIFrameElement | null {
  return document.querySelector<HTMLIFrameElement>(`iframe[data-uihtml-handle="${handle}"]`);
}
//=============================================================================
function clipOf(figure: number): HTMLElement | null {
  return document.querySelector<HTMLElement>(`[data-uihtml-figure="${figure}"]`);
}
//=============================================================================
afterEach(() => {
  vi.useFakeTimers();
  releaseUiHtmlHosts(FIGURE);
  vi.advanceTimersByTime(1);
  vi.useRealTimers();
  document.body.innerHTML = '';
});
//=============================================================================
describe('uihtml host layer', () => {
  it('hosts the page outside the figure panel', () => {
    const { stage } = mountFigure();
    syncUiHtmlHosts(FIGURE, stage, [htmlControl(11, '<html><body>a</body></html>')], testBridge());

    const frame = hostFrame(11);
    expect(frame).not.toBeNull();
    expect(stage.contains(frame)).toBe(false);
    expect(frame?.closest('#nelson-uihtml-hosts')).not.toBeNull();
  });

  it('places the hosts over the figure stage at its scale', () => {
    const { stage } = mountFigure();
    syncUiHtmlHosts(FIGURE, stage, [htmlControl(11, '<html><body>a</body></html>')], testBridge());

    const clip = clipOf(FIGURE);
    expect(clip?.style.visibility).toBe('visible');
    expect(clip?.style.left).toBe('30px');
    expect(clip?.style.top).toBe('50px');
    expect(clip?.style.width).toBe('200px');
    const layer = clip?.firstElementChild as HTMLElement;
    expect(layer.style.width).toBe('400px');
    expect(layer.style.transform).toBe('scale(0.5, 0.5)');
  });

  // The contract: the dock takes the content element of an unselected tab out
  // of the document. The page must not go with it.
  it('keeps the page loaded while the figure tab is hidden', () => {
    const { box, stage } = mountFigure();
    const bridge = testBridge();
    const controls = [htmlControl(11, '<html><body>a</body></html>')];
    syncUiHtmlHosts(FIGURE, stage, controls, bridge);
    const frame = hostFrame(11);

    box.remove();
    syncUiHtmlHosts(FIGURE, stage, controls, bridge);
    expect(hostFrame(11)).toBe(frame);
    expect(frame?.isConnected).toBe(true);
    expect(clipOf(FIGURE)?.style.visibility).toBe('hidden');

    document.body.appendChild(box);
    syncUiHtmlHosts(FIGURE, stage, controls, bridge);
    expect(hostFrame(11)).toBe(frame);
    expect(clipOf(FIGURE)?.style.visibility).toBe('visible');
  });

  // A control that merely left the display list (Visible off) is hidden with
  // `display`: `visibility: visible` on the frame would paint it back through
  // the hidden clip of an unselected tab.
  it('hides a vanished control without visibility', () => {
    const { stage } = mountFigure();
    const bridge = testBridge();
    syncUiHtmlHosts(FIGURE, stage, [htmlControl(11, '<html><body>a</body></html>')], bridge);
    const frame = hostFrame(11);
    expect(frame?.style.display).toBe('block');

    syncUiHtmlHosts(FIGURE, stage, [], bridge);
    expect(frame?.style.display).toBe('none');
    expect(frame?.style.visibility).toBe('');
  });

  it('creates nothing for a figure without a uihtml', () => {
    const { stage } = mountFigure();
    syncUiHtmlHosts(FIGURE, stage, [], testBridge());
    expect(document.getElementById('nelson-uihtml-hosts')).toBeNull();
  });

  // The engine must see a page's events in the order the page sent them, and the
  // RPC transport lets two calls run at once: one message leaves only once the
  // previous one has landed.
  it('sends the messages of one page to the engine in order', async () => {
    const { stage } = mountFigure();
    const sent: number[] = [];
    const pending: (() => void)[] = [];
    const bridge: UiHtmlBridge = {
      pull: () => Promise.resolve({ dataJson: 'null', revision: 0, events: [] }),
      push: (_handle, envelope) => {
        sent.push(JSON.parse(envelope).data.i as number);
        return new Promise<void>((resolve) => pending.push(resolve));
      },
      fileUrl: (handle) => `/uihtml/${handle}`,
    };
    const dispose = installUiHtmlMessageBridge();
    syncUiHtmlHosts(FIGURE, stage, [htmlControl(11, '<html><body>a</body></html>')], bridge);
    const source = hostFrame(11)?.contentWindow ?? null;
    for (const i of [1, 2, 3]) {
      window.dispatchEvent(
        new MessageEvent('message', {
          data: { __uihtml: 'event', name: 'tick', data: { i } },
          source,
        })
      );
    }

    await flush();
    expect(sent).toEqual([1]);
    pending.shift()?.();
    await flush();
    expect(sent).toEqual([1, 2]);
    pending.shift()?.();
    await flush();
    expect(sent).toEqual([1, 2, 3]);
    dispose();
  });

  // React runs a mount effect twice under StrictMode, and remounts a panel it
  // moves. The release must survive the round trip.
  it('keeps the pages across a remount and drops them with the figure', () => {
    const { stage } = mountFigure();
    syncUiHtmlHosts(FIGURE, stage, [htmlControl(11, '<html><body>a</body></html>')], testBridge());
    const frame = hostFrame(11);

    vi.useFakeTimers();
    releaseUiHtmlHosts(FIGURE);
    retainUiHtmlHosts(FIGURE);
    vi.advanceTimersByTime(1);
    expect(frame?.isConnected).toBe(true);

    releaseUiHtmlHosts(FIGURE);
    vi.advanceTimersByTime(1);
    expect(frame?.isConnected).toBe(false);
    expect(document.getElementById('nelson-uihtml-hosts')).toBeNull();
    vi.useRealTimers();
  });
});
//=============================================================================
