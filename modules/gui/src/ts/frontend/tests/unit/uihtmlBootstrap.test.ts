//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Drives the uihtml page bridge against a stub parent in jsdom. The bridge posts
// to `parent` and listens on `self`; the test supplies both, so it exercises the
// full postMessage handshake without a second real frame.
//=============================================================================
import { afterEach, describe, expect, it, vi } from 'vitest';
import { installUiHtmlBootstrap } from '../../src/panels/uihtmlBootstrap';
import type { UiHtmlComponent, UiHtmlEventArgs } from '../../src/panels/uihtmlProtocol';
//=============================================================================
let dispose: (() => void) | null = null;
//=============================================================================
interface Harness {
  posts: ReturnType<typeof vi.fn>;
  send: (data: unknown) => void;
}
//=============================================================================
function install(): Harness {
  const posts = vi.fn();
  dispose = installUiHtmlBootstrap({
    self: window,
    parent: { postMessage: posts as unknown as Window['postMessage'] },
  });
  return {
    posts,
    send: (data: unknown) => window.dispatchEvent(new MessageEvent('message', { data })),
  };
}
//=============================================================================
function setSetup(run: (component: UiHtmlComponent) => void): void {
  (window as unknown as { setup?: (component: UiHtmlComponent) => void }).setup = run;
}
//=============================================================================
afterEach(() => {
  dispose?.();
  dispose = null;
  delete (window as unknown as { setup?: unknown }).setup;
  document.body.innerHTML = '';
  vi.useRealTimers();
});
//=============================================================================
describe('uihtml bootstrap', () => {
  it('announces itself with a ready message on install', () => {
    const { posts } = install();
    expect(posts).toHaveBeenCalledWith({ __uihtml: 'ready' }, '*');
  });

  it('runs setup() on init and exposes the initial Data', () => {
    let seen: UiHtmlComponent | null = null;
    setSetup((component) => {
      seen = component;
    });
    const { send } = install();
    send({ __uihtml: 'init', revision: 1, value: { hello: 'world' } });
    expect(seen).not.toBeNull();
    expect((seen as unknown as UiHtmlComponent).Data).toEqual({ hello: 'world' });
  });

  it('shows a badge when the page defines no setup()', () => {
    const { send } = install();
    send({ __uihtml: 'init', revision: 1, value: null });
    expect(document.body.textContent).toContain(
      'uihtml: this page defines no setup(htmlComponent) function'
    );
  });

  it('fires DataChanged on a newer apply and updates Data', () => {
    const changed = vi.fn();
    let component: UiHtmlComponent | null = null;
    setSetup((c) => {
      component = c;
      c.addEventListener('DataChanged', changed);
    });
    const { send } = install();
    send({ __uihtml: 'init', revision: 1, value: 1 });
    send({ __uihtml: 'apply', revision: 2, value: 2 });
    expect(changed).toHaveBeenCalledTimes(1);
    expect((component as unknown as UiHtmlComponent).Data).toBe(2);
    const args = changed.mock.calls[0][0] as UiHtmlEventArgs;
    expect(args.Data).toBe(2);
    expect(args.Source).toBe(component);
  });

  it('ignores an apply whose revision is not newer', () => {
    const changed = vi.fn();
    let component: UiHtmlComponent | null = null;
    setSetup((c) => {
      component = c;
      c.addEventListener('DataChanged', changed);
    });
    const { send } = install();
    send({ __uihtml: 'init', revision: 5, value: 'a' });
    send({ __uihtml: 'apply', revision: 5, value: 'b' });
    send({ __uihtml: 'apply', revision: 4, value: 'c' });
    expect(changed).not.toHaveBeenCalled();
    expect((component as unknown as UiHtmlComponent).Data).toBe('a');
  });

  it('posts a data message when the page writes Data', () => {
    let component: UiHtmlComponent | null = null;
    setSetup((c) => {
      component = c;
    });
    const { posts, send } = install();
    send({ __uihtml: 'init', revision: 1, value: null });
    posts.mockClear();
    (component as unknown as UiHtmlComponent).Data = { a: 1 };
    expect(posts).toHaveBeenCalledWith({ __uihtml: 'data', value: { a: 1 } }, '*');
  });

  it('does not echo an engine apply back to the host', () => {
    setSetup(() => undefined);
    const { posts, send } = install();
    send({ __uihtml: 'init', revision: 1, value: null });
    posts.mockClear();
    send({ __uihtml: 'apply', revision: 2, value: { from: 'engine' } });
    expect(posts).not.toHaveBeenCalled();
  });

  it('sends named events to the engine', () => {
    let component: UiHtmlComponent | null = null;
    setSetup((c) => {
      component = c;
    });
    const { posts, send } = install();
    send({ __uihtml: 'init', revision: 1, value: null });
    posts.mockClear();
    (component as unknown as UiHtmlComponent).sendEventToNelson('clicked', { button: 2 });
    expect(posts).toHaveBeenCalledWith(
      { __uihtml: 'event', name: 'clicked', data: { button: 2 } },
      '*'
    );
  });

  it('delivers incoming engine events to registered listeners', () => {
    const handler = vi.fn();
    setSetup((c) => {
      c.addEventListener('custom', handler);
    });
    const { send } = install();
    send({ __uihtml: 'init', revision: 1, value: null });
    send({ __uihtml: 'event', name: 'custom', data: 42 });
    expect(handler).toHaveBeenCalledTimes(1);
    expect((handler.mock.calls[0][0] as UiHtmlEventArgs).Data).toBe(42);
  });

  it('stops delivering after removeEventListener', () => {
    const handler = vi.fn();
    let component: UiHtmlComponent | null = null;
    setSetup((c) => {
      component = c;
      c.addEventListener('custom', handler);
    });
    const { send } = install();
    send({ __uihtml: 'init', revision: 1, value: null });
    (component as unknown as UiHtmlComponent).removeEventListener('custom', handler);
    send({ __uihtml: 'event', name: 'custom', data: 1 });
    expect(handler).not.toHaveBeenCalled();
  });

  it('shows a waiting badge when the engine never answers', () => {
    vi.useFakeTimers();
    install();
    vi.advanceTimersByTime(3000);
    expect(document.body.textContent).toContain('uihtml: bridge loaded, waiting for the engine');
  });

  it('clears the waiting badge once init arrives in time', () => {
    vi.useFakeTimers();
    setSetup(() => undefined);
    const { send } = install();
    send({ __uihtml: 'init', revision: 1, value: null });
    vi.advanceTimersByTime(3000);
    expect(document.body.textContent).toBe('');
  });

  it('stops listening after dispose', () => {
    const setup = vi.fn();
    setSetup(setup);
    const { send } = install();
    dispose?.();
    dispose = null;
    send({ __uihtml: 'init', revision: 1, value: null });
    expect(setup).not.toHaveBeenCalled();
  });
});
//=============================================================================
