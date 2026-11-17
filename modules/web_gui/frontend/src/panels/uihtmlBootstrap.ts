//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The uihtml page contract, in one place.
//
// It is compiled to the standalone /uihtml-bootstrap.js asset (see the Vite
// plugin in vite.uihtml-bootstrap.ts) and served from the frontend root, which
// every window of the desktop serves - the docked shell and each separate figure
// window alike, since they share the same root folder. That matters: a figure in
// its own window is a different origin with a different port, so anything served
// only by the desktop window would 404 there.
//
// Both forms of HTMLSource reach this same code. A page given as markup is
// inlined into an iframe with srcdoc and references this absolute path, which
// resolves because an about:srcdoc document inherits the parent's base URL. A
// page given as a file is served by the engine, which injects the same tag.
//
// The page talks to the desktop through postMessage rather than the WebUI
// bridge: it is an iframe of the desktop document, not a window of its own.
//
// This module has no side effect on import: it exports installUiHtmlBootstrap(),
// which the entry (uihtmlBootstrap.entry.ts) calls once in a real page and which
// a unit test can drive against a stub parent. It returns a disposer that undoes
// everything it installed, which is what keeps tests isolated from one another.
//=============================================================================
import type {
  UiHtmlComponent,
  UiHtmlEventArgs,
  UiHtmlEventHandler,
  UiHtmlHostMessage,
  UiHtmlPageMessage,
  UiHtmlSetup,
} from './uihtmlProtocol';
//=============================================================================
export interface UiHtmlBootstrapOptions {
  // Where the page listens for host messages and reads its document. A real page
  // passes its own window (the default); a test passes an isolated one.
  self?: Window;
  // Where page -> host messages go. A real page posts to its parent frame (the
  // default); a test passes a stub to observe them.
  parent?: Pick<Window, 'postMessage'>;
}
//=============================================================================
export function installUiHtmlBootstrap(options: UiHtmlBootstrapOptions = {}): () => void {
  const view = options.self ?? window;
  const host = options.parent ?? view.parent;
  const doc = view.document;

  const listeners: Record<string, UiHtmlEventHandler[]> = {};
  let currentData: unknown = null;
  let revision = 0;
  let applying = false;
  let ready = false;
  let timeoutId = 0;

  function post(message: UiHtmlPageMessage): void {
    host.postMessage(message, '*');
  }

  // A page renders its own markup whether or not this bridge ever reached it, so
  // a broken handshake looks exactly like a working one. The badge shows how far
  // the handshake got and removes itself the moment setup() has run, so a healthy
  // page shows nothing and a stuck one says where it stopped.
  let badge: HTMLDivElement | null = null;
  let badgeOnReady: (() => void) | null = null;
  function status(text: string): void {
    if (!badge) {
      badge = doc.createElement('div');
      badge.style.cssText =
        'position:fixed;left:8px;bottom:8px;z-index:2147483647;padding:5px 9px;' +
        'background:#5b1a1a;color:#ffdede;border-radius:4px;' +
        'font:12px/1.4 ui-monospace,monospace';
      if (doc.body) {
        doc.body.appendChild(badge);
      } else {
        badgeOnReady = () => {
          if (badge) {
            doc.body.appendChild(badge);
          }
        };
        view.addEventListener('DOMContentLoaded', badgeOnReady);
      }
    }
    badge.textContent = 'uihtml: ' + text;
  }
  function clearStatus(): void {
    if (badge) {
      badge.remove();
      badge = null;
    }
  }

  function dispatch(name: string, detail: unknown): void {
    const handlers = listeners[name];
    if (!handlers) {
      return;
    }
    const args: UiHtmlEventArgs = { Source: htmlComponent, Data: detail };
    for (let i = 0; i < handlers.length; i++) {
      try {
        handlers[i](args);
      } catch (err) {
        console.error('uihtml: listener for "' + name + '" failed', err);
      }
    }
  }

  const htmlComponent: UiHtmlComponent = {
    get Data(): unknown {
      return currentData;
    },
    set Data(value: unknown) {
      currentData = value;
      // A value the engine just installed must not be reported straight back.
      if (applying) {
        return;
      }
      post({ __uihtml: 'data', value });
    },
    addEventListener(name: string, handler: UiHtmlEventHandler): void {
      if (!listeners[name]) {
        listeners[name] = [];
      }
      listeners[name].push(handler);
    },
    removeEventListener(name: string, handler: UiHtmlEventHandler): void {
      const handlers = listeners[name];
      if (!handlers) {
        return;
      }
      const index = handlers.indexOf(handler);
      if (index >= 0) {
        handlers.splice(index, 1);
      }
    },
    sendEventToNelson(name: string, data: unknown): void {
      post({ __uihtml: 'event', name, data });
    },
  };

  function onMessage(event: MessageEvent): void {
    const message = event.data as (UiHtmlHostMessage & { revision?: number }) | null | undefined;
    if (!message || typeof message !== 'object') {
      return;
    }
    if (message.__uihtml === 'apply') {
      // A stamp that is not newer is an echo of what this page already holds.
      if ((message.revision ?? 0) <= revision) {
        return;
      }
      revision = message.revision ?? 0;
      applying = true;
      try {
        currentData = message.value;
        if (ready) {
          dispatch('DataChanged', currentData);
        }
      } finally {
        applying = false;
      }
    } else if (message.__uihtml === 'event') {
      dispatch(message.name, message.data);
    } else if (message.__uihtml === 'init') {
      revision = message.revision ?? 0;
      applying = true;
      try {
        currentData = message.value;
      } finally {
        applying = false;
      }
      ready = true;
      const setup = (view as unknown as { setup?: UiHtmlSetup }).setup;
      if (typeof setup === 'function') {
        clearStatus();
        setup(htmlComponent);
      } else {
        status('this page defines no setup(htmlComponent) function');
      }
    }
  }

  view.addEventListener('message', onMessage);

  post({ __uihtml: 'ready' });
  // If the engine never answers, this line is what the user sees instead of an
  // interface that silently does nothing.
  timeoutId = view.setTimeout(() => {
    if (!ready) {
      status('bridge loaded, waiting for the engine');
    }
  }, 3000);

  return () => {
    view.removeEventListener('message', onMessage);
    if (badgeOnReady) {
      view.removeEventListener('DOMContentLoaded', badgeOnReady);
      badgeOnReady = null;
    }
    if (timeoutId !== 0) {
      view.clearTimeout(timeoutId);
      timeoutId = 0;
    }
    clearStatus();
  };
}
//=============================================================================
