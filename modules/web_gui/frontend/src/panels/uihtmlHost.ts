//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Hosts the iframe of every uihtml component of a figure.
//
// The iframes are created and moved with plain DOM calls, never by React. An
// iframe reloads its document whenever the browser MOVES its element in the DOM,
// and FigureControls maps its controls in the order of the flattened child walk,
// so a reorder there would restart the page and lose its state. A host outside
// the mapped list survives both re-rendering and reordering.
//
// For the same reason they cannot live inside the figure panel. The dock renders
// only the selected tab of a group: it takes the whole content element of an
// unselected panel OUT of the document, and an iframe that leaves the document
// loses its page. So the hosts live in a container of this module's own,
// appended to the document once and never re-parented, and they FOLLOW the
// figure's drawing stage: same rectangle, same scale. When the stage is gone the
// hosts are hidden, never detached.
//
// The page must also survive Visible off and a change of parent: the engine
// reloads a page only when HTMLSource changes to a different value. So a host is
// hidden, never destroyed, while its component still exists.
import type { UIControl } from '../rpc/types';
//=============================================================================
export interface UiHtmlPullResult {
  dataJson: string;
  revision: number;
  events: { name: string; data: string }[];
}
//=============================================================================
export interface UiHtmlBridge {
  // Fetch Data and any queued engine -> page events, and acknowledge them.
  pull: (handle: number) => Promise<UiHtmlPullResult>;
  // Page -> engine: a Data write or a named event, already JSON encoded. The
  // promise settles when the engine has taken it, which is what lets one page's
  // messages be sent strictly in order.
  push: (handle: number, envelope: string) => Promise<void>;
  // URL serving a file HTMLSource, with its relative assets alongside.
  fileUrl: (handle: number, source: string) => string;
}
//=============================================================================
interface HostEntry {
  frame: HTMLIFrameElement;
  source: string;
  loaded: boolean;
  revision: number;
  eventSeq: number;
  // An existence check is in flight for a host whose control vanished.
  pendingRemoval: boolean;
  // Set when the page's bootstrap announced itself. A page can load and render
  // its own markup without it, which looks like success and is not.
  announced: boolean;
  errorBox?: HTMLDivElement;
}
//=============================================================================
interface FigureHosts {
  // Fixed over the figure's canvas box, clipping whatever overflows it exactly
  // as that box does.
  clip: HTMLDivElement;
  // Engine-sized and scaled like the figure's drawing stage, so a control box
  // in engine pixels lands where the canvas draws it.
  layer: HTMLDivElement;
  hosts: Map<number, HostEntry>;
  // The panel's transport to the engine, refreshed on every sync.
  bridge: UiHtmlBridge;
  // The figure's stage element: read for its geometry, never written to.
  anchor: HTMLElement | null;
  // Last geometry written, so an unchanged frame writes nothing.
  geometry: string;
  shown: boolean;
  // setTimeout id of a pending release, 0 when none.
  release: number;
}
//=============================================================================
let rootElement: HTMLDivElement | null = null;
const figures = new Map<number, FigureHosts>();
let trackingFrame = 0;
let trackingTimer = 0;
let messageBridgeUsers = 0;
// One promise chain per page, keeping its messages to the engine in order.
const pushChains = new Map<number, Promise<void>>();
// The page announces itself when its bootstrap has run. Keyed by handle so the
// message bridge can start the handshake without depending on the iframe load
// event, whose ordering against the bootstrap is not something to bet on.
const readyHandlers = new Map<number, () => void>();
// The page contract lives in ONE place, served by the engine at this URL, so a
// srcdoc page and a served file run identical code. An about:srcdoc document
// inherits the parent's base URL, which is what makes this absolute path
// resolve from inside srcdoc too.
const BOOTSTRAP = '<script src="/uihtml-bootstrap.js"></script>';
// Above the docked panels, below the menus (150, 200), the disconnected banner
// (1000) and the dialogs (1100).
const HOST_Z_INDEX = 90;
const NO_BOOTSTRAP = 'the page loaded but its bootstrap never ran (/uihtml-bootstrap.js)';
function showHostError(entry: HostEntry, text: string): void {
  let box = entry.errorBox;
  if (!box) {
    box = document.createElement('div');
    box.style.cssText =
      'position:absolute;z-index:5;padding:6px 9px;background:#5b1a1a;color:#ffdede;' +
      'font:12px/1.4 ui-monospace,monospace;border-radius:4px;max-width:90%';
    entry.frame.parentElement?.appendChild(box);
    entry.errorBox = box;
  }
  box.textContent = 'uihtml: ' + text;
  box.style.left = entry.frame.style.left;
  box.style.top = entry.frame.style.top;
}
//=============================================================================
function clearHostError(entry: HostEntry): void {
  if (entry.errorBox) {
    entry.errorBox.remove();
    entry.errorBox = undefined;
  }
}
//=============================================================================
// The one container every host lives in. Zero-sized, so it can never cover
// anything: the clips inside are fixed to the viewport themselves.
function hostRoot(): HTMLElement {
  if (rootElement && rootElement.isConnected) {
    return rootElement;
  }
  const root = document.createElement('div');
  root.id = 'nelson-uihtml-hosts';
  root.style.cssText = `position:fixed;left:0;top:0;width:0;height:0;z-index:${HOST_Z_INDEX}`;
  document.body.appendChild(root);
  rootElement = root;
  return root;
}
//=============================================================================
function figureHostsFor(figure: number, bridge: UiHtmlBridge): FigureHosts {
  const known = figures.get(figure);
  if (known) {
    return known;
  }
  const clip = document.createElement('div');
  clip.setAttribute('data-uihtml-figure', String(figure));
  // Hidden until the first geometry pass says where the figure is.
  clip.style.cssText =
    'position:fixed;left:0;top:0;width:0;height:0;overflow:hidden;' +
    'pointer-events:none;visibility:hidden';
  const layer = document.createElement('div');
  layer.style.cssText = 'position:absolute;left:0;top:0;transform-origin:0 0;pointer-events:none';
  clip.appendChild(layer);
  hostRoot().appendChild(clip);
  const entry: FigureHosts = {
    clip,
    layer,
    hosts: new Map<number, HostEntry>(),
    bridge,
    anchor: null,
    geometry: '',
    shown: false,
    release: 0,
  };
  figures.set(figure, entry);
  return entry;
}
//=============================================================================
function hideFigureHosts(entry: FigureHosts): void {
  if (entry.shown) {
    entry.shown = false;
    entry.clip.style.visibility = 'hidden';
  }
}
//=============================================================================
// Put the clip over the figure's canvas box and the layer over its drawing
// stage. Idempotent and cheap: nothing is written unless a number moved.
function trackGeometry(entry: FigureHosts): void {
  const anchor = entry.anchor;
  const box = anchor?.parentElement ?? null;
  // A stage taken out of the document (unselected tab) or not rendered at all
  // (display none, zero-height group) has no geometry to follow. Its pages stay
  // loaded and merely stop being painted.
  if (!anchor || !box || !anchor.isConnected) {
    hideFigureHosts(entry);
    return;
  }
  const stage = anchor.getBoundingClientRect();
  const clip = box.getBoundingClientRect();
  const width = anchor.offsetWidth;
  const height = anchor.offsetHeight;
  if (clip.width <= 0 || clip.height <= 0 || width <= 0 || height <= 0) {
    hideFigureHosts(entry);
    return;
  }
  const left = stage.left - clip.left;
  const top = stage.top - clip.top;
  const scaleX = stage.width / width;
  const scaleY = stage.height / height;
  const geometry = [
    clip.left,
    clip.top,
    clip.width,
    clip.height,
    left,
    top,
    width,
    height,
    scaleX,
    scaleY,
  ].join(' ');
  if (geometry !== entry.geometry) {
    entry.geometry = geometry;
    entry.clip.style.left = `${clip.left}px`;
    entry.clip.style.top = `${clip.top}px`;
    entry.clip.style.width = `${clip.width}px`;
    entry.clip.style.height = `${clip.height}px`;
    entry.layer.style.left = `${left}px`;
    entry.layer.style.top = `${top}px`;
    entry.layer.style.width = `${width}px`;
    entry.layer.style.height = `${height}px`;
    entry.layer.style.transform = `scale(${scaleX}, ${scaleY})`;
  }
  if (!entry.shown) {
    entry.shown = true;
    entry.clip.style.visibility = 'visible';
  }
}
//=============================================================================
// Follow the figures: a dock layout change, a sash drag and a tab switch are
// only visible here as a new rectangle. Nothing runs until a uihtml exists, and
// both drivers stop with the last one.
//
// It runs per frame so the hosts never lag behind a drag, and on a slow
// heartbeat too: a window that paints no frame at all (occluded, minimised) must
// still stop showing the hosts of a panel the dock has taken away.
const TRACK_INTERVAL = 200;
function trackHostGeometry(): void {
  if (trackingTimer === 0) {
    trackingTimer = window.setInterval(() => figures.forEach(trackGeometry), TRACK_INTERVAL);
  }
  if (trackingFrame !== 0 || typeof requestAnimationFrame !== 'function') {
    return;
  }
  const step = () => {
    trackingFrame = 0;
    if (figures.size === 0) {
      return;
    }
    figures.forEach(trackGeometry);
    trackingFrame = requestAnimationFrame(step);
  };
  trackingFrame = requestAnimationFrame(step);
}
//=============================================================================
function stopHostGeometry(): void {
  if (trackingTimer !== 0) {
    window.clearInterval(trackingTimer);
    trackingTimer = 0;
  }
  if (trackingFrame !== 0 && typeof cancelAnimationFrame === 'function') {
    cancelAnimationFrame(trackingFrame);
    trackingFrame = 0;
  }
}
//=============================================================================
function dropFigureHosts(figure: number): void {
  const entry = figures.get(figure);
  if (!entry) {
    return;
  }
  entry.hosts.forEach((_host, handle) => {
    readyHandlers.delete(handle);
    pushChains.delete(handle);
  });
  entry.clip.remove();
  figures.delete(figure);
  if (figures.size === 0) {
    stopHostGeometry();
    if (rootElement) {
      rootElement.remove();
      rootElement = null;
    }
  }
}
//=============================================================================
function cancelRelease(entry: FigureHosts): void {
  if (entry.release !== 0) {
    window.clearTimeout(entry.release);
    entry.release = 0;
  }
}
//=============================================================================
// The figure panel left React. Its pages go too, but not on the spot: React runs
// every mount effect twice under StrictMode and remounts a panel it moves, and
// dropping the pages there would reload them. Retaining the figure again cancels
// the release.
export function releaseUiHtmlHosts(figure: number): void {
  const entry = figures.get(figure);
  if (!entry || entry.release !== 0) {
    return;
  }
  entry.anchor = null;
  hideFigureHosts(entry);
  entry.release = window.setTimeout(() => {
    const current = figures.get(figure);
    if (current && current.release !== 0) {
      dropFigureHosts(figure);
    }
  }, 0);
}
//=============================================================================
export function retainUiHtmlHosts(figure: number): void {
  const entry = figures.get(figure);
  if (entry) {
    cancelRelease(entry);
  }
}
//=============================================================================
function documentFor(source: string): string {
  const trimmed = source.trimStart();
  const injected = BOOTSTRAP;
  const head = trimmed.search(/<\/head>/i);
  if (head >= 0) {
    return trimmed.slice(0, head) + injected + trimmed.slice(head);
  }
  const body = trimmed.search(/<body[^>]*>/i);
  if (body >= 0) {
    const close = trimmed.indexOf('>', body);
    return trimmed.slice(0, close + 1) + injected + trimmed.slice(close + 1);
  }
  return injected + trimmed;
}
//=============================================================================
function isMarkup(source: string): boolean {
  return source.trimStart().startsWith('<');
}
//=============================================================================
// `first` distinguishes the handshake that runs setup() from a later Data
// update, which must fire DataChanged instead of replaying setup().
async function pushState(
  entry: HostEntry,
  handle: number,
  bridge: UiHtmlBridge,
  first: boolean
): Promise<void> {
  let state: UiHtmlPullResult;
  try {
    state = await bridge.pull(handle);
  } catch (err) {
    // A silent failure leaves a blank page with no clue: show it on top of the
    // frame.
    showHostError(entry, String((err as Error)?.message ?? err));
    return;
  }
  clearHostError(entry);
  entry.revision = state.revision;
  entry.frame.contentWindow?.postMessage(
    {
      __uihtml: first ? 'init' : 'apply',
      revision: state.revision,
      value: JSON.parse(state.dataJson || 'null'),
    },
    '*'
  );
  for (const event of state.events) {
    entry.frame.contentWindow?.postMessage(
      { __uihtml: 'event', name: event.name, data: JSON.parse(event.data || 'null') },
      '*'
    );
  }
}
//=============================================================================
// A page that rendered its markup but never announced itself lost its bootstrap,
// unless nothing was served at all: a file HTMLSource that no file answers gets
// WebUI's own 404 page, which carries no bootstrap either. Ask the URL before
// deciding what to report.
function reportSilentPage(
  hosts: Map<number, HostEntry>,
  handle: number,
  source: string,
  bridge: UiHtmlBridge
): void {
  const stillSilent = (): HostEntry | null => {
    const current = hosts.get(handle);
    return current && !current.announced && current.source === source ? current : null;
  };
  const current = stillSilent();
  if (!current) {
    return;
  }
  if (source.length === 0 || isMarkup(source)) {
    showHostError(current, NO_BOOTSTRAP);
    return;
  }
  void fetch(bridge.fileUrl(handle, source))
    .then((response) => {
      const host = stillSilent();
      if (host) {
        showHostError(host, response.ok ? NO_BOOTSTRAP : `HTMLSource file not found: ${source}`);
      }
    })
    .catch(() => {
      const host = stillSilent();
      if (host) {
        showHostError(host, `HTMLSource file could not be served: ${source}`);
      }
    });
}
//=============================================================================
// Brings the hosts of one figure in line with `controls`. `anchor` is the
// figure's drawing stage, followed for geometry.
export function syncUiHtmlHosts(
  figure: number,
  anchor: HTMLElement | null,
  controls: UIControl[],
  bridge: UiHtmlBridge
): void {
  const known = figures.get(figure);
  if (!known && !controls.some((control) => control.style === 'html')) {
    // A figure without a uihtml costs nothing: no container, layer or tracking
    // loop.
    return;
  }
  const entry = known ?? figureHostsFor(figure, bridge);
  cancelRelease(entry);
  entry.bridge = bridge;
  entry.anchor = anchor;
  const hosts = entry.hosts;
  const seen = new Set<number>();

  for (const control of controls) {
    if (control.style !== 'html') {
      continue;
    }
    seen.add(control.handle);
    const extra = (control.data ?? {}) as {
      source?: string;
      revision?: number;
      eventSeq?: number;
    };
    const source = extra.source ?? '';
    let host = hosts.get(control.handle);

    if (!host) {
      const frame = document.createElement('iframe');
      frame.style.cssText = 'position:absolute;border:0;background:#fff;pointer-events:auto';
      frame.setAttribute('data-uihtml-handle', String(control.handle));
      entry.layer.appendChild(frame);
      host = {
        frame,
        source: '',
        loaded: false,
        revision: -1,
        eventSeq: -1,
        pendingRemoval: false,
        announced: false,
      };
      hosts.set(control.handle, host);
    }

    // Geometry: the control box is already in engine pixels, top-left origin,
    // and the layer carries the stage's own scale.
    host.frame.style.left = `${control.x}px`;
    host.frame.style.top = `${control.y}px`;
    host.frame.style.width = `${control.w}px`;
    host.frame.style.height = `${control.h}px`;

    // Only a DIFFERENT source reloads the page, exactly as the engine does.
    if (source !== host.source) {
      host.source = source;
      host.loaded = false;
      host.revision = -1;
      host.eventSeq = -1;
      const handle = control.handle;
      const current = host;
      // A page announces itself once per document, so every announcement means a
      // document with no state yet, including the one after a reload. The
      // handshake puts setup() and its listeners back; treating a reload as
      // initialised leaves a page that renders but answers nothing.
      const start = () => {
        const announcing = hosts.get(handle);
        if (!announcing) {
          return;
        }
        announcing.announced = true;
        announcing.loaded = true;
        clearHostError(announcing);
        void pushState(announcing, handle, bridge, true);
      };
      readyHandlers.set(handle, start);
      current.announced = false;
      // Silence is indistinguishable from success, so say what went wrong.
      window.setTimeout(() => reportSilentPage(hosts, handle, source, bridge), 4000);
      if (isMarkup(source)) {
        current.frame.srcdoc = documentFor(source);
      } else if (source.length > 0) {
        // A file is served by the engine under /uihtml/<handle>, next to its
        // own assets. Pull first: that is what registers the route, and the
        // browser must not request the page before it exists.
        void bridge.pull(handle).then(() => {
          current.frame.removeAttribute('srcdoc');
          current.frame.src = bridge.fileUrl(handle, source);
        });
      } else {
        current.frame.srcdoc = documentFor('<html><body></body></html>');
      }
      continue;
    }

    if (!host.loaded) {
      continue;
    }
    const revision = extra.revision ?? 0;
    const eventSeq = extra.eventSeq ?? 0;
    if (revision !== host.revision || eventSeq !== host.eventSeq) {
      host.revision = revision;
      host.eventSeq = eventSeq;
      void pushState(host, control.handle, bridge, false);
    }
  }

  // A control drops out of the display list both when its component is deleted
  // AND when it is merely hidden (an unselected tab is not walked, Visible off
  // returns early). The page must survive the second case, so the host is hidden
  // at once and dropped only when the engine confirms the component is gone.
  // Hiding uses `display`, never `visibility`: a visible frame inside a hidden
  // clip would paint itself back into view.
  for (const [handle, host] of hosts) {
    if (seen.has(handle)) {
      host.frame.style.display = 'block';
      continue;
    }
    host.frame.style.display = 'none';
    if (host.pendingRemoval) {
      continue;
    }
    host.pendingRemoval = true;
    void bridge
      .pull(handle)
      .then(() => {
        const current = hosts.get(handle);
        if (current) {
          current.pendingRemoval = false;
        }
      })
      .catch(() => {
        const current = hosts.get(handle);
        if (current) {
          clearHostError(current);
          current.frame.remove();
          hosts.delete(handle);
          readyHandlers.delete(handle);
          pushChains.delete(handle);
        }
      });
  }

  trackGeometry(entry);
  trackHostGeometry();
}
//=============================================================================
// Which page sent this message. Asking our own hosts, rather than the document,
// keeps a foreign iframe out of the answer and names the figure that owns it.
function senderOf(
  source: MessageEventSource | null
): { entry: FigureHosts; handle: number } | null {
  for (const entry of figures.values()) {
    for (const [handle, host] of entry.hosts) {
      if (host.frame.contentWindow === source) {
        return { entry, handle };
      }
    }
  }
  return null;
}
//=============================================================================
// The order of a page's events is part of the contract, but the RPC transport
// allows two calls in flight, enough for two events posted in the same tick to
// arrive reversed. Each page has its own chain: the next message leaves only
// after the previous one landed.
function pushInOrder(entry: FigureHosts, handle: number, envelope: string): void {
  const previous = pushChains.get(handle) ?? Promise.resolve();
  const next = previous
    .catch(() => undefined)
    .then(() => entry.bridge.push(handle, envelope))
    .catch(() => undefined);
  pushChains.set(handle, next);
}
//=============================================================================
function onPageMessage(event: MessageEvent): void {
  const message = event.data as
    | { __uihtml?: string; value?: unknown; name?: string; data?: unknown }
    | undefined;
  if (!message || typeof message !== 'object' || !message.__uihtml) {
    return;
  }
  const sender = senderOf(event.source);
  if (!sender) {
    return;
  }
  const { entry, handle } = sender;
  if (message.__uihtml === 'ready') {
    readyHandlers.get(handle)?.();
    return;
  }
  if (message.__uihtml === 'data') {
    pushInOrder(entry, handle, JSON.stringify({ kind: 'data', value: message.value ?? null }));
  } else if (message.__uihtml === 'event') {
    pushInOrder(
      entry,
      handle,
      JSON.stringify({ kind: 'event', name: message.name ?? '', data: message.data ?? null })
    );
  }
}
//=============================================================================
// Routes what the pages post back to the engine. One listener per document,
// however many figure panels ask: every host is always in the document, so a
// listener per panel would answer for another panel's pages and send each message
// once per open figure.
export function installUiHtmlMessageBridge(): () => void {
  messageBridgeUsers += 1;
  if (messageBridgeUsers === 1) {
    window.addEventListener('message', onPageMessage);
  }
  return () => {
    messageBridgeUsers -= 1;
    if (messageBridgeUsers === 0) {
      window.removeEventListener('message', onPageMessage);
    }
  };
}
//=============================================================================
