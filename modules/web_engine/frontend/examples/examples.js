//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Stand-alone gallery window opened by demo() on the desktop: the shared
// component over the WebUI `examplesRpc` binding (list, refresh, open, run).
//=============================================================================
import { mountExampleGallery } from './example-gallery.js';

function stopZoom(event) {
  event.preventDefault();
  event.stopImmediatePropagation();
}
function hasZoomModifier(event) {
  return event.ctrlKey || event.metaKey;
}
function isZoomKey(event) {
  const key = event.key.toLowerCase();
  return ['+', '-', '=', '0', '_', 'add', 'subtract'].includes(key);
}
window.addEventListener('keydown', (event) => { if (hasZoomModifier(event) && isZoomKey(event)) stopZoom(event); }, true);
window.addEventListener('wheel', (event) => { if (hasZoomModifier(event)) stopZoom(event); }, { capture: true, passive: false });
window.addEventListener('touchmove', (event) => { if (event.touches.length > 1) stopZoom(event); }, { capture: true, passive: false });
window.addEventListener('gesturestart', stopZoom, { capture: true, passive: false });
window.addEventListener('gesturechange', stopZoom, { capture: true, passive: false });

let rpcId = 0;
// The WebAssembly desktop opens this page in a separate tab and answers the
// gallery through a BroadcastChannel named in the URL instead of a native bridge.
const urlParams = new URLSearchParams(window.location.search);
const examplesChannelName = urlParams.get('channel') || '';
const useBrowserBridge = typeof BroadcastChannel === 'function' && examplesChannelName;
// The desktop keeps the native WebUI bridge: it serves this page with a
// `webui.js` script that injects `window.webui` only on DOMContentLoaded, which
// fires after this deferred module runs its first request. The WebAssembly
// packager strips that script, so its absence marks the static/browser build.
// On the desktop we must wait for that bridge instead of falling back to the
// static fetch path, which has no `catalog.json` file to serve (404).
const expectsNativeBridge = document.querySelector('script[src*="webui.js"]') !== null;
let gallery = null;
let examplesEventsChannel = null;

if (useBrowserBridge) {
  examplesEventsChannel = new BroadcastChannel(examplesChannelName);
  examplesEventsChannel.onmessage = (event) => {
    if (event.data?.type !== 'event' || event.data?.event !== 'changed') return;
    void gallery?.reload(true);
  };
  window.addEventListener('pagehide', () => examplesEventsChannel?.close(), { once: true });
}

function delay(milliseconds) {
  return new Promise((resolve) => window.setTimeout(resolve, milliseconds));
}

function transportReady() {
  if (typeof window.webui?.call === 'function') {
    return typeof window.webui.isConnected !== 'function' || window.webui.isConnected();
  }
  if (expectsNativeBridge) return false;
  if (useBrowserBridge) return true;
  if (typeof fetch === 'function') return true;
  return typeof window.examplesRpc === 'function';
}

async function waitForTransport() {
  for (let attempt = 0; attempt < 50; attempt += 1) {
    if (transportReady()) return;
    await delay(100);
  }
  throw new Error('Native bridge unavailable');
}

async function callTransport(payload) {
  if (typeof window.webui?.call === 'function') {
    return window.webui.call('examplesRpc', payload);
  }
  if (useBrowserBridge) {
    const request = typeof payload === 'string' ? JSON.parse(payload) : payload;
    return new Promise((resolve, reject) => {
      const channel = new BroadcastChannel(examplesChannelName);
      const timeoutMilliseconds = request.method === 'run' ? 120000 : 10000;
      const timer = window.setTimeout(() => {
        channel.close();
        reject(new Error('Browser examples bridge unavailable'));
      }, timeoutMilliseconds);
      channel.onmessage = (event) => {
        if (event.data?.type !== 'response' || event.data?.id !== request.id) return;
        window.clearTimeout(timer);
        channel.close();
        resolve(event.data.payload);
      };
      channel.postMessage({ type: 'request', payload: request });
    });
  }
  if (typeof fetch === 'function') {
    const request = typeof payload === 'string' ? JSON.parse(payload) : payload;
    if (request.method === 'list' || request.method === 'refresh') {
      const response = await fetch(new URL('catalog.json', window.location.href));
      if (!response.ok) throw new Error(`Cannot load examples catalog (${response.status})`);
      return { id: request.id, result: await response.json() };
    }
    throw new Error('Open this example from the Nelson WebAssembly desktop to run it.');
  }
  return window.examplesRpc(payload);
}

async function rpc(method, params = {}) {
  await waitForTransport();
  const raw = await callTransport(JSON.stringify({ id: ++rpcId, method, params }));
  const response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  if (response.error) throw new Error(response.error.message || 'Request failed');
  return response.result;
}

// The interpreter validates every path and command against its own catalog,
// so the page never builds a command line itself.
function action(method, entry) {
  return entry.kind === 'command' ? rpc(method, { command: entry.command }) : rpc(method, { path: entry.file });
}

gallery = mountExampleGallery(document.getElementById('gallery'), {
  primarySource: 'nelson',
  hasEditor: true,
  compact: 'auto',
  load: ({ refresh }) => rpc(refresh ? 'refresh' : 'list'),
  onRun: (entry) => action('run', entry),
  onOpen: (entry) => action('open', entry),
});

window.reloadExamples = () => gallery.reload(false);
