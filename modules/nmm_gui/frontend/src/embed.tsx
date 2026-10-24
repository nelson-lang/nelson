//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The package manager as an embeddable component: the web desktop mounts it
// in a docked panel and provides the transport (its `tool.dispatch` RPC), the
// backend events (`tool.event` pushes) and the deep link nmm_gui() was called
// with. The window page (main.tsx) keeps its WebUI transport.
//=============================================================================
import { useEffect, useState } from 'react';
import { App } from './components/App';
import { setEmbedded, setRpcTransport, type Json, type RpcTransport } from './rpc';
import { loadTranslations } from './i18n';
import { consumeDeepLink, handleBackendEvent, load } from './actions';
import { loadPrefs } from './prefs';
//=============================================================================
export interface PackageManagerHost {
  transport: RpcTransport;
  /** Delivers __nmm_gui_notify__ events (progress, changed, failed, deeplink). */
  subscribe: (listener: (channel: string, payload: string) => void) => () => void;
  /** The pending nmm_gui(action, name[, version]) request, consumed once. */
  deepLink?: () => Json;
}
//=============================================================================
export function PackageManagerApp({ host }: { host: PackageManagerHost }): JSX.Element {
  const [ready, setReady] = useState(false);
  useEffect(() => {
    setEmbedded(true);
    setRpcTransport(host.transport);
    const unsubscribe = host.subscribe(handleBackendEvent);
    let cancelled = false;
    void (async () => {
      await loadTranslations();
      if (cancelled) return;
      setReady(true);
      await loadPrefs();
      await load();
      if (cancelled) return;
      await consumeDeepLink(host.deepLink ? host.deepLink() : null);
    })();
    return () => {
      cancelled = true;
      unsubscribe();
      setRpcTransport(null);
      setEmbedded(false);
    };
  }, [host]);
  return ready ? <App /> : <div className="nmm-app" />;
}
//=============================================================================
