//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import React, { useEffect } from 'react';
import ReactDOM from 'react-dom/client';
import { App } from './App';
import { initializePersistentStorage } from './desktop/persistentStorage';
import { installBrowserZoomLock } from './desktop/browserZoom';
import { scheduleStartupUrlActions } from './desktop/startupUrlActions';
import { loadTranslations } from './i18n';
import { whenBridgeReady } from './rpc/client';
import './styles.css';
import './console.css';
import './panels.css';
import './debugger.css';
import './dialogs.css';
import './shell.css';
import '@xterm/xterm/css/xterm.css';
import 'dockview/dist/styles/dockview.css';
//=============================================================================
// Drops the splash once the desktop is on screen. The effect runs after the
// first commit of the whole tree, and two animation frames later it is painted:
// dropping the splash any earlier leaves an empty page in between.
function BootReveal({ children }: { children: React.ReactNode }): JSX.Element {
  useEffect(() => {
    requestAnimationFrame(() => requestAnimationFrame(() => window.__nelsonBoot?.done()));
  }, []);
  return <>{children}</>;
}
//=============================================================================
installBrowserZoomLock();
window.__nelsonBoot?.set(55);
// The desktop paints as soon as the bundle is ready instead of waiting on RPC:
// the layout and theme restore synchronously from localStorage, and the
// translation catalog (cached from the last session) is already in place, so the
// first render is correct. Preferences and the catalog still reconcile in the
// background; the catalog refresh re-renders through subscribeTranslations.
void whenBridgeReady()
  .then(() => {
    window.__nelsonBoot?.set(72);
  })
  .finally(() => {
    void initializePersistentStorage();
    void loadTranslations();
    window.__nelsonBoot?.set(88);
    ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
      <React.StrictMode>
        <BootReveal>
          <App />
        </BootReveal>
      </React.StrictMode>
    );
    scheduleStartupUrlActions(window.location.href);
  });
//=============================================================================
