//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import React from 'react';
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
installBrowserZoomLock();
// host preferences and the catalog are loaded before the first render so menus and panels start translated
void whenBridgeReady()
  .then(() => Promise.all([initializePersistentStorage(), loadTranslations()]))
  .finally(() => {
    ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
      <React.StrictMode>
        <App />
      </React.StrictMode>
    );
    scheduleStartupUrlActions(window.location.href);
  });
//=============================================================================
