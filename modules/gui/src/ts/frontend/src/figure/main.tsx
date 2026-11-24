//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Slim entry for a standalone native figure window (figure.html). It mounts only
// the figure panel, so its bundle excludes the desktop shell, the Monaco editor,
// the xterm console and the dockview layout the desktop entry (index.html / App)
// pulls in. A native figure window loads this instead of index.html, which cuts
// the per-window page-load cost dramatically.
import React from 'react';
import ReactDOM from 'react-dom/client';
import { StandaloneFigureApp } from './StandaloneFigureApp';
import { initializePersistentStorage } from '../desktop/persistentStorage';
import { installBrowserZoomLock } from '../desktop/browserZoom';
import { loadTranslations } from '../i18n';
import { whenBridgeReady, rpc } from '../rpc/client';
import type { ClientRoleResult } from '../rpc/types';
import '../styles.css';
import '../panels.css';
import '../shell.css';
import '../dialogs.css';
//=============================================================================
// The window learns its figure handle from client.role; a browser popup instead
// carries it in the URL ('?figure=N'). Fall back to figure 0 if neither answers,
// so the window never renders blank. engineIdle tells the panel that it can
// fetch its first frame at once.
async function resolveFigure(): Promise<{ handle: number; engineIdle: boolean }> {
  const p = new URLSearchParams(window.location.search).get('figure');
  if (p !== null && p !== '') {
    return { handle: Number(p), engineIdle: false };
  }
  try {
    const r = await rpc<ClientRoleResult>('client.role');
    if (r.role === 'figure' && r.handle != null) {
      return { handle: r.handle, engineIdle: r.engineIdle === true };
    }
  } catch {
    // ignore: fall through to the default handle
  }
  return { handle: 0, engineIdle: false };
}
//=============================================================================
installBrowserZoomLock();
void whenBridgeReady()
  .then(() => Promise.all([initializePersistentStorage(), loadTranslations()]))
  .then(() => resolveFigure())
  .then(({ handle, engineIdle }) => {
    ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
      <React.StrictMode>
        <StandaloneFigureApp handle={handle} engineIdle={engineIdle} />
      </React.StrictMode>
    );
  });
//=============================================================================
