//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Nelson package manager window. Read-only requests return live nmm data;
// install / remove / update are queued in the Nelson console and the backend
// pushes 'progress' events, then 'changed' when they finish.
import { createRoot } from 'react-dom/client';
import './app.css';
import { App } from './components/App';
import { loadTranslations } from './i18n';
import { consumeDeepLink, installBackendHooks, load } from './actions';
import { loadPrefs, watchWindowSize } from './prefs';
//=============================================================================
async function start(): Promise<void> {
  installBackendHooks();
  watchWindowSize();
  // Labels are ready for the first render (a failed request is retried by
  // the next load, until then keys show).
  await loadTranslations();
  createRoot(document.getElementById('root')!).render(<App />);
  await loadPrefs();
  await load();
  await consumeDeepLink();
}
//=============================================================================
start();
//=============================================================================
