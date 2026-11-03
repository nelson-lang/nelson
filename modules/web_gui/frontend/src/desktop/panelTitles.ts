//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DockviewApi } from 'dockview';
import { t } from '../i18n';
//=============================================================================
function corePanelTitles(): Record<string, string> {
  return {
    console: t('Command Window'),
    files: t('Files'),
    workspace: t('Workspace'),
    history: t('Command History'),
    newsfeed: t('News feed'),
    variables: t('Variables Editor'),
    help: t('Documentation'),
    examples: t('&Examples'),
    notebook: t('&Notebook'),
    packages: t('&Package Manager'),
    profiler: t('Profiler'),
    diagnostics: t('Diagnostics'),
    terminal: t('Terminal'),
    breakpoints: t('Breakpoints'),
    callstack: t('Call Stack'),
  };
}
//=============================================================================
// Translated tab title of a core panel (the id for any other panel).
export function corePanelTitle(id: string): string {
  return corePanelTitles()[id] ?? id;
}
//=============================================================================
// A saved layout keeps the titles of the language it was saved in.
export function retitleCorePanels(api: DockviewApi): void {
  const titles = corePanelTitles();
  for (const panel of api.panels) {
    const title = titles[panel.id];
    if (title && panel.title !== title) panel.api.setTitle(title);
  }
}
//=============================================================================
