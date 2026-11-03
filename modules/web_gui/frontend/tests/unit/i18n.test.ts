//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import { rpc } from '../../src/rpc/client';
import { loadTranslations, subscribeTranslations, t, tf } from '../../src/i18n';
import { corePanelTitle } from '../../src/desktop/panelTitles';
//=============================================================================
vi.mock('../../src/rpc/client', () => ({ rpc: vi.fn() }));
//=============================================================================
describe('i18n', () => {
  it('keeps the English key and fills placeholders before the catalog arrives', () => {
    expect(t('Command Window')).toBe('Command Window');
    expect(tf('Open %1', 'a.m')).toBe('Open a.m');
    expect(tf('%1 (%2 updates available)', 'Packages', undefined)).toBe(
      'Packages ( updates available)'
    );
    expect(tf('Step %1 of %2', 1)).toBe('Step 1 of %2');
  });

  it('shows the keys shared with Qt without shortcut marks', () => {
    expect(t('E&xit')).toBe('Exit');
    expect(t('&Load Workspace...')).toBe('Load Workspace…');
    expect(t('Copy && Paste')).toBe('Copy & Paste');
  });

  it('loads the whole catalog once and notifies subscribers', async () => {
    vi.mocked(rpc).mockResolvedValue({
      translations: {
        'Command Window': 'Fenêtre de commandes',
        'Open %1': 'Ouvrir %1',
        '&Load Workspace...': "&Charger l'espace de travail...",
        Empty: '',
      },
    });
    const changed = vi.fn();
    const off = subscribeTranslations(changed);
    await loadTranslations();
    await loadTranslations();
    off();
    expect(rpc).toHaveBeenCalledTimes(1);
    expect(vi.mocked(rpc).mock.calls[0][1]).toEqual({});
    expect(changed).toHaveBeenCalledTimes(1);
    expect(t('Command Window')).toBe('Fenêtre de commandes');
    expect(corePanelTitle('console')).toBe('Fenêtre de commandes');
    expect(tf('Open %1', 'a.m')).toBe('Ouvrir a.m');
    expect(t('Empty')).toBe('Empty');
    expect(t('&Load Workspace...')).toBe("Charger l'espace de travail…");
    expect(corePanelTitle('figure:3')).toBe('figure:3');
  });
});
//=============================================================================
