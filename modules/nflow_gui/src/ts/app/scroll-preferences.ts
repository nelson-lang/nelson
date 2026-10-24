//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getUiPref, setUiPref } from './ui-prefs.js';
//=============================================================================
export function bindScrollPreferences(root: ParentNode = document): void {
  root.querySelectorAll<HTMLElement>('[data-preference-scroll]').forEach((element) => {
    if (element.dataset.scrollPreferenceBound === '1') return;
    const id = element.dataset.preferenceScroll;
    if (!id) return;
    const key = `nf-scroll-${id}`;
    const saved = Number.parseInt(getUiPref(key, '0') || '0', 10);
    if (Number.isFinite(saved) && saved >= 0) element.scrollTop = saved;
    element.dataset.scrollPreferenceBound = '1';
    element.addEventListener(
      'scroll',
      () => setUiPref(key, String(Math.max(0, Math.round(element.scrollTop)))),
      { passive: true }
    );
  });
}
//=============================================================================
