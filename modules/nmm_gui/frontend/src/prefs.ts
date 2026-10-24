//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Filter, sort, selection and window size are kept between sessions in
// <prefdir>/nmm_gui.json (read and written by the C++, see NmmGuiPrefs.cpp).
import { rpc } from './rpc';
import { state } from './store';
//=============================================================================
let prefsLoaded = false;
let lastPrefs = '';
let prefsTimer = 0;
let sizeTimer = 0;
function currentPrefs() {
  return { filter: state.filter, sort: state.sort, selected: state.selected, category: state.category, license: state.license };
}
//=============================================================================
export async function loadPrefs(): Promise<void> {
  try {
    const r = await rpc('prefs');
    if (r && typeof r === 'object') {
      if (['all', 'installed', 'updates'].includes(r.filter)) state.filter = r.filter;
      if (['name', 'updates', 'installed'].includes(r.sort)) state.sort = r.sort;
      for (const key of ['selected', 'category', 'license'] as const) if (typeof r[key] === 'string') state[key] = r[key];
    }
  } catch {
  }
  lastPrefs = JSON.stringify(currentPrefs());
  prefsLoaded = true;
}
//=============================================================================
// Called after every render: saves once the state settled on a new value.
export function schedulePrefs(): void {
  if (!prefsLoaded || !state.loaded) return;
  const prefs = currentPrefs();
  const text = JSON.stringify(prefs);
  if (text === lastPrefs) return;
  lastPrefs = text;
  window.clearTimeout(prefsTimer);
  prefsTimer = window.setTimeout(() => rpc('savePrefs', { prefs }).catch(() => {}), 600);
}
//=============================================================================
export function watchWindowSize(): void {
  window.addEventListener('resize', () => {
    window.clearTimeout(sizeTimer);
    sizeTimer = window.setTimeout(() => rpc('savePrefs', { prefs: { width: window.outerWidth, height: window.outerHeight } }).catch(() => {}), 800);
  });
}
//=============================================================================
