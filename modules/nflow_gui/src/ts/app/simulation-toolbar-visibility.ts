//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getUiPref, setUiPref } from './ui-prefs.js';
//=============================================================================
const OPEN_KEY = 'nf-simulation-toolbar-open';
function getToolbar(): HTMLElement | null {
  return document.getElementById('floatingToolbar');
}
//=============================================================================
function readOpen(): boolean {
  const value = getUiPref(OPEN_KEY, null);
  return value === null ? true : value === '1';
}
//=============================================================================
export function restoreSimulationToolbar(): void {
  if (typeof document === 'undefined') return;
  const toolbar = getToolbar();
  if (toolbar) toolbar.hidden = !readOpen();
}
//=============================================================================
export function toggleSimulationToolbar(force?: boolean): void {
  if (typeof document === 'undefined') return;
  const toolbar = getToolbar();
  if (!toolbar) return;
  const open = force !== undefined ? force : toolbar.hidden;
  toolbar.hidden = !open;
  setUiPref(OPEN_KEY, open ? '1' : '0');
}
//=============================================================================
