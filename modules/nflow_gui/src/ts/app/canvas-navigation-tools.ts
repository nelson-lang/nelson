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
const OPEN_KEY = 'nf-canvas-navigation-open';
function getTools(): HTMLElement | null {
  return document.querySelector<HTMLElement>('.canvas-navigation-tools');
}
//=============================================================================
function readOpen(): boolean {
  const value = getUiPref(OPEN_KEY, null);
  return value === null ? true : value === '1';
}
//=============================================================================
export function restoreCanvasNavigationTools(): void {
  if (typeof document === 'undefined') return;
  const tools = getTools();
  if (tools) tools.hidden = !readOpen();
}
//=============================================================================
export function toggleCanvasNavigationTools(force?: boolean): void {
  if (typeof document === 'undefined') return;
  const tools = getTools();
  if (!tools) return;
  const open = force !== undefined ? force : tools.hidden;
  tools.hidden = !open;
  setUiPref(OPEN_KEY, open ? '1' : '0');
}
//=============================================================================
