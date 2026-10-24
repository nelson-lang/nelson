//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getUiPref, setUiPref } from '../app/ui-prefs.js';
//=============================================================================
function sectionKey(section: string): string {
  const normalized = section
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-|-$/g, '');
  return `nf-inspector-section-${normalized || 'default'}-open`;
}
//=============================================================================
export function readInspectorSectionOpen(section: string, fallback: boolean): boolean {
  return getUiPref(sectionKey(section), fallback ? '1' : '0') !== '0';
}
//=============================================================================
export function writeInspectorSectionOpen(section: string, open: boolean): void {
  setUiPref(sectionKey(section), open ? '1' : '0');
}
//=============================================================================
