//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
const labels: Record<string, string> = {
  mechanicalTranslationalIsland: 'Translational mechanism',
  physicalIsland: 'Physical mechanism',
  planarMechanicalIsland: 'Planar mechanism',
};
//=============================================================================
export function isEngineNativeBlockType(type: string): boolean {
  return Object.prototype.hasOwnProperty.call(labels, String(type || ''));
}
//=============================================================================
export function engineNativeBlockLabel(type: string): string {
  return labels[String(type || '')] || String(type || 'Internal mechanism');
}
//=============================================================================
