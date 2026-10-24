//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type ScopeConnectionMask = boolean | boolean[] | undefined;
export function isScopeSeriesConnected(mask: ScopeConnectionMask, index: number): boolean {
  if (Array.isArray(mask)) return mask[index] === true;
  return mask !== false;
}
//=============================================================================
export function areScopeSeriesConnected(mask: ScopeConnectionMask, count: number): boolean {
  for (let index = 0; index < count; index += 1) {
    if (!isScopeSeriesConnected(mask, index)) return false;
  }
  return true;
}
//=============================================================================
export function firstConnectedScopeSeries(mask: ScopeConnectionMask, count: number): number {
  for (let index = 0; index < count; index += 1) {
    if (isScopeSeriesConnected(mask, index)) return index;
  }
  return -1;
}
//=============================================================================
export function scopeConnectionKey(mask: ScopeConnectionMask, count: number): string {
  if (Array.isArray(mask)) return mask.join(',');
  return mask === false ? `none:${count}` : `all:${count}`;
}
//=============================================================================
