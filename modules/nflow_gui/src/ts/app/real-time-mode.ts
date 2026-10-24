//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type RealTimeMode = 'auto' | 'real-time' | 'fast';
export function realTimeToMode(realTime: unknown): RealTimeMode {
  if (realTime === true) return 'real-time';
  if (realTime === false) return 'fast';
  return 'auto';
}
//=============================================================================
export function modeToRealTime(mode: unknown): boolean | undefined {
  if (mode === 'real-time') return true;
  if (mode === 'fast') return false;
  return undefined;
}
//=============================================================================
