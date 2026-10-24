//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
//=============================================================================
export type NFlowHostMode = 'webview' | 'web' | 'wasm';
let cachedHostMode: NFlowHostMode | null = null;
export async function getNFlowHostMode(): Promise<NFlowHostMode> {
  if (cachedHostMode !== null) return cachedHostMode;
  if (!canUseNFlowRpc('nflow.host.mode')) {
    cachedHostMode = 'web';
    return cachedHostMode;
  }
  try {
    const response = await nflowRpc<{ mode?: string }>('nflow.host.mode');
    cachedHostMode =
      response?.mode === 'wasm' ? 'wasm' : response?.mode === 'webview' ? 'webview' : 'web';
  } catch {
    cachedHostMode = 'web';
  }
  return cachedHostMode;
}
//=============================================================================
export async function shouldUseNativeDialogs(): Promise<boolean> {
  return (await getNFlowHostMode()) === 'webview';
}
//=============================================================================
export function clearHostModeCacheForTests(): void {
  cachedHostMode = null;
}
//=============================================================================
