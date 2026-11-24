//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/// <reference types="vite/client" />

declare const __NELSON_WASM__: boolean;
declare const __NELSON_WASM_NFLOW__: boolean;
declare const __NELSON_WASM_NMM__: boolean;

interface Window {
  __nf_serializeDiagram?: () => string;
  __nf_runSimulationForTest?: () => Promise<unknown>;
}

interface Navigator {
  userAgentData?: { platform?: string };
}
