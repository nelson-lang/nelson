//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unified examples catalog (NFlow models and scripts, Nelson module examples,
// external block libraries) served by the `nflow.examples.list` RPC in the
// schema of the shared gallery.
//=============================================================================
import type { ExampleCatalog } from '@nelson/example-gallery/example-gallery.js';
import { nflowRpc, canUseNFlowRpc } from './native-rpc.js';
//=============================================================================
export type {
  ExampleCatalog,
  ExampleEntry,
  ExampleGroup,
} from '@nelson/example-gallery/example-gallery.js';
//=============================================================================
let cache: ExampleCatalog | null = null;
let inflight: Promise<ExampleCatalog> | null = null;
export function examplesAvailable(): boolean {
  return canUseNFlowRpc('nflow.examples.list');
}
//=============================================================================
function emptyCatalog(): ExampleCatalog {
  return { groups: [], diagnostics: [], locale: 'en_US' };
}
//=============================================================================
export async function listExamples(options?: { refresh?: boolean }): Promise<ExampleCatalog> {
  if (options?.refresh) {
    cache = null;
    inflight = null;
  }
  if (cache) return cache;
  if (inflight) return inflight;
  inflight = (async () => {
    try {
      const res = await nflowRpc<Partial<ExampleCatalog>>('nflow.examples.list', {});
      cache = {
        groups: Array.isArray(res?.groups) ? res.groups : [],
        diagnostics: Array.isArray(res?.diagnostics) ? res.diagnostics : [],
        locale: typeof res?.locale === 'string' && res.locale ? res.locale : 'en_US',
      };
      return cache;
    } catch (_e) {
      cache = emptyCatalog();
      return cache;
    } finally {
      inflight = null;
    }
  })();
  return inflight;
}
//=============================================================================
