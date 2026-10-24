//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Live document-editor session.
//
// A window opened via open_system is bound to a live NFlowModelDocument in the
// C++ registry (identified by name), shared with the scripted API. The editor
// loads it (getDiagram), commits its edits on a debounce, saves it (save), and
// reloads it on a scripted mutation (nflow.model.changed).
//
// Conflict policy: last-writer-wins with a revision guard. The host rejects a
// commit built on a stale revision; a scripted change reloads the editor only
// when it has no divergent unsaved edits.
//=============================================================================
import { nflowRpc, onNFlowPush, canUseNFlowRpc } from './native-rpc.js';
import { onAppEvent } from './app-events.js';
//=============================================================================
interface HostBootstrap {
  modelName?: string;
}
//=============================================================================
const COMMIT_DEBOUNCE_MS = 250;
let boundModelName: string | null = null;
function readBootstrapModelName(): string {
  try {
    const boot = (window as unknown as { __nf_bootstrap?: HostBootstrap }).__nf_bootstrap;
    if (boot && typeof boot.modelName === 'string' && boot.modelName) {
      return boot.modelName;
    }
  } catch (_e) {
    /* ignore */
  }
  try {
    const v = sessionStorage.getItem('nf-model-name');
    if (v) return v;
  } catch (_e) {
    /* ignore */
  }
  return '';
}
//=============================================================================
export function getBoundModelName(): string {
  if (boundModelName === null) {
    boundModelName = readBootstrapModelName();
  }
  return boundModelName;
}
//=============================================================================
export function isModelBound(): boolean {
  return getBoundModelName().length > 0;
}
//=============================================================================
// Mirrored on the window so a duplicated bundle instance shares it and an echoed
// push can be recognized (same double-bundle reason as setCurrentFilePath).
function getLastRevision(): number {
  try {
    const v = (window as unknown as { __nfModelRevision?: number }).__nfModelRevision;
    if (typeof v === 'number') return v;
  } catch (_e) {
    /* ignore */
  }
  return 0;
}
//=============================================================================
function setLastRevision(rev: number): void {
  try {
    (window as unknown as { __nfModelRevision?: number }).__nfModelRevision = rev;
  } catch (_e) {
    /* ignore */
  }
}
//=============================================================================
export interface ModelDiagram {
  content: string;
  revision: number;
  filePath: string;
  name: string;
}
//=============================================================================
export async function fetchModelDiagram(): Promise<ModelDiagram | null> {
  if (!isModelBound() || !canUseNFlowRpc('nflow.model.getDiagram')) return null;
  try {
    const raw = await nflowRpc<string>('nflow.model.getDiagram', { name: getBoundModelName() });
    const resp = JSON.parse(raw);
    if (resp && resp.success) {
      const revision = Number(resp.revision) || 0;
      setLastRevision(revision);
      return {
        content: String(resp.content || ''),
        revision,
        filePath: String(resp.filePath || ''),
        name: String(resp.name || ''),
      };
    }
  } catch (_e) {
    /* ignore */
  }
  return null;
}
//=============================================================================
export type CommitOutcome = 'ok' | 'conflict' | 'error';
export async function commitModel(content: string): Promise<CommitOutcome> {
  if (!isModelBound()) return 'error';
  try {
    const raw = await nflowRpc<string>('nflow.model.commit', {
      name: getBoundModelName(),
      baseRevision: getLastRevision(),
      content,
    });
    const resp = JSON.parse(raw);
    if (resp && resp.success) {
      setLastRevision(Number(resp.revision) || getLastRevision());
      return 'ok';
    }
    if (resp && resp.conflict) {
      return 'conflict';
    }
  } catch (_e) {
    /* ignore */
  }
  return 'error';
}
//=============================================================================
// Returns {success, needPath?, filePath?}.
export async function saveModel(content: string, path?: string): Promise<Record<string, unknown>> {
  const params: Record<string, unknown> = { name: getBoundModelName(), content };
  if (path) params.path = path;
  const raw = await nflowRpc<string>('nflow.model.save', params);
  const resp = JSON.parse(raw) as Record<string, unknown>;
  if (resp['success'] === true && typeof resp['revision'] === 'number') {
    setLastRevision(resp['revision'] as number);
  }
  return resp;
}
//=============================================================================
let debounceTimer: ReturnType<typeof setTimeout> | null = null;
let serializeFn: (() => string) | null = null;
// No-op when unbound.
export function initModelSession(opts: {
  serialize: () => string;
  onModelChanged: () => void | Promise<void>;
}): void {
  if (!isModelBound()) return;
  serializeFn = opts.serialize;

  const scheduleCommit = () => {
    if (debounceTimer) clearTimeout(debounceTimer);
    debounceTimer = setTimeout(() => {
      debounceTimer = null;
      if (serializeFn) void commitModel(serializeFn());
    }, COMMIT_DEBOUNCE_MS);
  };
  window.addEventListener('diagramChanged', scheduleCommit);

  onNFlowPush('nflow.model.changed', (payload: unknown) => {
    const origin = (payload as { origin?: string })?.origin;
    if (origin && origin !== 'script') return;
    void opts.onModelChanged();
  });

  // gcb reads the current block from the editor selection.
  onAppEvent('nf-block-selected', (detail) => {
    const blockId = detail?.blockId;
    const name = getBoundModelName();
    const block = blockId ? `${name}/${blockId}` : '';
    void nflowRpc('nflow.context.set', { name, block }).catch(() => {});
  });
}
//=============================================================================
export async function flushPendingCommit(): Promise<void> {
  if (debounceTimer) {
    clearTimeout(debounceTimer);
    debounceTimer = null;
  }
  if (serializeFn) {
    await commitModel(serializeFn());
  }
}
//=============================================================================
