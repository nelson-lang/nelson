//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { state, deepClone } from './state.js';
import { serializeDiagram } from './file-operations.js';
import { getViewBox } from './workspace-handlers.js';
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
//=============================================================================
type RendererRef = {
  current: {
    createBlock?: (
      type: string,
      x: number,
      y: number,
      options?: Record<string, unknown>
    ) => Record<string, unknown> | null | undefined;
    createConnection?: (
      fromId: string | number,
      toId: string | number,
      toIndex?: number,
      fromIndex?: number
    ) => unknown;
    deleteBlock?: (id: string) => void;
    updateSelectionBox?: () => void;
    clearWorkspace?: () => void;
    updateConnections?: (force?: boolean) => void;
    forceFullRoute?: (timeMs?: number) => Promise<any> | void;
  } | null;
} | null;
//=============================================================================
type Snapshot = { diagram: Record<string, unknown>; ts: number };
let rendererRef: RendererRef = null;
let undoStack: Snapshot[] = [];
let redoStack: Snapshot[] = [];
const maxStack = 200;
let applying = false;
let pushTimer = 0 as unknown as number;
// Fallback when `navigator.clipboard` is unavailable
let _clipboard: string | null = null;
async function loadHistoryDiagram(diagram: Record<string, unknown>): Promise<void> {
  const { loadDiagram } = await import('./diagram-handlers.js');
  await loadDiagram(diagram);
}
//=============================================================================
// Restores `target` (the new top of the undo stack) without recording it.
// The top is then re-serialized from the loaded state, so change events the
// load emits afterwards compare equal to it and push nothing; an edit made
// right after the undo/redo is recorded normally.
async function applySnapshot(target: Snapshot | undefined): Promise<boolean> {
  if (pushTimer) {
    clearTimeout(pushTimer as unknown as number);
    pushTimer = 0 as unknown as number;
  }
  applying = true;
  try {
    await loadHistoryDiagram(target?.diagram || { blocks: [], connections: [] });
  } catch (_e) {
    return false;
  } finally {
    applying = false;
  }
  if (pushTimer) {
    clearTimeout(pushTimer as unknown as number);
    pushTimer = 0 as unknown as number;
  }
  if (target) {
    try {
      target.diagram = serializeDiagram(state);
    } catch (_e) {
      // keep the stored snapshot
    }
  }
  return true;
}
//=============================================================================
function pushSnapshot(): void {
  if (applying) return;
  try {
    const snap: Snapshot = { diagram: serializeDiagram(state), ts: Date.now() };
    const isEmptyDiagram =
      (!Array.isArray(snap.diagram?.blocks) ||
        (snap.diagram!.blocks as Array<unknown>).length === 0) &&
      (!Array.isArray(snap.diagram?.connections) ||
        (snap.diagram!.connections as Array<unknown>).length === 0);
    if (undoStack.length === 0 && isEmptyDiagram && state.loadingDiagram) {
      return;
    }
    const last = undoStack[undoStack.length - 1];
    if (last && JSON.stringify(last.diagram) === JSON.stringify(snap.diagram)) return;
    undoStack.push(snap);
    if (undoStack.length > maxStack) undoStack.shift();
    redoStack = [];
  } catch (_e) {
    // ignore
  }
}
//=============================================================================
function schedulePush(): void {
  if (applying) return;
  if (pushTimer) {
    clearTimeout(pushTimer as unknown as number);
  }
  pushTimer = setTimeout(() => {
    pushTimer = 0 as unknown as number;
    // Postpone the snapshot until a renderer drag finishes.
    if (state.isDragging) {
      pushTimer = setTimeout(() => {
        pushTimer = 0 as unknown as number;
        if (!applying) {
          pushSnapshot();
        }
      }, 120) as unknown as number;
      return;
    }
    if (!applying) {
      pushSnapshot();
    }
  }, 300) as unknown as number;
}
//=============================================================================
export function initHistory({ renderer }: { renderer: RendererRef }): void {
  rendererRef = renderer;
  window.addEventListener('diagramChanged', () => {
    schedulePush();
  });
  const onCommitted = () => {
    try {
      if (pushTimer) {
        clearTimeout(pushTimer as unknown as number);
        pushTimer = 0 as unknown as number;
      }
      pushSnapshot();
    } catch (_e) {
      // ignore
    }
  };
  window.addEventListener('diagramCommitted', onCommitted);
  // Tests expect an initial snapshot.
  pushSnapshot();

  // Replace an empty initial snapshot with the loaded diagram.
  const onLoaded = () => {
    try {
      if (undoStack.length > 0) {
        const first = undoStack[0];
        const isEmptyDiagram =
          (!Array.isArray(first.diagram?.blocks) ||
            (first.diagram!.blocks as Array<unknown>).length === 0) &&
          (!Array.isArray(first.diagram?.connections) ||
            (first.diagram!.connections as Array<unknown>).length === 0);
        if (isEmptyDiagram) {
          try {
            const snap = { diagram: serializeDiagram(state), ts: Date.now() } as Snapshot;
            undoStack[0] = snap;
          } catch (_e) {
            // ignore
          }
        }
      }
    } catch (_e) {
      // ignore
    }
    window.removeEventListener('diagramLoaded', onLoaded);
  };
  window.addEventListener('diagramLoaded', onLoaded);
}
//=============================================================================
export async function undo(): Promise<boolean> {
  // Flush a pending push so the current state is captured.
  if (pushTimer) {
    clearTimeout(pushTimer as unknown as number);
    pushTimer = 0 as unknown as number;
    pushSnapshot();
  }
  if (undoStack.length <= 1) {
    return false;
  }
  const current = undoStack.pop();
  if (current) redoStack.push(current);
  return applySnapshot(undoStack[undoStack.length - 1]);
}
//=============================================================================
export async function redo(): Promise<boolean> {
  if (pushTimer) {
    clearTimeout(pushTimer as unknown as number);
    pushTimer = 0 as unknown as number;
    pushSnapshot();
  }
  if (!redoStack.length) {
    return false;
  }
  const next = redoStack.pop();
  if (next) undoStack.push(next);
  return applySnapshot(next);
}
//=============================================================================
export function clearHistory(): void {
  undoStack = [];
  redoStack = [];
  applying = false;
  if (pushTimer) {
    clearTimeout(pushTimer as unknown as number);
    pushTimer = 0 as unknown as number;
  }
}
//=============================================================================
export function duplicateSelection(): boolean {
  if (!rendererRef || !rendererRef.current) return false;
  const renderer = rendererRef.current;
  const selIds =
    state.selectedIds && state.selectedIds.size
      ? Array.from(state.selectedIds)
      : state.selectedId
      ? [state.selectedId]
      : [];
  if (!selIds.length) return false;
  const mapping: Record<string, number | string> = {};
  const offset = { x: 20, y: 20 };
  const createdIds: Array<number | string> = [];
  selIds.forEach((id) => {
    const b = state.blocks.get(id as number) as Record<string, unknown> | undefined;
    if (!b) return;
    const params = deepClone((b.params || {}) as unknown) as unknown;
    const newX = (Number(b.x) || 0) + offset.x;
    const newY = (Number(b.y) || 0) + offset.y;
    const newBlock = renderer.createBlock?.(b.type as string, newX, newY, { params });
    if (newBlock) {
      const nb = newBlock as Record<string, unknown>;
      const key = String(id);
      mapping[key] = nb.id as number | string;
      const nbId = nb.id as number | string | undefined;
      if (nbId !== undefined && nbId !== null) createdIds.push(nbId);
    }
  });
  state.connections.forEach((conn) => {
    const c = conn as Record<string, unknown>;
    const fromKey = String(c.from);
    const toKey = String(c.to);
    if (!c || !mapping[fromKey] || !mapping[toKey]) return;
    try {
      const toIndex = Number(c.toIndex ?? 0);
      const fromIndex = Number(c.fromIndex ?? 0);
      renderer.createConnection?.(
        mapping[fromKey] as number,
        mapping[toKey] as number,
        toIndex,
        fromIndex
      );
    } catch (_e) {
      // ignore
    }
  });
  state.selectedId = createdIds.length === 1 ? createdIds[0] : null;
  state.selectedIds = new Set(createdIds);
  if (renderer.updateSelectionBox) renderer.updateSelectionBox();

  try {
    if (createdIds.length > 0) {
      if (typeof renderer.forceFullRoute === 'function') {
        void renderer.forceFullRoute(2000);
      } else if (typeof renderer.updateConnections === 'function') {
        renderer.updateConnections(true);
      }
    }
  } catch (_e) {
    // ignore routing errors
  }
  return true;
}
//=============================================================================
export async function copySelection(): Promise<boolean> {
  if (!rendererRef || !rendererRef.current) return false;
  const selIds =
    state.selectedIds && state.selectedIds.size
      ? Array.from(state.selectedIds)
      : state.selectedId
      ? [state.selectedId]
      : [];
  if (!selIds.length) return false;
  const blocks: Array<Record<string, unknown>> = [];
  selIds.forEach((id) => {
    const b = state.blocks.get(id as number) as Record<string, unknown> | undefined;
    if (!b) return;
    const params = deepClone((b.params || {}) as unknown) as unknown;
    blocks.push({
      id: b.id ?? id,
      type: b.type,
      x: Number(b.x) || 0,
      y: Number(b.y) || 0,
      rotation: b.rotation || 0,
      params,
    });
  });
  const selKeys = new Set(selIds.map((s) => String(s)));
  const connections: Array<Record<string, unknown>> = [];
  (state.connections as Array<Record<string, unknown>>).forEach((c) => {
    if (!c) return;
    const fromKey = String(c.from);
    const toKey = String(c.to);
    if (selKeys.has(fromKey) && selKeys.has(toKey)) {
      connections.push({
        from: c.from,
        to: c.to,
        fromIndex: c.fromIndex ?? 0,
        toIndex: c.toIndex ?? 0,
        points: Array.isArray(c.points) ? c.points : undefined,
      });
    }
  });

  const payload = { __nflow__: true, created: Date.now(), blocks, connections } as Record<
    string,
    unknown
  >;
  const text = JSON.stringify(payload);
  // Always set the C++ host clipboard: other windows may have another port or
  // origin, which blocks navigator.clipboard across windows.
  if (canUseNFlowRpc('nflow.clipboard.set')) {
    nflowRpc('nflow.clipboard.set', { data: text }).catch(() => {});
  }
  try {
    if (
      typeof navigator !== 'undefined' &&
      navigator.clipboard &&
      typeof navigator.clipboard.writeText === 'function'
    ) {
      await navigator.clipboard.writeText(text);
    } else {
      _clipboard = text;
    }
    return true;
  } catch (_e) {
    _clipboard = text;
    return true;
  }
}
//=============================================================================
export async function cutSelection(): Promise<boolean> {
  const copied = await copySelection();
  if (!copied) return false;
  if (!rendererRef || !rendererRef.current) return false;
  const renderer = rendererRef.current;
  const selIds =
    state.selectedIds && state.selectedIds.size
      ? Array.from(state.selectedIds)
      : state.selectedId
      ? [state.selectedId]
      : [];
  if (!selIds.length) return false;
  selIds.forEach((id) => {
    try {
      renderer.deleteBlock?.(String(id));
    } catch (_e) {
      // ignore
    }
  });
  state.selectedId = null;
  state.selectedIds = new Set();
  if (renderer.updateSelectionBox) renderer.updateSelectionBox();
  return true;
}
//=============================================================================
export async function pasteSelection(): Promise<boolean> {
  if (!rendererRef || !rendererRef.current) return false;

  // In a newly opened window blockTemplates may still be empty, and createBlock()
  // would return null for every type. __nf_lib_ready_promise resolves after
  // initLibraries() AND refreshBlockTemplates().
  try {
    const libReady =
      (window as any).__nf_lib_ready_promise ?? (window as any).__nf_init_libraries_promise;
    if (libReady && typeof libReady.then === 'function') await libReady;
  } catch (_e) {
    // proceed; createBlock falls back to refreshBlockTemplates
  }

  if (!rendererRef || !rendererRef.current) return false;
  const renderer = rendererRef.current;
  let text: string | null = null;
  // The host clipboard first: it works across windows and ports and does not
  // depend on browser permission timing.
  if (canUseNFlowRpc('nflow.clipboard.get')) {
    try {
      const hostText = await nflowRpc<string>('nflow.clipboard.get');
      if (hostText && hostText.includes('__nflow__')) {
        text = hostText;
      }
    } catch (_e) {
      // ignore
    }
  }
  // OS clipboard covers content not set via the host RPC clipboard.
  if (!text || !text.includes('__nflow__')) {
    try {
      if (
        typeof navigator !== 'undefined' &&
        navigator.clipboard &&
        typeof navigator.clipboard.readText === 'function'
      ) {
        const osText = await navigator.clipboard.readText();
        if (osText && osText.includes('__nflow__')) {
          text = osText;
        }
      }
    } catch (_e) {
      // ignore clipboard read errors
    }
  }
  if (!text) text = _clipboard;
  if (!text) return false;
  let payload: Record<string, unknown> | null = null;
  try {
    payload = JSON.parse(text) as Record<string, unknown>;
  } catch (_e) {
    return false;
  }
  if (!payload || payload.__nflow__ !== true) return false;
  const blocks = Array.isArray(payload.blocks)
    ? (payload.blocks as Array<Record<string, unknown>>)
    : [];
  const connections = Array.isArray(payload.connections)
    ? (payload.connections as Array<Record<string, unknown>>)
    : [];
  if (!blocks.length) return false;
  // Pasting between windows: enable the discovered libraries that hold block
  // types this window has not loaded yet, so createBlock() can succeed.
  try {
    const neededTypes = Array.from(new Set(blocks.map((b) => String(b.type || ''))));
    const normalize = (s: string) =>
      String(s || '')
        .trim()
        .toLowerCase();
    const missingTypes = neededTypes.filter((t) => {
      try {
        const nt = normalize(t);
        const bt = (window as any).blockTemplates as Record<string, unknown> | undefined;
        if (!bt) return true;
        if (Object.prototype.hasOwnProperty.call(bt, t)) return false;
        // Case-insensitive and namespaced match
        const keys = Object.keys(bt || {});
        for (const k of keys) {
          const nk = normalize(k);
          if (nk === nt) return false;
          const last = nk.split('.').pop() || '';
          if (last === nt) return false;
        }
      } catch (_e) {}
      return true;
    });
    if (missingTypes.length) {
      try {
        const loader = await import('./library-loader.js');
        // Newly generated crates must be visible to this window first (as in
        // initLibraries()).
        try {
          if (canUseNFlowRpc('nflow.library.list')) {
            try {
              const raw = await nflowRpc<unknown>('nflow.library.list');
              if (raw) {
                try {
                  const hostManifests = typeof raw === 'string' ? JSON.parse(raw) : raw;
                  if (Array.isArray(hostManifests)) {
                    const discovered = loader.getDiscoveredManifests();
                    for (const m of hostManifests) {
                      try {
                        const manifest = typeof m === 'string' ? JSON.parse(m) : m;
                        if (manifest && manifest.id) discovered.set(manifest.id, manifest);
                      } catch (_e) {}
                    }
                  }
                } catch (_e) {}
              }
            } catch (_e) {}
          }
        } catch (_e) {}

        const discovered = loader.getDiscoveredManifests();

        const toEnable = new Set<string>();
        for (const [id, manifestRec] of discovered) {
          try {
            const manifest = manifestRec as any;
            if (!manifest || !Array.isArray(manifest.blocks)) continue;
            for (const mb of manifest.blocks) {
              if (!mb || !mb.type) continue;
              if (missingTypes.includes(String(mb.type))) {
                toEnable.add(id);
                break;
              }
            }
          } catch (_e) {}
        }

        // Prefer loadLibrary; fall back to enabling from the manifest.
        try {
          const tried: string[] = [];
          for (const id of Array.from(toEnable)) {
            tried.push(id);
            try {
              if (typeof (loader as any).loadLibrary === 'function') {
                const entry = await (loader as any).loadLibrary(id);
                if (!entry && typeof (loader as any).enableLibraryFromManifest === 'function') {
                  await (loader as any).enableLibraryFromManifest(id);
                }
              } else if (typeof (loader as any).enableLibraryFromManifest === 'function') {
                await (loader as any).enableLibraryFromManifest(id);
              }
            } catch (e) {}
          }
          try {
            if (typeof (loader as any).loadAllDiscovered === 'function') {
              await (loader as any).loadAllDiscovered();
            }
          } catch (_e) {
            // ignore
          }
        } catch (_e) {
          // ignore
        }
        try {
          const renderMod = await import('../render/render.js');
          if (renderMod && typeof renderMod.refreshBlockTemplates === 'function') {
            renderMod.refreshBlockTemplates();
          }
        } catch (_e) {}

        try {
          const TEMPLATE_WAIT_MS = 5000;
          const waitForTemplates = async (types: string[], timeout = TEMPLATE_WAIT_MS) => {
            const start = Date.now();
            while (Date.now() - start < timeout) {
              try {
                const bt = (window as any).blockTemplates || {};
                let allPresent = true;
                for (const t of types) {
                  const nt = normalize(t);
                  const keys = Object.keys(bt || {});
                  const found = keys.some((k) => {
                    const nk = normalize(k);
                    if (nk === nt) return true;
                    const last = nk.split('.').pop() || '';
                    if (last === nt) return true;
                    return false;
                  });
                  if (!found) {
                    allPresent = false;
                    break;
                  }
                }
                if (allPresent) return true;
              } catch (_e) {}
              await new Promise((r) => setTimeout(r, 120));
            }
            return false;
          };
          const ready = await waitForTemplates(missingTypes, TEMPLATE_WAIT_MS);
          if (!ready) {
            try {
              try {
                const loaderMod = await import('./library-loader.js');
                if (typeof loaderMod.getMissingBlockTypes === 'function') {
                  try {
                    // const reported = loaderMod.getMissingBlockTypes(missingTypes as any) || [];
                  } catch (_e2) {}
                }
              } catch (_e) {}
            } catch (_e) {}
          }
        } catch (_e) {}
      } catch (_e) {
        // still attempt to create the blocks
      }
    }
  } catch (_e) {
    // ignore enabling path failures
  }
  // Block creation is disabled in locked diagrams.
  if (state.locked) {
    return false;
  }
  // renderBlockLibrary() only updates window.blockTemplates; createBlock() reads
  // the module-level variable, which only refreshBlockTemplates() updates.
  try {
    const renderMod = await import('../render/render.js');
    if (renderMod && typeof renderMod.refreshBlockTemplates === 'function') {
      renderMod.refreshBlockTemplates();
    }
  } catch (_e) {}
  const mapping: Record<string, number | string> = {};
  // Offset that centres the pasted blocks in the current viewport.
  const offset = { x: 20, y: 20 };
  try {
    let minX = Infinity;
    let minY = Infinity;
    let maxX = -Infinity;
    let maxY = -Infinity;
    for (const pb of blocks) {
      try {
        const bx = Number(pb.x ?? 0);
        const by = Number(pb.y ?? 0);
        const bw = Number(pb.width ?? 0) || 0;
        const bh = Number(pb.height ?? 0) || 0;
        minX = Math.min(minX, bx);
        minY = Math.min(minY, by);
        maxX = Math.max(maxX, bx + bw);
        maxY = Math.max(maxY, by + bh);
      } catch (_e) {
        // ignore per-block faults
      }
    }
    if (isFinite(minX) && isFinite(minY) && isFinite(maxX) && isFinite(maxY)) {
      const origCenterX = minX + (maxX - minX) / 2;
      const origCenterY = minY + (maxY - minY) / 2;
      try {
        const vb = getViewBox();
        const viewCenterX = vb.x + vb.w / 2;
        const viewCenterY = vb.y + vb.h / 2;
        offset.x = viewCenterX - origCenterX;
        offset.y = viewCenterY - origCenterY;
      } catch (_e) {
        // ignore viewbox read errors
      }
    }
  } catch (_e) {
    // ignore bounding computation errors
  }
  const createdIds: Array<number | string> = [];
  for (const b of blocks) {
    try {
      const id = b.id;
      const type = (b.type as string) || 'unknown';
      const params = deepClone((b.params || {}) as unknown) as unknown;
      const newX = (Number(b.x) || 0) + offset.x;
      const newY = (Number(b.y) || 0) + offset.y;
      const newBlock = renderer.createBlock?.(type, newX, newY, { params });
      if (!newBlock) {
        try {
          // Templates for this type may be missing in this window.
        } catch (_e) {}
      }
      if (newBlock) {
        const nb = newBlock as Record<string, unknown>;
        mapping[String(id)] = nb.id as number | string;
        const nbId = nb.id as number | string | undefined;
        if (nbId !== undefined && nbId !== null) createdIds.push(nbId);
      }
    } catch (_e) {
      // ignore per-block failures
    }
  }
  connections.forEach((c) => {
    try {
      const fromKey = String(c.from);
      const toKey = String(c.to);
      const hasFrom = Object.prototype.hasOwnProperty.call(mapping, fromKey);
      const hasTo = Object.prototype.hasOwnProperty.call(mapping, toKey);
      // Both endpoints must have been pasted
      if (hasFrom && hasTo) {
        const toIndex = Number(c.toIndex ?? 0);
        const fromIndex = Number(c.fromIndex ?? 0);
        renderer.createConnection?.(mapping[fromKey], mapping[toKey], toIndex, fromIndex);
      }
    } catch (_e) {
      // ignore
    }
  });
  state.selectedId = createdIds.length === 1 ? createdIds[0] : null;
  state.selectedIds = new Set(createdIds);
  if (renderer.updateSelectionBox) renderer.updateSelectionBox();
  if (createdIds.length === 0 && blocks.length > 0) {
    // Some pasted blocks could not be created in this window.
  }
  return true;
}
//=============================================================================
