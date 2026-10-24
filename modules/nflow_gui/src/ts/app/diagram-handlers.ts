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
import { state, signalDiagramChanged, getFitToDiagram } from './state.js';
import { writeStatus } from './status-service.js';
import { buildTitleName } from './title-manager.js';
import { parseVariables } from '../utils/expr.js';
import { t } from '../utils/i18n.js';
import { getMissingBlockTypes } from './library-loader.js';
import { showMissingBlocksModal } from './modal-handlers.js';
import { renderSubsystemBreadcrumb } from './components/subsystem-breadcrumb.js';
import type { SubsystemBreadcrumbItem } from './components/subsystem-breadcrumb.js';
import { realTimeToMode } from './real-time-mode.js';
import { emitAppEvent } from './app-events.js';
//=============================================================================
type RendererCurrent = {
  createBlock?: (type: string, x: number, y: number, opts?: Record<string, unknown>) => unknown;
  selectBlock?: (id: unknown) => void;
  deleteBlock?: (id: number) => void;
  clearWorkspace?: () => void;
  createConnection?: (from: unknown, to: unknown, toIndex?: number, fromIndex?: number) => unknown;
  getLastConnectionError?: () => { reason?: string; message?: string } | null;
  renderCurrentWirePaths?: (arg?: boolean) => void;
  forceFullRoute?: (time: number) => void;
  updateConnections?: (arg?: boolean) => void;
  updateBlockLabel?: (block: unknown) => void;
};
//=============================================================================
type RendererRef = { current: RendererCurrent | null } | null;
let statusEl: HTMLElement | null = null;
let diagramNameInput: HTMLInputElement | null = null;
let runtimeInput: HTMLInputElement | null = null;
let simDt: HTMLInputElement | null = null;
let realTimeModeInput: HTMLSelectElement | null = null;
let autoRouteInput: HTMLInputElement | null = null;
let variablesInput: HTMLInputElement | null = null;
let variablesPreview: HTMLElement | null = null;
let lockedInput: HTMLInputElement | null = null;
let rendererRef: RendererRef = null;
export function setStatusEl(el: HTMLElement | null): void {
  statusEl = el;
}
//=============================================================================
export function setDiagramNameInput(el: HTMLInputElement | null): void {
  diagramNameInput = el;
}
//=============================================================================
export function setRuntimeInput(el: HTMLInputElement | null): void {
  runtimeInput = el;
}
//=============================================================================
export function setSimDt(el: HTMLInputElement | null): void {
  simDt = el;
}
//=============================================================================
export function setRealTimeModeInput(el: HTMLSelectElement | null): void {
  realTimeModeInput = el;
}
//=============================================================================
export function setAutoRouteInput(el: HTMLInputElement | null): void {
  autoRouteInput = el;
}
//=============================================================================
export function setVariablesInput(el: HTMLInputElement | null): void {
  variablesInput = el;
}
//=============================================================================
export function setVariablesPreview(el: HTMLElement | null): void {
  variablesPreview = el;
}
//=============================================================================
export function setLockedInput(el: HTMLInputElement | null): void {
  lockedInput = el;
}
//=============================================================================
export function syncLockedUi(): void {
  try {
    const el =
      lockedInput ||
      (typeof document !== 'undefined'
        ? (document.getElementById('lockedInput') as HTMLInputElement | null)
        : null);
    if (el) el.checked = Boolean(state.locked);
    document.body.classList.toggle('diagram-locked', Boolean(state.locked));
  } catch (_e) {
    // ignore
  }
}
//=============================================================================
export function setRendererRef(ref: RendererRef): void {
  rendererRef = ref;
}
//=============================================================================
export function getRendererRef(): RendererRef {
  return rendererRef;
}
//=============================================================================
export function getStatusEl(): HTMLElement | null {
  return statusEl;
}
//=============================================================================
export function getRuntimeInput(): HTMLInputElement | null {
  return runtimeInput;
}
//=============================================================================
export const connectionKey = (
  conn:
    | Record<string, unknown>
    | { from: unknown; fromIndex?: unknown; to: unknown; toIndex?: unknown }
) => {
  const c = conn as unknown as Record<string, unknown>;
  return `${String(c.from)}:${Number(c.fromIndex ?? 0)}->${String(c.to)}:${Number(c.toIndex ?? 0)}`;
};
//=============================================================================
// Refit once the canvas has settled: opening a model can run while the canvas is still
// resizing (e.g. the Example Browser modal unmounting), and a fit then lands the model
// off-frame. Waits for a real size that is stable across two frames, bounded by
// maxAttempts. Exported for unit testing.
export function runDeferredFit(
  getRect: () => { width: number; height: number } | null,
  doFit: () => void,
  scheduleFrame: (cb: () => void) => void,
  maxAttempts = 10
): void {
  let attempts = 0;
  let lastW = -1;
  let lastH = -1;
  const tick = (): void => {
    attempts += 1;
    const rect = getRect();
    const sized = !rect || (rect.width > 1 && rect.height > 1);
    const stable = !rect || (rect.width === lastW && rect.height === lastH);
    if ((sized && stable) || attempts >= maxAttempts) {
      doFit();
      return;
    }
    if (rect) {
      lastW = rect.width;
      lastH = rect.height;
    }
    scheduleFrame(tick);
  };
  scheduleFrame(tick);
}
//=============================================================================
function scheduleDeferredFit(): void {
  if (typeof requestAnimationFrame !== 'function') return;
  runDeferredFit(
    () => {
      const canvas = typeof document !== 'undefined' ? document.getElementById('canvas') : null;
      return canvas ? canvas.getBoundingClientRect() : null;
    },
    () => {
      const fit = getFitToDiagram();
      if (typeof fit === 'function') fit();
    },
    (cb) => requestAnimationFrame(cb)
  );
}
//=============================================================================
export async function loadDiagram(
  data: unknown,
  options: Record<string, unknown> = {}
): Promise<void> {
  state.routeEpoch = (Number(state.routeEpoch) || 0) + 1;
  const preserveSubsystemStack = Boolean(options && options.preserveSubsystemStack);

  if (!data || typeof data !== 'object') throw new Error('Invalid diagram file');
  const D = data as Record<string, unknown>;
  const blocks: Array<unknown> = Array.isArray(D.blocks) ? (D.blocks as Array<unknown>) : [];
  const connections: Array<unknown> = Array.isArray(D.connections)
    ? (D.connections as Array<unknown>)
    : [];
  state.diagramName =
    typeof D.name === 'string' && (D.name as string).trim() ? (D.name as string).trim() : 'nflow';
  const diagramNameInputEl =
    diagramNameInput ||
    (typeof document !== 'undefined'
      ? (document.getElementById('diagramName') as HTMLInputElement | null)
      : null);
  if (diagramNameInputEl) diagramNameInputEl.value = state.diagramName;
  try {
    document.title = buildTitleName(state.diagramName);
  } catch (_e) {}
  const runtimeInputEl =
    runtimeInput ||
    (typeof document !== 'undefined'
      ? (document.getElementById('runtimeInput') as HTMLInputElement | null)
      : null);
  if (runtimeInputEl && Number.isFinite(Number(D.runtime))) {
    runtimeInputEl.value = String(Number(D.runtime));
  }
  const simDtEl =
    simDt ||
    (typeof document !== 'undefined'
      ? (document.getElementById('simDt') as HTMLInputElement | null)
      : null);
  if (simDtEl && Number.isFinite(Number(D.sampleTime))) {
    simDtEl.value = String(Number(D.sampleTime));
    const value = Number(simDtEl.value);
    state.sampleTime = Number.isFinite(value) && value > 0 ? value : 0.01;
  }
  state.realTime =
    typeof (D as Record<string, unknown>).realTime === 'boolean'
      ? ((D as Record<string, unknown>).realTime as boolean)
      : undefined;
  const realTimeModeInputEl =
    realTimeModeInput ||
    (typeof document !== 'undefined'
      ? (document.getElementById('realTimeMode') as HTMLSelectElement | null)
      : null);
  if (realTimeModeInputEl) realTimeModeInputEl.value = realTimeToMode(state.realTime);
  {
    const rec = D as Record<string, unknown>;
    state.solver = typeof rec.solver === 'string' ? (rec.solver as string) : 'discrete';
    state.relTol = typeof rec.relTol === 'number' ? (rec.relTol as number) : 1e-6;
    state.absTol = typeof rec.absTol === 'number' ? (rec.absTol as number) : 1e-8;
    state.maxStep = typeof rec.maxStep === 'number' ? (rec.maxStep as number) : 0;
    // StopFcn: Nelson commands executed when the run stops.
    state.stopFcn = typeof rec.stopFcn === 'string' ? (rec.stopFcn as string) : '';
    if (typeof document !== 'undefined') {
      const simSolverEl = document.getElementById('simSolver') as HTMLSelectElement | null;
      if (simSolverEl) simSolverEl.value = state.solver ?? 'discrete';
      const relEl = document.getElementById('simRelTol') as HTMLInputElement | null;
      if (relEl && typeof state.relTol === 'number') relEl.value = String(state.relTol);
      const absEl = document.getElementById('simAbsTol') as HTMLInputElement | null;
      if (absEl && typeof state.absTol === 'number') absEl.value = String(state.absTol);
      const maxEl = document.getElementById('simMaxStep') as HTMLInputElement | null;
      if (maxEl && typeof state.maxStep === 'number') maxEl.value = String(state.maxStep);
      const stopFcnEl = document.getElementById('simStopFcn') as HTMLTextAreaElement | null;
      if (stopFcnEl) stopFcnEl.value = state.stopFcn ?? '';
      const tolGroup = document.getElementById('solverToleranceGroup');
      if (tolGroup) {
        const s = state.solver;
        const isVariable =
          s === 'ode45' ||
          s === 'variableStiff' ||
          s === 'variableNonstiff' ||
          s === 'cvodesBdf' ||
          s === 'cvodesAdams';
        tolGroup.style.display = isVariable ? '' : 'none';
      }
    }
  }
  state.autoRoute = (D.autoRoute as unknown as boolean) !== false;
  const autoRouteInputEl =
    autoRouteInput ||
    (typeof document !== 'undefined'
      ? (document.getElementById('autoRouteInput') as HTMLInputElement | null)
      : null);
  if (autoRouteInputEl) autoRouteInputEl.checked = state.autoRoute;
  // locked diagram flag - accept boolean or string 'true'/'false'
  const rawLocked = (D as Record<string, unknown>).locked;
  state.locked = rawLocked === true || String(rawLocked) === 'true';
  const lockedInputEl =
    lockedInput ||
    (typeof document !== 'undefined'
      ? (document.getElementById('lockedInput') as HTMLInputElement | null)
      : null);
  if (lockedInputEl) lockedInputEl.checked = Boolean(state.locked);
  try {
    document.body.classList.toggle('diagram-locked', Boolean(state.locked));
  } catch (_e) {}
  state.variablesText = typeof D.variables === 'string' ? (D.variables as string) : '';
  const variablesInputEl =
    variablesInput ||
    (typeof document !== 'undefined'
      ? (document.getElementById('variablesInput') as HTMLInputElement | null)
      : null);
  if (variablesInputEl) {
    variablesInputEl.value = state.variablesText;
  }
  const metadata = D.metadata as Record<string, unknown> | undefined;
  state.modelVersion = typeof D.version === 'string' ? (D.version as string) : '1.0.0';
  state.author =
    metadata && typeof metadata.author === 'string'
      ? (metadata.author as string)
      : typeof D.author === 'string'
      ? (D.author as string)
      : '';
  const authorEl =
    typeof document !== 'undefined'
      ? (document.getElementById('authorInput') as HTMLInputElement | null)
      : null;
  if (authorEl) authorEl.value = state.author || '';

  state.createdBy =
    metadata && typeof metadata.createdBy === 'string'
      ? (metadata.createdBy as string)
      : state.author;
  state.created =
    metadata && typeof metadata.created === 'string'
      ? (metadata.created as string)
      : typeof D.created === 'string'
      ? (D.created as string)
      : '';
  const createdEl =
    typeof document !== 'undefined'
      ? (document.getElementById('createdInput') as HTMLInputElement | null)
      : null;
  if (createdEl) createdEl.value = state.created || '';
  state.lastSavedBy =
    metadata && typeof metadata.lastSavedBy === 'string' ? (metadata.lastSavedBy as string) : '';
  state.lastSavedAt =
    metadata && typeof metadata.lastSavedAt === 'string' ? (metadata.lastSavedAt as string) : '';
  emitAppEvent('nf-model-metadata-change');
  state.license =
    typeof D.license === 'string'
      ? (D.license as string)
      : metadata && typeof metadata.license === 'string'
      ? (metadata.license as string)
      : '';
  const licenseEl =
    typeof document !== 'undefined'
      ? (document.getElementById('licenseInput') as HTMLInputElement | null)
      : null;
  if (licenseEl) licenseEl.value = state.license || '';
  state.comments = typeof D.comments === 'string' ? (D.comments as string) : '';
  const commentsEl =
    typeof document !== 'undefined'
      ? (document.getElementById('commentsInput') as HTMLInputElement | null)
      : null;
  if (commentsEl) commentsEl.value = state.comments || '';
  try {
    const cb = (D as Record<string, unknown>).canvasBackground as string | undefined;
    if (typeof cb === 'string' && cb.trim()) {
      state.canvasBackground = cb.trim();
      try {
        document.documentElement.style.setProperty('--canvas-background', cb.trim());
      } catch (e) {}
    } else {
      state.canvasBackground = '';
    }
  } catch (e) {}
  const parsed = parseVariables(state.variablesText);
  state.variables = parsed.vars;
  state.variablesDisplay = parsed.display;
  const variablesPreviewEl =
    variablesPreview ||
    (typeof document !== 'undefined'
      ? (document.getElementById('variablesPreview') as HTMLElement | null)
      : null);
  if (variablesPreviewEl) {
    const entries = state.variablesDisplay.join('\n');
    variablesPreviewEl.textContent = entries || t('No variables defined.');
  }
  if (!preserveSubsystemStack) {
    state.subsystemStack = [];
    updateSubsystemNavUi();
  }
  if (rendererRef?.current) {
    rendererRef.current.clearWorkspace?.();
  }
  state.spawnIndex = 0;
  state.loadingDiagram = true;
  state.routingDirty = false;
  state.dirtyBlocks.clear();
  state.dirtyConnections.clear();

  const pointQueuesByKey = new Map<string, unknown[]>();
  let hasConnectionsWithoutPoints = false;
  const enqueuePoints = (key: string, points: unknown[]) => {
    if (!Array.isArray(points) || points.length < 2) return;
    const queue = pointQueuesByKey.get(key) || [];
    queue.push(points);
    pointQueuesByKey.set(key, queue);
  };
  const parsePoints = (rawPoints: unknown) => {
    let pointsValue: unknown = rawPoints;
    if (typeof pointsValue === 'string') {
      const trimmed = (pointsValue as string).trim();
      if (trimmed.startsWith('[') && trimmed.endsWith(']')) {
        try {
          pointsValue = JSON.parse(trimmed as string);
        } catch {
          pointsValue = rawPoints;
        }
      }
    }
    if (!Array.isArray(pointsValue)) return [];
    const parsed = (pointsValue as Array<unknown>)
      .map((pt: unknown) => {
        if (Array.isArray(pt) && pt.length >= 2) {
          const arr = pt as Array<unknown>;
          const node: { x: number; y: number; pin?: boolean } = {
            x: Number(arr[0]),
            y: Number(arr[1]),
          };
          // Optional third element `1` marks a pinned (user-anchored) point.
          if (Number(arr[2]) === 1) node.pin = true;
          return node;
        }
        const p = pt as unknown as Record<string, unknown>;
        const node: { x: number; y: number; pin?: boolean } = {
          x: Number(p.x),
          y: Number(p.y),
        };
        if (p.pin) node.pin = true;
        return node;
      })
      .filter(
        (pt: unknown) =>
          Number.isFinite((pt as unknown as Record<string, number>).x) &&
          Number.isFinite((pt as unknown as Record<string, number>).y)
      );
    return parsed.length >= 2 ? parsed : [];
  };
  if (Array.isArray(D.routing)) {
    (D.routing as Array<unknown>).forEach((entryUnk: unknown) => {
      if (!entryUnk || typeof entryUnk !== 'object') return;
      const ent = entryUnk as unknown as Record<string, unknown>;
      const key = `${String(ent.from)}:${Number(ent.fromIndex ?? 0)}->${String(ent.to)}:${Number(
        ent.toIndex ?? 0
      )}`;
      const points = parsePoints(ent.points);
      if (points.length >= 2) enqueuePoints(key, points);
    });
  }
  (connections as Array<unknown>).forEach((entryUnk: unknown) => {
    if (!entryUnk || typeof entryUnk !== 'object') return;
    const ent = entryUnk as unknown as Record<string, unknown>;
    const key = `${String(ent.from)}:${Number(ent.fromIndex ?? 0)}->${String(ent.to)}:${Number(
      ent.toIndex ?? 0
    )}`;
    const points = parsePoints(ent.points);
    if (points.length >= 2) enqueuePoints(key, points);
    else hasConnectionsWithoutPoints = true;
  });
  const takePointsForKey = (key: string) => {
    const queue = pointQueuesByKey.get(key);
    if (!queue || !queue.length) return null;
    const points = queue.shift();
    if (!queue.length) pointQueuesByKey.delete(key);
    return points;
  };

  let loadedPointCount = 0;

  state.blocks.clear();
  state.connections = [];

  const idMap = new Map<number | string, number | string>();

  const neededTypes = (blocks as Array<unknown>)
    .map((b: unknown) => (b as unknown as Record<string, unknown>)?.type)
    .filter(Boolean)
    .map(String);
  const missing = getMissingBlockTypes(neededTypes as string[]);
  if (missing.length > 0) {
    showMissingBlocksModal(missing);
    return;
  }

  // Temporarily clear `state.locked` while creating blocks/connections from
  // the loaded data so the renderer's createBlock/createConnection helpers
  // (which respect `state.locked`) can instantiate elements. The original
  // locked state is restored after creation and the UI updated.
  const _prevLocked = Boolean(state.locked);
  state.locked = false;

  (blocks as unknown as Array<unknown>).forEach((blockDataUnk: unknown) => {
    const blockData = blockDataUnk as unknown as Record<string, unknown>;
    if (!blockData || !blockData.type) return;
    const newBlock = rendererRef?.current?.createBlock?.(
      String(blockData.type),
      Number(blockData.x) || 0,
      Number(blockData.y) || 0,
      {
        id: blockData.id,
        rotation: Number(blockData.rotation) || 0,
        params: blockData.params || {},
        inputs: Number(blockData.inputs) || 0,
        outputs: Number(blockData.outputs) || 0,
        ports: Array.isArray((blockData as Record<string, unknown>).ports)
          ? (blockData as Record<string, unknown>).ports
          : undefined,
      }
    );
    // Map original id to created id (renderer might choose a different id)
    const origId = blockData.id as number | string;
    if (state.blocks.has(origId as number)) {
      idMap.set(origId, origId);
    } else if (newBlock && (newBlock as Record<string, unknown>).id !== undefined) {
      idMap.set(origId, (newBlock as Record<string, unknown>).id as number | string);
    }
    const created = state.blocks.get((idMap.get(origId) ?? origId) as unknown as number);
    if (created && rendererRef?.current?.updateBlockLabel) {
      rendererRef.current.updateBlockLabel(created as unknown as number);
    }
  });

  (connections as unknown as Array<unknown>).forEach((connDataUnk: unknown) => {
    const connData = connDataUnk as unknown as Record<string, unknown>;
    if (!connData) return;
    const fromId = (
      idMap.has(connData.from as number | string)
        ? idMap.get(connData.from as number | string)
        : connData.from
    ) as number | string;
    const toId = (
      idMap.has(connData.to as number | string)
        ? idMap.get(connData.to as number | string)
        : connData.to
    ) as number | string;
    if (
      !state.blocks.has(fromId as unknown as number) ||
      !state.blocks.has(toId as unknown as number)
    )
      return;
    const beforeLen = state.connections.length;
    const createdConn = rendererRef?.current?.createConnection?.(
      fromId as unknown as number,
      toId as unknown as number,
      Number(connData.toIndex ?? 0),
      Number(connData.fromIndex ?? 0)
    );
    if (!createdConn) {
      const createdError =
        typeof rendererRef?.current?.getLastConnectionError === 'function'
          ? rendererRef?.current?.getLastConnectionError?.()
          : null;
      if (createdError?.reason === 'input_occupied') {
        throw new Error(
          `Invalid diagram: multiple outputs connected to ${connData.to}.in${Number(
            connData.toIndex ?? 0
          )}.`
        );
      }
      if (createdError?.reason === 'duplicate') return;
      throw new Error(createdError?.message || 'Invalid connection in loaded diagram.');
    }
    if (state.connections.length <= beforeLen) return;
    const created = state.connections[state.connections.length - 1] as unknown as Record<
      string,
      unknown
    >;
    const key = connectionKey(created);
    const points = takePointsForKey(key);
    if (Array.isArray(points) && points.length >= 2) {
      created.points = (points as Array<unknown>).map((ptUnk: unknown) => {
        const p = ptUnk as unknown as Record<string, unknown>;
        return { x: Number(p.x), y: Number(p.y) };
      });
      loadedPointCount += 1;
    }
    if (typeof connData.label === 'string' && connData.label.trim() !== '') {
      created.label = connData.label;
    }
  });

  // Acausal diagrams persist undirected `physicalWires` nets instead of directed
  // connections. Rebuild drawable wires from them: each N-ary net becomes a spanning
  // chain of pin-to-pin edges (union-find re-lifts the identical net on save). Pin
  // keys are "<blockId>.<pinName>"; the pin index comes from the created block's
  // 'phys' ports.
  const physWires = Array.isArray((D as Record<string, unknown>).physicalWires)
    ? ((D as Record<string, unknown>).physicalWires as Array<{ pins?: unknown }>)
    : [];
  if (physWires.length > 0) {
    const splitKey = (key: string): { blockId: string; pinName: string } => {
      const i = key.lastIndexOf('.');
      return i < 0
        ? { blockId: key, pinName: '' }
        : { blockId: key.slice(0, i), pinName: key.slice(i + 1) };
    };
    const physIndexOf = (blockId: number | string, pinName: string): number | null => {
      const blk = state.blocks.get(blockId as unknown as number) as
        | { ports?: Array<{ type?: string; name?: string; index?: number }> }
        | undefined;
      const ports = blk?.ports ?? [];
      const port = ports.find((p) => p.type === 'phys' && p.name === pinName);
      return port && typeof port.index === 'number' ? port.index : null;
    };
    for (const net of physWires) {
      const pins = Array.isArray(net.pins) ? (net.pins as unknown[]).map(String) : [];
      for (let i = 1; i < pins.length; i += 1) {
        const a = splitKey(pins[i - 1]);
        const b = splitKey(pins[i]);
        const aId = (idMap.has(a.blockId) ? idMap.get(a.blockId) : a.blockId) as number | string;
        const bId = (idMap.has(b.blockId) ? idMap.get(b.blockId) : b.blockId) as number | string;
        if (
          !state.blocks.has(aId as unknown as number) ||
          !state.blocks.has(bId as unknown as number)
        )
          continue;
        const aIdx = physIndexOf(aId, a.pinName);
        const bIdx = physIndexOf(bId, b.pinName);
        if (aIdx === null || bIdx === null) continue;
        rendererRef?.current?.createConnection?.(
          aId as unknown as number,
          bId as unknown as number,
          bIdx,
          aIdx
        );
      }
    }
  }

  state.locked = _prevLocked;
  try {
    const lockedInputEl2 =
      lockedInput ||
      (typeof document !== 'undefined'
        ? (document.getElementById('lockedInput') as HTMLInputElement | null)
        : null);
    if (lockedInputEl2) lockedInputEl2.checked = Boolean(state.locked);
    try {
      document.body.classList.toggle('diagram-locked', Boolean(state.locked));
    } catch (_e) {
      // ignore
    }
  } catch (_e) {
    // ignore
  }

  state.loadingDiagram = false;
  state.routingDirty = true;

  // Frame the loaded model: once now, then again after the canvas settles. When the
  // open comes from the Example Browser the modal may still be unmounting, so a
  // single synchronous fit can read a stale viewport and land the blocks off-screen.
  // fitToDiagram() is a no-op for an empty diagram.
  const fitToDiagram = getFitToDiagram();
  if (typeof fitToDiagram === 'function') {
    fitToDiagram();
    scheduleDeferredFit();
  }

  if (loadedPointCount > 0) {
    state.fastRouting = false;
    state.routingDirty = false;
    if (state.dirtyBlocks) state.dirtyBlocks.clear();
    if (state.dirtyConnections) state.dirtyConnections.clear();
    if (rendererRef?.current?.renderCurrentWirePaths) {
      rendererRef.current.renderCurrentWirePaths(true);
    }
    const shouldAutoRoute = state.autoRoute || hasConnectionsWithoutPoints;
    if (shouldAutoRoute) {
      requestAnimationFrame(() => {
        if (rendererRef?.current?.renderCurrentWirePaths) {
          rendererRef.current.renderCurrentWirePaths(true);
        }
        requestAnimationFrame(() => {
          setTimeout(() => {
            if (rendererRef?.current?.forceFullRoute) {
              rendererRef.current.forceFullRoute(10000);
            }
          }, 0);
        });
      });
    }
  } else if (state.autoRoute) {
    state.fastRouting = true;
    state.routingDirty = true;
    state.dirtyConnections = new Set(state.connections as unknown as number[]);
    if (rendererRef?.current?.updateConnections) {
      rendererRef.current.updateConnections(true);
    }
    requestAnimationFrame(() => {
      setTimeout(() => {
        state.fastRouting = false;
        if (rendererRef?.current?.forceFullRoute) {
          rendererRef.current.forceFullRoute(10000);
        }
      }, 0);
    });
  } else if (rendererRef?.current?.updateConnections) {
    rendererRef.current.updateConnections(true);
  }

  signalDiagramChanged();
  try {
    const ev = new CustomEvent('diagramLoaded');
    window.dispatchEvent(ev);
  } catch (_e) {
    // ignore if CustomEvent not available in this environment
  }
  writeStatus(statusEl, t('Loaded diagram: {name}', { name: state.diagramName }));
}
//=============================================================================
let subsystemUpBtn: HTMLElement | null = null;
export function setSubsystemUpBtn(el: HTMLElement | null): void {
  subsystemUpBtn = el;
}
//=============================================================================
export function updateSubsystemNavUi(): void {
  const isRoot = state.subsystemStack.length === 0;
  // Legacy up-button support (removed from the default layout; the
  // breadcrumb below covers navigation).
  if (subsystemUpBtn) {
    subsystemUpBtn.hidden = isRoot;
    subsystemUpBtn.setAttribute('aria-hidden', String(isRoot));
  }
  if (typeof document !== 'undefined') document.body.classList.toggle('is-root-diagram', isRoot);
  updateSubsystemBreadcrumb(isRoot);
}
//=============================================================================
/** Clickable path (root > sub > ...) while inside subsystems. */
function updateSubsystemBreadcrumb(isRoot: boolean): void {
  if (typeof document === 'undefined') return;
  const nav = document.querySelector('.subsystem-nav');
  if (!nav) return;

  const items: SubsystemBreadcrumbItem[] = state.subsystemStack.map((entry, index) => ({
    name: String(
      ((entry as Record<string, unknown>)?.parentDiagram as Record<string, unknown> | undefined)
        ?.name || 'Diagram'
    ),
    levelsUp: state.subsystemStack.length - index,
  }));

  renderSubsystemBreadcrumb(
    nav,
    isRoot,
    items,
    String(state.diagramName || 'Subsystem'),
    (levelsUp) => {
      void (async () => {
        try {
          const m = await import('./subsystem-handlers.js');
          for (let k = 0; k < levelsUp; k++) {
            await m.closeSubsystemView();
          }
        } catch (_e) {
          /* ignore navigation errors */
        }
      })();
    }
  );
}
//=============================================================================
export {};
//=============================================================================
