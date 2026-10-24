//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { emitAppEvent } from './app-events.js';
//=============================================================================
type Block = unknown;
type Connection = unknown;
type PortRef = unknown;
type PendingAction = unknown;
export interface AppState {
  blocks: Map<number | string, Block>;
  connections: Connection[];
  pendingPort: PortRef | null;
  nextId: number;
  selectedId: number | string | null;
  selectedConnection: number | string | null;
  selectedIds: Set<number | string>;
  selectedConnections: Set<number | string>;
  suppressNextCanvasClick: boolean;
  deleteMode: boolean;
  isPinching: boolean;
  isPanning: boolean;
  isDragging: boolean;
  routingDirty: boolean;
  routingScheduled: boolean;
  fastRouting: boolean;
  deferRouting: boolean;
  deferRoutingIds: Set<number>;
  dirtyBlocks: Set<number>;
  dirtyConnections: Set<number>;
  variables: Record<string, unknown>;
  variablesText: string;
  variablesDisplay: string[];
  diagramName: string;
  modelVersion: string;
  currentUser: string;
  author: string;
  createdBy: string;
  created: string;
  lastSavedBy: string;
  lastSavedAt: string;
  license: string;
  comments: string;
  selectedLoopKey: string | null;
  sampleTime: number;
  realTime?: boolean;
  solver?: string;
  relTol?: number;
  absTol?: number;
  maxStep?: number;
  // StopFcn: Nelson commands run when a simulation stops;
  // executed in the base workspace with the result exposed as 'out'.
  stopFcn?: string;
  autoRoute: boolean;
  locked?: boolean;
  canvasBackground?: string;
  loadingDiagram: boolean;
  loadedSubsystems: Map<string, unknown>;
  subsystemStack: unknown[];
  routeEpoch: number;
  spawnIndex: number;
  simulationActive: boolean;
  dirty: boolean;
  pendingAction: PendingAction | null;
}
//=============================================================================
export const state: AppState = {
  blocks: new Map(),
  connections: [],
  pendingPort: null,
  nextId: 1,
  selectedId: null,
  selectedConnection: null,
  selectedIds: new Set(),
  selectedConnections: new Set(),
  suppressNextCanvasClick: false,
  deleteMode: false,
  isPinching: false,
  isPanning: false,
  isDragging: false,
  routingDirty: false,
  routingScheduled: false,
  fastRouting: false,
  deferRouting: false,
  deferRoutingIds: new Set(),
  dirtyBlocks: new Set(),
  dirtyConnections: new Set(),
  variables: {},
  variablesText: '',
  variablesDisplay: [],
  diagramName: 'nflow',
  modelVersion: '1.0.0',
  currentUser: '',
  author: '',
  createdBy: '',
  created: new Date().toISOString(),
  lastSavedBy: '',
  lastSavedAt: '',
  license: '',
  comments: '',
  selectedLoopKey: null,
  sampleTime: 0.01,
  realTime: undefined,
  solver: 'discrete',
  relTol: 1e-6,
  absTol: 1e-8,
  maxStep: 0,
  autoRoute: true,
  locked: false,
  canvasBackground: '',
  loadingDiagram: false,
  loadedSubsystems: new Map(),
  subsystemStack: [],
  routeEpoch: 0,
  spawnIndex: 0,
  simulationActive: false,
  dirty: false,
  pendingAction: null,
};
//=============================================================================
let fitToDiagram: () => void = () => {};
let updateStabilityPanel: () => void = () => {};
let updateViewBox: (arg?: unknown) => void = () => {};
let updateViewBoxWithAnchor: (arg?: unknown) => void = () => {};
let currentFilePath: string | null = null;
export function markDirty(): void {
  state.dirty = true;
}
//=============================================================================
export function clearDirty(): void {
  state.dirty = false;
}
//=============================================================================
export const signalDiagramChanged = (): void => {
  markDirty();
  try {
    window.dispatchEvent(new Event('diagramChanged'));
  } catch (_e) {
    // ignore in non-browser contexts
  }
};
//=============================================================================
export const deepClone = <T>(value: T): T => JSON.parse(JSON.stringify(value));
export function setFitToDiagram(fn: () => void): void {
  fitToDiagram = fn;
}
//=============================================================================
export function setUpdateStabilityPanel(fn: () => void): void {
  updateStabilityPanel = fn;
}
//=============================================================================
export function setUpdateViewBox(fn: (arg?: unknown) => void): void {
  updateViewBox = fn;
}
//=============================================================================
export function setUpdateViewBoxWithAnchor(fn: (arg?: unknown) => void): void {
  updateViewBoxWithAnchor = fn;
}
//=============================================================================
export function setCurrentFilePath(path: string | null): void {
  currentFilePath = path;
  // Mirror onto a window slot: the bundle ships as two entry copies (a lazy chunk
  // imports the un-hashed entry, the page loads the hashed one), so this module
  // can be evaluated twice. Save must see the path set by an open in the other copy.
  try {
    (window as unknown as { __nfCurrentFilePath?: string | null }).__nfCurrentFilePath = path;
    emitAppEvent('nf-current-file-change', { path });
  } catch (_e) {
    /* ignore in non-browser contexts */
  }
}
//=============================================================================
export function getCurrentFilePath(): string | null {
  try {
    const shared = (window as unknown as { __nfCurrentFilePath?: string | null })
      .__nfCurrentFilePath;
    if (shared !== undefined) return shared;
  } catch (_e) {
    /* ignore in non-browser contexts */
  }
  return currentFilePath;
}
//=============================================================================
export function getFitToDiagram(): () => void {
  return fitToDiagram;
}
//=============================================================================
export function getUpdateStabilityPanel(): () => void {
  return updateStabilityPanel;
}
//=============================================================================
export function getUpdateViewBox(): (arg?: unknown) => void {
  return updateViewBox;
}
//=============================================================================
export function getUpdateViewBoxWithAnchor(): (arg?: unknown) => void {
  return updateViewBoxWithAnchor;
}
//=============================================================================
