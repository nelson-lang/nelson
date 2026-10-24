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
import { state, signalDiagramChanged } from './state.js';
import { serializeDiagram } from './file-operations.js';
import { getUpdateStatusBar, getSvg } from './workspace-handlers.js';
//=============================================================================
// Single source of truth for viewport measurement (see workspace-handlers).
// Re-exported here so canvas-events and event-handlers keep their import path.
export { getViewportSize } from './workspace-handlers.js';
import { normalizeWorldPosition } from '../utils/geometry.js';
import { pushStatus } from './status-service.js';
import { collectExternalPorts } from '../utils/subsystem-ports.js';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
/** Dual-write: keeps legacy statusEl write for tests AND notifies the service. */
export function _setStatus(
  el: HTMLElement | null,
  msg: string,
  level: 'info' | 'warn' | 'error' | 'success' = 'info',
  durationMs = 3000
): void {
  if (el) el.textContent = msg;
  pushStatus({ level, message: msg, durationMs });
}
//=============================================================================
export const deepClone = <T>(value: T): T => JSON.parse(JSON.stringify(value));
const NON_TEXT_INPUT_TYPES = new Set([
  'button',
  'checkbox',
  'color',
  'file',
  'hidden',
  'image',
  'radio',
  'range',
  'reset',
  'submit',
]);
//=============================================================================
export function isTextSelectionTarget(target: EventTarget | null): boolean {
  if (!(target instanceof Element)) return false;
  if ((target as HTMLElement).isContentEditable || target.closest('[contenteditable="true"]')) {
    return true;
  }
  if (target instanceof HTMLTextAreaElement) return true;
  if (target instanceof HTMLInputElement) {
    return !NON_TEXT_INPUT_TYPES.has(target.type.toLowerCase());
  }
  return Boolean(target.closest('.nf-console-panel'));
}
//=============================================================================
export function shouldUseNativeKeyboardBehavior(
  target: EventTarget | null,
  selectAllShortcut: boolean
): boolean {
  if (isTextSelectionTarget(target)) return true;
  if (selectAllShortcut || !(target instanceof Element)) return false;
  return ['INPUT', 'TEXTAREA', 'SELECT'].includes(target.tagName);
}
//=============================================================================
export function buildCodegenDiagramPayload(): Record<string, unknown> {
  const diagram = serializeDiagram(state);
  diagram.variables = state.variables || {};
  return diagram;
}
//=============================================================================
export const buildSubsystemSpec = (data: any, fallbackName = 'Subsystem') => {
  const blocks = Array.isArray(data?.blocks) ? deepClone(data.blocks) : [];
  const connections = Array.isArray(data?.connections) ? deepClone(data.connections) : [];
  if (!blocks.length) throw new Error('Subsystem YAML has no blocks');
  const name = String(data?.name || fallbackName).trim() || fallbackName;
  const externalInputs = collectExternalPorts(blocks, FieldNames.kLabelSource);
  const externalOutputs = collectExternalPorts(blocks, FieldNames.kLabelSink);
  if (!externalInputs.length && !externalOutputs.length) {
    throw new Error("No external ports found. Mark label blocks with 'Is external port'");
  }
  const spec: { name: string; [key: string]: unknown } = {
    name,
    blocks,
    connections,
    externalInputs,
    externalOutputs,
  };
  // Carry the inner physical wires (acausal / physical-pin nets) so an acausal
  // sub-circuit stored in a reusable subsystem keeps its connections.
  if (Array.isArray(data?.physicalWires) && data.physicalWires.length) {
    spec.physicalWires = deepClone(data.physicalWires);
  }
  // Carry a reusable / masked block's mask flag and parameter set so it
  // round-trips when placed from the loaded-subsystems palette.
  if (data?.masked) spec.masked = true;
  if (data?.maskParameters && typeof data.maskParameters === 'object') {
    spec.maskParameters = deepClone(data.maskParameters);
  }
  return spec;
};
//=============================================================================
export type RendererLike = {
  current?: {
    forceFullRoute?: (ms?: number) => void;
    updateConnections?: (fast?: boolean) => void;
    selectBlock?: (id: any) => void;
    selectConnection?: (id: any) => void;
    deleteBlock?: (id: any) => void;
    deleteConnection?: (id: any) => void;
    updateBlockTransform?: (b: any) => void;
    setMultiSelection?: (ids: any, conns: any) => void;
    createBlock?: (type: string, x: number, y: number, options?: Record<string, unknown>) => any;
    clientToSvg?: (x: number, y: number) => { x: number; y: number };
    startMarqueeSelection?: (ev: PointerEvent) => void;
    updateSelectionBox?: () => void;
    clearPending?: () => void;
  } | null;
} | null;
//=============================================================================
let rendererRef: RendererLike = null;
let statusEl: HTMLElement | null = null;
let runtimeInput: HTMLInputElement | null = null;
let simDt: HTMLInputElement | null = null;
let realTimeModeInput: HTMLSelectElement | null = null;
let simSolverInput: HTMLSelectElement | null = null;
let simRelTolInput: HTMLInputElement | null = null;
let simAbsTolInput: HTMLInputElement | null = null;
let simMaxStepInput: HTMLInputElement | null = null;
let solverToleranceGroup: HTMLElement | null = null;
let simStopFcnInput: HTMLTextAreaElement | null = null;
let autoRouteInput: HTMLInputElement | null = null;
let _variablesInput: HTMLTextAreaElement | null = null;
let _variablesPreview: HTMLElement | null = null;
let _inspectorBody: HTMLElement | null = null;
let _rotateSelectionBtn: HTMLElement | null = null;
let _marginLoopSelect: HTMLSelectElement | null = null;
let _marginOutputText: HTMLElement | null = null;
let _lockedInput: HTMLInputElement | null = null;
let _diagramNameInput: HTMLInputElement | null = null;
let _subsystemUpBtn: HTMLElement | null = null;
let svg: SVGSVGElement | null = null;
let GRID_SIZE = 20;
// Module-scope references to quiet `noUnusedLocals` while keeping API stable
void _variablesInput;
void _variablesPreview;
void _inspectorBody;
void _marginLoopSelect;
void _marginOutputText;
void _diagramNameInput;
void _subsystemUpBtn;
export function setRendererRef(ref: RendererLike) {
  rendererRef = ref;
}
//=============================================================================
export function setStatusEl(el: HTMLElement | null) {
  statusEl = el;
}
//=============================================================================
export function setRuntimeInput(el: HTMLInputElement | null) {
  runtimeInput = el;
}
//=============================================================================
export function setSimDt(el: HTMLInputElement | null) {
  simDt = el;
}
//=============================================================================
export function setRealTimeModeInput(el: HTMLSelectElement | null) {
  realTimeModeInput = el;
}
//=============================================================================
export function setSimSolverInput(el: HTMLSelectElement | null) {
  simSolverInput = el;
}
//=============================================================================
export function setSimToleranceInputs(
  relTol: HTMLInputElement | null,
  absTol: HTMLInputElement | null,
  maxStep: HTMLInputElement | null,
  group: HTMLElement | null
) {
  simRelTolInput = relTol;
  simAbsTolInput = absTol;
  simMaxStepInput = maxStep;
  solverToleranceGroup = group;
}
//=============================================================================
export function setSimStopFcnInput(el: HTMLTextAreaElement | null) {
  simStopFcnInput = el;
}
//=============================================================================
// The relTol/absTol/maxStep fields only apply to the variable-step (CVODES)
// solvers; hide them for the fixed-step / discrete choices.
function isVariableSolver(name: string | undefined): boolean {
  return (
    name === 'ode45' ||
    name === 'variableStiff' ||
    name === 'variableNonstiff' ||
    name === 'cvodesBdf' ||
    name === 'cvodesAdams'
  );
}
//=============================================================================
export function updateSolverToleranceVisibility(solver: string | undefined) {
  if (solverToleranceGroup) {
    solverToleranceGroup.style.display = isVariableSolver(solver) ? '' : 'none';
  }
}
//=============================================================================
export function setAutoRouteInput(el: HTMLInputElement | null) {
  autoRouteInput = el;
}
//=============================================================================
export function setVariablesInput(el: HTMLTextAreaElement | null) {
  _variablesInput = el;
}
//=============================================================================
export function setVariablesPreview(el: HTMLElement | null) {
  _variablesPreview = el;
}
//=============================================================================
export function setInspectorBody(el: HTMLElement | null) {
  _inspectorBody = el;
}
//=============================================================================
export function setRotateSelectionBtn(el: HTMLElement | null) {
  _rotateSelectionBtn = el;
}
//=============================================================================
export function setLockedInput(el: HTMLInputElement | null) {
  _lockedInput = el;
}
//=============================================================================
export function setMarginLoopSelect(el: HTMLSelectElement | null) {
  _marginLoopSelect = el;
}
//=============================================================================
export function setMarginOutputText(el: HTMLElement | null) {
  _marginOutputText = el;
}
//=============================================================================
export function setGridSize(size: number) {
  GRID_SIZE = size;
}
//=============================================================================
export function setDiagramNameInput(el: HTMLInputElement | null) {
  _diagramNameInput = el;
}
//=============================================================================
export function setSvg(el: SVGSVGElement | null) {
  svg = el;
}
//=============================================================================
export function setSubsystemUpBtn(el: HTMLElement | null) {
  _subsystemUpBtn = el;
}
//=============================================================================
let renderInspector: (b: any) => void = (_b: any) => {};
let updateStatusBar = (_s?: any, _t?: any, _z?: any) => {};
export function setRenderInspector(fn: (b: any) => void) {
  renderInspector = fn;
}
//=============================================================================
export function moveSelectedBlocksByDelta(dx: number, dy: number): boolean {
  const selectedIds = state.selectedIds && state.selectedIds.size > 0 ? state.selectedIds : null;
  const ids =
    selectedIds && state.selectedId != null && selectedIds.has(state.selectedId as any)
      ? Array.from(selectedIds)
      : state.selectedId
      ? [state.selectedId]
      : [];
  if (!ids.length) return false;
  ids.forEach((id) => {
    const block: any = state.blocks.get(id as any);
    if (!block) return;
    const pos = normalizeWorldPosition({
      x: Number(block.x ?? 0) + dx,
      y: Number(block.y ?? 0) + dy,
    });
    block.x = pos.x;
    block.y = pos.y;
    rendererRef?.current?.updateBlockTransform?.(block);
  });
  if (state.dirtyBlocks) ids.forEach((id) => state.dirtyBlocks.add(id as any));
  state.fastRouting = false;
  state.routingDirty = true;
  rendererRef?.current?.updateConnections?.(true);
  rendererRef?.current?.updateSelectionBox?.();
  try {
    signalDiagramChanged();
  } catch (e) {}
  return true;
}
//=============================================================================
// UI references for the listener sections, read once the setters have run.
export interface EventRefs {
  rendererRef: RendererLike;
  statusEl: HTMLElement | null;
  runtimeInput: HTMLInputElement | null;
  simDt: HTMLInputElement | null;
  realTimeModeInput: HTMLSelectElement | null;
  simSolverInput: HTMLSelectElement | null;
  simRelTolInput: HTMLInputElement | null;
  simAbsTolInput: HTMLInputElement | null;
  simMaxStepInput: HTMLInputElement | null;
  simStopFcnInput: HTMLTextAreaElement | null;
  autoRouteInput: HTMLInputElement | null;
  _rotateSelectionBtn: HTMLElement | null;
  _lockedInput: HTMLInputElement | null;
  svg: SVGSVGElement | null;
  GRID_SIZE: number;
  renderInspector: (b: any) => void;
  updateStatusBar: (s?: any, t?: any, z?: any) => void;
}
//=============================================================================
export function collectEventRefs(): EventRefs {
  updateStatusBar = getUpdateStatusBar() ?? updateStatusBar;
  svg = svg || getSvg();
  return {
    rendererRef,
    statusEl,
    runtimeInput,
    simDt,
    realTimeModeInput,
    simSolverInput,
    simRelTolInput,
    simAbsTolInput,
    simMaxStepInput,
    simStopFcnInput,
    autoRouteInput,
    _rotateSelectionBtn,
    _lockedInput,
    svg,
    GRID_SIZE,
    renderInspector,
    updateStatusBar,
  };
}
//=============================================================================
