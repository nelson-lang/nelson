//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Wires the editor's DOM listeners. The state, setters and helpers live in
// event-context.ts; each group of listeners has its own module.
import { collectEventRefs } from './event-context.js';
import { initSimulationControls } from './sim-controls.js';
import { initSimulationSettingsInputs } from './sim-settings-inputs.js';
import { initFileActions } from './file-actions.js';
import { initKeyboardShortcuts } from './keyboard-shortcuts.js';
import { initCanvasEvents } from './canvas-events.js';
//=============================================================================
export {
  buildCodegenDiagramPayload,
  getViewportSize,
  isTextSelectionTarget,
  moveSelectedBlocksByDelta,
  setAutoRouteInput,
  setDiagramNameInput,
  setGridSize,
  setInspectorBody,
  setLockedInput,
  setMarginLoopSelect,
  setMarginOutputText,
  setRealTimeModeInput,
  setRenderInspector,
  setRendererRef,
  setRotateSelectionBtn,
  setRuntimeInput,
  setSimDt,
  setSimSolverInput,
  setSimStopFcnInput,
  setSimToleranceInputs,
  setStatusEl,
  setSubsystemUpBtn,
  setSvg,
  setVariablesInput,
  setVariablesPreview,
  shouldUseNativeKeyboardBehavior,
  updateSolverToleranceVisibility,
} from './event-context.js';
//=============================================================================
export function initEventListeners() {
  const refs = collectEventRefs();
  import('./modelica-settings.js').then((m) => m.initModelicaSettings()).catch(() => {});

  const sim = initSimulationControls(refs);
  initSimulationSettingsInputs(refs);
  initFileActions(refs);
  initKeyboardShortcuts(refs, sim);
  initCanvasEvents(refs);
}
//=============================================================================
