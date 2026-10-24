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
import { state } from './state.js';
import { renderScope } from '../sim/sim.js';
import { t } from '../utils/i18n.js';
import { modeToRealTime, realTimeToMode } from './real-time-mode.js';
import { _setStatus, updateSolverToleranceVisibility } from './event-context.js';
import type { EventRefs } from './event-context.js';
//=============================================================================
// Simulation settings fields (step, stop time, solver, tolerances, ...).
export function initSimulationSettingsInputs(refs: EventRefs) {
  const {
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
    _lockedInput,
    renderInspector,
  } = refs;
  const refreshSelectedScopeInspector = () => {
    const selected: any = state.selectedId ? state.blocks.get(state.selectedId) : null;
    if (!selected || (selected.type !== 'scope' && selected.type !== 'xyScope')) return;
    renderScope(selected);
    renderInspector(selected);
  };

  if (simDt) {
    const simDtEl = simDt as HTMLInputElement;
    const updateSimDt = () => {
      const value = Number(simDtEl.value);
      state.sampleTime = Number.isFinite(value) && value > 0 ? value : 0.01;
      refreshSelectedScopeInspector();
    };
    updateSimDt();
    simDtEl.addEventListener('input', updateSimDt);
    simDtEl.addEventListener('change', updateSimDt);
  }

  if (runtimeInput) {
    const updateRuntimeInspector = () => {
      refreshSelectedScopeInspector();
    };
    runtimeInput.addEventListener('input', updateRuntimeInspector);
    runtimeInput.addEventListener('change', updateRuntimeInspector);
  }

  if (realTimeModeInput) {
    const realTimeModeEl = realTimeModeInput as HTMLSelectElement;
    realTimeModeEl.value = realTimeToMode(state.realTime);
    const updateRealTimeMode = () => {
      state.realTime = modeToRealTime(realTimeModeEl.value);
    };
    realTimeModeEl.addEventListener('change', updateRealTimeMode);
  }

  if (simSolverInput) {
    const simSolverEl = simSolverInput as HTMLSelectElement;
    simSolverEl.value = state.solver ?? 'discrete';
    const updateSolver = () => {
      state.solver = simSolverEl.value;
      updateSolverToleranceVisibility(state.solver);
    };
    simSolverEl.addEventListener('change', updateSolver);
    updateSolverToleranceVisibility(state.solver ?? 'discrete');
  }

  if (simRelTolInput) {
    const el = simRelTolInput as HTMLInputElement;
    if (typeof state.relTol === 'number') el.value = String(state.relTol);
    el.addEventListener('change', () => {
      const v = Number(el.value);
      state.relTol = Number.isFinite(v) && v > 0 ? v : undefined;
    });
  }
  if (simAbsTolInput) {
    const el = simAbsTolInput as HTMLInputElement;
    if (typeof state.absTol === 'number') el.value = String(state.absTol);
    el.addEventListener('change', () => {
      const v = Number(el.value);
      state.absTol = Number.isFinite(v) && v > 0 ? v : undefined;
    });
  }
  if (simMaxStepInput) {
    const el = simMaxStepInput as HTMLInputElement;
    if (typeof state.maxStep === 'number') el.value = String(state.maxStep);
    el.addEventListener('change', () => {
      const v = Number(el.value);
      state.maxStep = Number.isFinite(v) && v > 0 ? v : undefined;
    });
  }
  if (simStopFcnInput) {
    const el = simStopFcnInput as HTMLTextAreaElement;
    if (typeof state.stopFcn === 'string') el.value = state.stopFcn;
    // StopFcn is Nelson code run when the run stops; keep state live so Run
    // picks up an edit without needing to blur the field first.
    el.addEventListener('input', () => {
      state.stopFcn = el.value;
    });
  }

  if (autoRouteInput) {
    const autoRouteEl = autoRouteInput as HTMLInputElement;
    autoRouteEl.checked = state.autoRoute !== false;
    const updateAutoRoute = () => {
      state.autoRoute = autoRouteEl.checked;
      if (state.autoRoute) {
        state.fastRouting = false;
        state.routingDirty = true;
        rendererRef?.current?.forceFullRoute?.(500);
      } else {
        rendererRef?.current?.updateConnections?.(true);
      }
    };
    autoRouteEl.addEventListener('change', updateAutoRoute);
  }

  if (_lockedInput) {
    const lockedEl = _lockedInput as HTMLInputElement;
    lockedEl.checked = Boolean(state.locked);
    const updateLocked = () => {
      state.locked = Boolean(lockedEl.checked);
      try {
        document.body.classList.toggle('diagram-locked', Boolean(state.locked));
      } catch (_e) {}
      try {
        _setStatus(
          statusEl,
          state.locked ? t('Diagram locked') : t('Diagram unlocked'),
          'info',
          2000
        );
      } catch (_e) {}
    };
    lockedEl.addEventListener('change', updateLocked);
  }
}
//=============================================================================
