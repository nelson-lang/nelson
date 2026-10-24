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
import { simulate, renderScope } from '../sim/sim.js';
import { createSimController, SIM_CONTROL_TIMEOUT_MS } from '../sim/sim-controller.js';
import type { SimPhase } from '../sim/sim-controller.js';
import { t } from '../utils/i18n.js';
import { nflowRpc } from './native-rpc.js';
import { emitAppEvent } from './app-events.js';
import { _setStatus } from './event-context.js';
import type { EventRefs } from './event-context.js';
//=============================================================================
export interface SimulationActions {
  handleRun: () => Promise<void>;
  handleStop: (announce?: boolean) => Promise<void>;
  handleReset: () => Promise<void>;
}
//=============================================================================
// Run / pause / stop / reset buttons and the status they drive.
export function initSimulationControls(refs: EventRefs): SimulationActions {
  const { statusEl, runtimeInput, renderInspector, updateStatusBar } = refs;
  let lastSimulationTime = 0;

  const simController = createSimController({
    run: (signal) =>
      simulate({
        state,
        runtimeInput,
        statusEl,
        signal,
        onStatusUpdate: (status: string, time: number) => {
          lastSimulationTime = Number.isFinite(time) ? time : lastSimulationTime;
          updateStatusBar(status, time);
        },
      }),
    rpc: (method, params) => nflowRpc(method, params, { timeoutMs: SIM_CONTROL_TIMEOUT_MS }),
  });

  const setSimulationActive = (active: boolean) => {
    state.simulationActive = active;
    document.body.classList.toggle('simulation-active', active);
  };

  const setRunButtonsMode = (running: boolean) => {
    const isPaused = !running && simController.phase === 'paused';
    const label = running ? t('Pause') : isPaused ? t('Resume') : t('Run');
    const aria = running ? t('Pause') : isPaused ? t('Resume') : t('Run');
    const title = running ? t('Pause') : isPaused ? t('Resume') : t('Run');
    const pauseIcon = 'M7 5h4v14H7zM13 5h4v14h-4z';
    const playIcon = 'M7 5l12 7-12 7z';
    const iconPath = running ? pauseIcon : playIcon;
    const targets = Array.from(document.querySelectorAll('#playPauseBtn, #statusPlayPauseBtn'));
    targets.forEach((button) => {
      if (!(button instanceof HTMLElement)) return;
      // data-action exposes the running state to tests and other UI code.
      button.setAttribute('data-action', running ? 'pause' : 'run');
      button.setAttribute('aria-label', aria);
      button.setAttribute('title', title);
      const labelEl =
        button.querySelector('.status-btn-label') || button.querySelector('.run-label');
      if (labelEl) labelEl.textContent = label;
      else {
        const svgEl = button.querySelector('svg');
        if (svgEl) {
          const span = document.createElement('span');
          span.className = 'run-label';
          span.textContent = label;
          svgEl.insertAdjacentElement('afterend', span);
        } else {
          button.textContent = label;
        }
      }
      const svgPath = button.querySelector('svg path');
      if (svgPath) svgPath.setAttribute('d', iconPath);
      else {
        const poly = button.querySelector('svg polygon');
        if (poly && iconPath === playIcon) poly.setAttribute('points', '5 3 19 12 5 21 5 3');
      }
    });
    const menuRun = document.getElementById('simulationRunAction');
    if (menuRun) {
      menuRun.dataset.action = 'run';
      menuRun.setAttribute('aria-disabled', running ? 'true' : 'false');
      menuRun.classList.toggle('disabled', running);
      const menuLabel = menuRun.querySelector('span:first-child');
      if (menuLabel) menuLabel.textContent = isPaused ? t('Resume') : t('Run');
    }
    const menuPause = document.getElementById('simulationPauseAction');
    if (menuPause) {
      menuPause.dataset.action = 'pause';
      menuPause.setAttribute('aria-disabled', running ? 'false' : 'true');
      menuPause.classList.toggle('disabled', !running);
    }
    const stoppable = simController.phase === 'running' || simController.phase === 'paused';
    document.querySelectorAll<HTMLElement>('[data-action="stop"]').forEach((button) => {
      if (button instanceof HTMLButtonElement) button.disabled = !stoppable;
      button.setAttribute('aria-disabled', stoppable ? 'false' : 'true');
      button.classList.toggle('disabled', !stoppable);
    });
  };

  const renderSimPhase = (phase: SimPhase) => {
    setSimulationActive(phase !== 'idle');
    setRunButtonsMode(phase === 'running');
  };
  simController.subscribe((phase) => {
    renderSimPhase(phase);
    emitAppEvent('nf-sim-state', { running: phase === 'running', phase });
  });

  const handleRun = async () => {
    const phase = simController.phase;
    if (phase === 'stopping') return;
    if (phase !== 'idle') {
      await simController.togglePause();
      _setStatus(statusEl, phase === 'running' ? t('Pausing...') : t('Resuming...'), 'info');
      return;
    }
    emitAppEvent('nf-sim-start');
    try {
      await simController.start();
    } finally {
      emitAppEvent('nf-sim-stop');
    }
  };

  const handleStop = async (announce = true) => {
    const wasActive = simController.phase !== 'idle';
    if (wasActive) {
      _setStatus(statusEl, t('Stopping...'), 'info');
      await simController.stop();
    }
    renderSimPhase(simController.phase);
    if (announce && wasActive) {
      _setStatus(
        statusEl,
        t('Stopped at {time} s; results preserved', { time: lastSimulationTime.toFixed(2) }),
        'success',
        3000
      );
    }
    emitAppEvent('nf-sim-stop');
  };

  const handleReset = async () => {
    await handleStop(false);
    lastSimulationTime = 0;
    clearSimulationOutputs();
    _setStatus(statusEl, t('Reset'), 'success', 2000);
    try {
      if (typeof updateStatusBar === 'function') updateStatusBar(t('Reset'), 0);
      emitAppEvent('nf-sim-time', { time: 0, progress: 0, done: false });
    } catch (_err) {}
    const selected: any = state.selectedId ? state.blocks.get(state.selectedId) : null;
    if (selected && (selected.type === 'scope' || selected.type === 'xyScope'))
      renderInspector(selected);
  };

  const clearSimulationOutputs = () => {
    const hasIncoming = (blockId: any, inputIndex: number) =>
      state.connections.some(
        (conn: any) => conn.to === blockId && Number(conn.toIndex ?? 0) === Number(inputIndex)
      );
    state.blocks.forEach((block: any) => {
      if (block.type === 'scope') {
        const connected = Array.from({ length: block.inputs }, (_, idx) =>
          hasIncoming(block.id, idx)
        );
        const series = Array.from({ length: block.inputs }, () => []);
        block.scopeData = { time: [], series, connected };
        renderScope(block);
      } else if (block.type === 'xyScope') {
        const connected = [hasIncoming(block.id, 0), hasIncoming(block.id, 1)];
        block.xyScopeData = { series: { x: [], y: [] }, connected };
        renderScope(block);
      } else if (block.type === 'fileSink' && block.params) {
        block.params.lastCsv = '';
      }
    });
  };

  setRunButtonsMode(false);

  document.addEventListener('click', (event) => {
    const target = event.target as Element | null;
    const button = target?.closest?.(
      '[data-action="run"], [data-action="pause"], [data-action="stop"], [data-action="reset"]'
    );
    if (!(button instanceof HTMLElement) || button.getAttribute('aria-disabled') === 'true') return;
    event.preventDefault();
    const action = button.dataset.action;
    if (action === 'stop') void handleStop();
    else if (action === 'reset') void handleReset();
    else void handleRun();
  });

  return { handleRun, handleStop, handleReset };
}
//=============================================================================
