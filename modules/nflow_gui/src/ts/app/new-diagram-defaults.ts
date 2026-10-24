//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getUiPref, setUiPref } from './ui-prefs.js';
//=============================================================================
const PREFIX = 'nf-new-diagram-';
export type NewDiagramDefaults = {
  runtime: number;
  sampleTime: number;
  execution: string;
  solver: string;
  relTol: number;
  absTol: number;
  maxStep: number;
  autoRoute: boolean;
};
//=============================================================================
function positiveNumber(key: string, fallback: number, allowZero = false): number {
  const value = Number(getUiPref(`${PREFIX}${key}`, String(fallback)));
  if (!Number.isFinite(value) || (allowZero ? value < 0 : value <= 0)) return fallback;
  return value;
}
//=============================================================================
export function readNewDiagramDefaults(): NewDiagramDefaults {
  const execution = getUiPref(`${PREFIX}execution`, 'auto') || 'auto';
  const solver = getUiPref(`${PREFIX}solver`, 'discrete') || 'discrete';
  return {
    runtime: positiveNumber('runtime', 10),
    sampleTime: positiveNumber('sample-time', 0.01),
    execution: ['auto', 'real-time', 'fast'].includes(execution) ? execution : 'auto',
    solver: ['discrete', 'ode1', 'ode4', 'ode45', 'variableNonstiff', 'variableStiff'].includes(
      solver
    )
      ? solver
      : 'discrete',
    relTol: positiveNumber('relative-tolerance', 1e-6),
    absTol: positiveNumber('absolute-tolerance', 1e-8),
    maxStep: positiveNumber('max-step', 0, true),
    autoRoute: getUiPref(`${PREFIX}auto-route`, '1') !== '0',
  };
}
//=============================================================================
function bindValue(id: string, key: string): void {
  const element = document.getElementById(id) as HTMLInputElement | HTMLSelectElement | null;
  if (!element || element.dataset.newDiagramDefaultBound === '1') return;
  element.dataset.newDiagramDefaultBound = '1';
  element.addEventListener('change', () => setUiPref(`${PREFIX}${key}`, element.value));
}
//=============================================================================
export function bindNewDiagramDefaults(): void {
  bindValue('runtimeInput', 'runtime');
  bindValue('simDt', 'sample-time');
  bindValue('realTimeMode', 'execution');
  bindValue('simSolver', 'solver');
  bindValue('simRelTol', 'relative-tolerance');
  bindValue('simAbsTol', 'absolute-tolerance');
  bindValue('simMaxStep', 'max-step');
  const autoRoute = document.getElementById('autoRouteInput') as HTMLInputElement | null;
  if (autoRoute && autoRoute.dataset.newDiagramDefaultBound !== '1') {
    autoRoute.dataset.newDiagramDefaultBound = '1';
    autoRoute.addEventListener('change', () =>
      setUiPref(`${PREFIX}auto-route`, autoRoute.checked ? '1' : '0')
    );
  }
}
//=============================================================================
