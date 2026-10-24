//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type CodegenCapability =
  | 'supported'
  | 'model-dependent'
  | 'tunable'
  | 'instrumentation'
  | 'unsupported';
export type BlockCapabilities = {
  c: CodegenCapability;
  rust: CodegenCapability;
  modelicaRequired: boolean;
};
//=============================================================================
type CapabilityBlock = {
  type?: unknown;
  acausal?: unknown;
  physicalPins?: unknown;
};
//=============================================================================
// Mirrors the direct C/Rust support stamped into the block help from the
// authoritative native code-generation registry. Blocks lowered as part of a
// complete diagram are reported separately as model-dependent.
const DIRECT_CODEGEN_TYPES = new Set([
  'abs',
  'and',
  'atan2',
  'backlash',
  'bias',
  'chirp',
  'clock',
  'compareToConstant',
  'compareToZero',
  'constant',
  'crossProduct',
  'ddelay',
  'deadZone',
  'delay',
  'derivative',
  'difference',
  'directLookup',
  'display',
  'divide',
  'dotProduct',
  'dstateSpace',
  'dtf',
  'fileSink',
  'fileSource',
  'foh',
  'fromWorkspace',
  'gain',
  'hpf',
  'impulse',
  'integrator',
  'lookup1D',
  'lookup2D',
  'lookupND',
  'lpf',
  'mathFunction',
  'max',
  'min',
  'mult',
  'multiportSwitch',
  'negate',
  'noise',
  'not',
  'or',
  'pid',
  'productOfElements',
  'quantizer',
  'ramp',
  'rate',
  'relationalOperator',
  'roundingFunction',
  'saturation',
  'scope',
  'sign',
  'sine',
  'sqrt',
  'stateSpace',
  'step',
  'sum',
  'sumElements',
  'switch',
  'tf',
  'toggleSwitch',
  'toWorkspace',
  'trigFunction',
  'unitDelay',
  'xor',
  'xyScope',
  'xyzScope',
  'zoh',
]);
//=============================================================================
const NON_GENERATABLE_TYPES = new Set([
  'constraint',
  'fmu',
  'fmuMe',
  'if',
  'merge',
  'modelica',
  'nelsonFunction',
  'switchCase',
]);
//=============================================================================
function normalizeType(value: unknown): string {
  const type = String(value ?? '');
  const parts = type.split('.');
  return parts[parts.length - 1] || type;
}
//=============================================================================
export function getBlockCapabilities(block: CapabilityBlock): BlockCapabilities {
  const type = normalizeType(block.type);
  const isAcausal = block.acausal === true || Array.isArray(block.physicalPins);
  let codegen: CodegenCapability = 'model-dependent';

  const dashboardObserver = new Set([
    'dashboardScope',
    'dashboardDisplay',
    'dashboardGauge',
    'dashboardHalfGauge',
    'dashboardQuarterGauge',
    'dashboardLinearGauge',
    'dashboardLamp',
    'dashboardMultiStateImage',
  ]);

  if (dashboardObserver.has(type)) {
    codegen = 'instrumentation';
  } else if (type === 'dashboardCallbackButton') {
    codegen = 'unsupported';
  } else if (type.startsWith('dashboard')) {
    codegen = 'tunable';
  } else if (DIRECT_CODEGEN_TYPES.has(type)) {
    codegen = 'supported';
  } else if (NON_GENERATABLE_TYPES.has(type)) {
    codegen = 'unsupported';
  } else if (isAcausal || type === 'subsystem') {
    codegen = 'model-dependent';
  }

  return {
    c: codegen,
    rust: codegen,
    modelicaRequired: type === 'modelica',
  };
}
//=============================================================================
