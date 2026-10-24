//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test } from 'vitest';
import { getBlockCapabilities } from './block-capabilities.js';
//=============================================================================
test('reports direct C and Rust support for a generated block', () => {
  expect(getBlockCapabilities({ type: 'sine' })).toEqual({
    c: 'supported',
    rust: 'supported',
    modelicaRequired: false,
  });
});
//=============================================================================
test('reports physical block code generation as model dependent', () => {
  expect(
    getBlockCapabilities({ type: 'RotSpeedSensor', acausal: true, physicalPins: [{}] })
  ).toEqual({
    c: 'model-dependent',
    rust: 'model-dependent',
    modelicaRequired: false,
  });
});
//=============================================================================
test('reports the Modelica bridge requirement and code generation limit', () => {
  expect(getBlockCapabilities({ type: 'modelica' })).toEqual({
    c: 'unsupported',
    rust: 'unsupported',
    modelicaRequired: true,
  });
});
//=============================================================================
test('does not claim that an FMU requires Modelica', () => {
  expect(getBlockCapabilities({ type: 'fmu' })).toEqual({
    c: 'unsupported',
    rust: 'unsupported',
    modelicaRequired: false,
  });
});
//=============================================================================
test('distinguishes dashboard code generation policies', () => {
  expect(getBlockCapabilities({ type: 'dashboardGauge' }).c).toBe('instrumentation');
  expect(getBlockCapabilities({ type: 'dashboardSlider' }).rust).toBe('tunable');
  expect(getBlockCapabilities({ type: 'dashboardCallbackButton' }).c).toBe('unsupported');
});
//=============================================================================
