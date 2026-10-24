//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  setStatusEl,
  setDiagramNameInput,
  setRuntimeInput,
  setSimDt,
  setAutoRouteInput,
  setVariablesInput,
  setVariablesPreview,
  setRendererRef,
  setSubsystemUpBtn,
} from './diagram-handlers.js';
//=============================================================================
describe('diagram-handlers setters (basic invocation)', () => {
  test('setters accept elements and null without throwing', () => {
    const status = document.createElement('div');
    const name = document.createElement('input');
    const runtime = document.createElement('input');
    const sim = document.createElement('input');
    const auto = document.createElement('input');
    auto.type = 'checkbox';
    const vars = document.createElement('input');
    const varsPreview = document.createElement('div');
    const btn = document.createElement('button');

    expect(() => setStatusEl(status)).not.toThrow();
    expect(() => setDiagramNameInput(name as HTMLInputElement)).not.toThrow();
    expect(() => setRuntimeInput(runtime as HTMLInputElement)).not.toThrow();
    expect(() => setSimDt(sim as HTMLInputElement)).not.toThrow();
    expect(() => setAutoRouteInput(auto as HTMLInputElement)).not.toThrow();
    expect(() => setVariablesInput(vars as HTMLInputElement)).not.toThrow();
    expect(() => setVariablesPreview(varsPreview as HTMLElement)).not.toThrow();
    expect(() => setSubsystemUpBtn(btn)).not.toThrow();

    expect(() => setRendererRef(null)).not.toThrow();
    expect(() => setRendererRef({ current: { selectBlock: () => {} } })).not.toThrow();
  });
});
//=============================================================================
import { vi } from 'vitest';
import * as dh from './diagram-handlers';
//=============================================================================
test('diagram-handlers setters and renderer ref assign', () => {
  const el = document.createElement('div');
  const input = document.createElement('input');
  const renderer = {
    current: { selectBlock: vi.fn(), updateBlockLabel: vi.fn(), updateConnections: vi.fn() },
  } as any;
  dh.setStatusEl(el);
  dh.setDiagramNameInput(input as any);
  dh.setRuntimeInput(input as any);
  dh.setSimDt(input as any);
  dh.setAutoRouteInput(input as any);
  dh.setVariablesInput(input as any);
  dh.setVariablesPreview(el);
  dh.setRendererRef(renderer);
  expect(typeof dh.setStatusEl).toBe('function');
});
//=============================================================================
