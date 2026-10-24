//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, describe, expect, it } from 'vitest';
import {
  createFigureWebGlRendererForCanvas,
  requestedBackendForClient,
} from '../../src/panels/figureWebGlRenderer';
import { isWebGpuAvailable } from '../../src/panels/figureWebGpuRenderer';
import { detectBrowserRuntimeDiagnostics } from '../../src/wasm/browserCapabilities';
//=============================================================================
// Minimal WebGL2 stub: enough for the WebGl2Renderer constructor (shader/program
// creation and uniform lookups). It lets us assert the WebGPU opt-in path falls
// back to WebGL2 when no GPU device is available.
function stubWebGl2Canvas(): HTMLCanvasElement {
  const canvas = document.createElement('canvas');
  const gl = {
    VERTEX_SHADER: 0x8b31,
    FRAGMENT_SHADER: 0x8b30,
    COMPILE_STATUS: true,
    LINK_STATUS: true,
    createShader: () => ({}),
    shaderSource: () => {},
    compileShader: () => {},
    getShaderParameter: () => true,
    getShaderInfoLog: () => '',
    deleteShader: () => {},
    createProgram: () => ({}),
    attachShader: () => {},
    linkProgram: () => {},
    getProgramParameter: () => true,
    getProgramInfoLog: () => '',
    getUniformLocation: () => ({}),
    deleteProgram: () => {},
  } as unknown as WebGL2RenderingContext;
  const original = canvas.getContext.bind(canvas);
  canvas.getContext = ((kind: string) =>
    kind === 'webgl2' ? gl : original(kind as '2d')) as typeof canvas.getContext;
  return canvas;
}
//=============================================================================
describe('WebGPU backend selection', () => {
  afterEach(() => {
    delete (globalThis.navigator as unknown as { gpu?: unknown }).gpu;
  });

  it('accepts webgpu as an explicit backend override', () => {
    expect(requestedBackendForClient('webgpu', null)).toBe('webgpu');
    expect(requestedBackendForClient(null, 'webgpu')).toBe('webgpu');
  });

  it('keeps auto resolving away from webgpu so WebGL2 stays the default', () => {
    expect(requestedBackendForClient(null, null)).toBe('auto');
    expect(requestedBackendForClient('unknown', 'webgl2')).toBe('webgl2');
  });

  it('reports WebGPU availability from navigator.gpu', () => {
    expect(isWebGpuAvailable()).toBe(false);
    (globalThis.navigator as unknown as { gpu?: unknown }).gpu = {};
    expect(isWebGpuAvailable()).toBe(true);
  });

  it('falls back to WebGL2 when webgpu is requested but no device exists', async () => {
    const canvas = stubWebGl2Canvas();
    const renderer = await createFigureWebGlRendererForCanvas(canvas, 'webgpu');
    expect(renderer).not.toBeNull();
    expect(renderer?.backend).toBe('webgl2');
    renderer?.destroy();
  });
});
//=============================================================================
describe('WebGPU capability diagnostics', () => {
  it('surfaces webGpu based on navigator.gpu presence', () => {
    expect(detectBrowserRuntimeDiagnostics({ navigator: { gpu: {} } }).features.webGpu).toBe(true);
    expect(detectBrowserRuntimeDiagnostics({ navigator: {} }).features.webGpu).toBe(false);
  });
});
//=============================================================================
