//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { expect, it, vi } from 'vitest';
import { FigureCanvas } from '../../src/panels/FigureCanvas';
import type { DrawPrimitive, FigureWebGlMesh } from '../../src/rpc/types';
//=============================================================================
vi.mock('../../src/panels/FigureWebGlCanvas', () => ({ FigureWebGlCanvas: () => null }));
it('keeps every subplot label on the same canvas across filled, wireframe and filled frames', () => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  const postFrame = vi.fn();
  class CanvasWorker {
    addEventListener = vi.fn();
    postMessage = postFrame;
    terminate = vi.fn();
  }
  vi.stubGlobal('Worker', CanvasWorker);
  const context = {
    setTransform: vi.fn(),
    clearRect: vi.fn(),
    fillText: vi.fn(),
  };
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue(
    context as unknown as CanvasRenderingContext2D
  );
  const transfer = Object.getOwnPropertyDescriptor(
    HTMLCanvasElement.prototype,
    'transferControlToOffscreen'
  );
  Object.defineProperty(HTMLCanvasElement.prototype, 'transferControlToOffscreen', {
    configurable: true,
    value: () => ({}),
  });
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  const primitives = ['Signal', 'Helix', 'Surface', 'Oscillation', '0', '5', '10'].map(
    (text, i) => ({
      kind: 'text',
      text,
      x: i * 10,
      y: 20,
      color: '#000',
      size: 12,
      align: 'left',
    })
  ) as DrawPrimitive[];
  const meshes = [{}] as FigureWebGlMesh[];
  const render = (webglMeshes: FigureWebGlMesh[]) =>
    act(() =>
      root.render(
        <FigureCanvas
          handle={0}
          primitives={[...primitives]}
          webglMeshes={webglMeshes}
          width={320}
          height={240}
        />
      )
    );
  try {
    render(meshes);
    const labels = host.querySelector('[data-figure-layer="text-overlay"]');
    expect(labels).not.toBeNull();
    context.fillText.mockClear();
    render([]);
    expect(host.querySelector('[data-figure-layer="text-overlay"]')).toBe(labels);
    expect(context.fillText.mock.calls.map((call) => call[0])).toEqual(
      primitives.map((p) => (p.kind === 'text' ? p.text : ''))
    );
    const frame = postFrame.mock.calls
      .map((call) => call[0])
      .filter((message) => message.type === 'frame')
      .at(-1);
    expect(frame.primitives.some((p: DrawPrimitive) => p.kind === 'text')).toBe(false);
    render(meshes);
    expect(host.querySelector('[data-figure-layer="text-overlay"]')).toBe(labels);
  } finally {
    act(() => root.unmount());
    host.remove();
    if (transfer)
      Object.defineProperty(HTMLCanvasElement.prototype, 'transferControlToOffscreen', transfer);
    else Reflect.deleteProperty(HTMLCanvasElement.prototype, 'transferControlToOffscreen');
    vi.restoreAllMocks();
    vi.unstubAllGlobals();
  }
});
//=============================================================================
