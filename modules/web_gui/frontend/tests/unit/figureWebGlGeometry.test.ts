//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import {
  hasCompleteFigureWebGlGeometry,
  isMatrixOnlyWebGlFrame,
  queueFigureWebGlAnimationFrameForHandle,
  queueLatestWebGlAnimationFrame,
  retainFigureDisplayListWebGlGeometry,
  retainFigureWebGlGeometry,
  takePendingFigureWebGlAnimationFrame,
} from '../../src/panels/figureWebGlGeometry';
import {
  expandLineSegmentEndpoints,
  createFigureWebGlRenderer,
  createFigureWebGlRendererForCanvas,
  needsDepthPrepass,
  requestedBackendForClient,
} from '../../src/panels/figureWebGlRenderer';
import type { FigureDisplayListResult, FigureWebGlMesh } from '../../src/rpc/types';
//=============================================================================
const mesh = (over: Partial<FigureWebGlMesh> = {}): FigureWebGlMesh => ({
  id: 0,
  revision: '1',
  positions: [0, 0, 0, 1, 1, 1],
  vertexColors: [1, 0, 0, 1, 0, 1, 0, 1],
  triangleIndices: [0, 1, 1],
  lineIndices: [0, 1],
  model: new Array(16).fill(0),
  projection: new Array(16).fill(0),
  viewport: [0, 0, 100, 100],
  lineColor: [0, 0, 0, 1],
  lineWidth: 1,
  drawFaces: false,
  ...over,
});
//=============================================================================
function float32Base64(values: number[]): string {
  const bytes = new Uint8Array(new Float32Array(values).buffer);
  return Buffer.from(bytes).toString('base64');
}
//=============================================================================
function uint32Base64(values: number[]): string {
  const bytes = new Uint8Array(new Uint32Array(values).buffer);
  return Buffer.from(bytes).toString('base64');
}
//=============================================================================
function uint16Base64(values: number[]): string {
  const bytes = new Uint8Array(new Uint16Array(values).buffer);
  return Buffer.from(bytes).toString('base64');
}
//=============================================================================
function uint8Base64(values: number[]): string {
  return Buffer.from(Uint8Array.from(values)).toString('base64');
}
//=============================================================================
describe('compact WebGL wire form (float16 positions, uint8 colours)', () => {
  it('decodes normalized float16 positions with the bounding box and uint8 colours', () => {
    // half 0x0000 = 0.0, 0x3C00 = 1.0; box maps [0,1] per axis to [min,max].
    const retained = new Map();
    const result = retainFigureWebGlGeometry(
      [
        mesh({
          id: 7,
          positions: undefined,
          vertexColors: undefined,
          drawFaces: true,
          positionsF16: uint16Base64([0x0000, 0x0000, 0x0000, 0x3c00, 0x3c00, 0x3c00]),
          positionBBox: [0, 0, 0, 10, 20, 30],
          vertexColorsU8: uint8Base64([255, 0, 0, 255, 0, 255, 0, 255]),
          triangleIndicesBase64: uint32Base64([0, 1, 1]),
          triangleIndexType: 'uint32',
          lineIndicesBase64: uint32Base64([0, 1]),
          lineIndexType: 'uint32',
        }),
      ],
      retained
    );
    const geometry = retained.get(7);
    expect(Array.from(geometry.positions)).toEqual([0, 0, 0, 10, 20, 30]);
    expect(Array.from(geometry.vertexColors)).toEqual([1, 0, 0, 1, 0, 1, 0, 1]);
    // the compact payload is dropped once decoded to typed arrays
    expect(result[0].positionsF16).toBeUndefined();
    expect(result[0].vertexColorsU8).toBeUndefined();
  });
});
//=============================================================================
describe('expandLineSegmentEndpoints', () => {
  it('packs segment endpoints for instanced line rendering', () => {
    const endpoints = expandLineSegmentEndpoints([1, 2, 3, 4, 5, 6, 7, 8, 9], [2, 0, 1, 2]);
    expect(Array.from(endpoints)).toEqual([7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  });
});
//=============================================================================
describe('WebGL backend selection', () => {
  it('ignores unknown stored renderers', () => {
    expect(requestedBackendForClient(null, 'unknown')).toBe('auto');
    expect(requestedBackendForClient('unknown', 'webgl2')).toBe('webgl2');
    expect(requestedBackendForClient(null, 'webgl2')).toBe('webgl2');
    expect(requestedBackendForClient('software', null)).toBe('software');
  });

  it('can select the software fallback without reading browser globals', async () => {
    const canvas = document.createElement('canvas');
    await expect(createFigureWebGlRendererForCanvas(canvas, 'software')).resolves.toBeNull();
  });
});
//=============================================================================
describe('needsDepthPrepass', () => {
  it('runs only for non-face meshes', () => {
    expect(needsDepthPrepass([mesh({ drawFaces: true })])).toBe(false);
    expect(needsDepthPrepass([mesh({ drawFaces: true }), mesh({ drawFaces: false })])).toBe(true);
  });
});
//=============================================================================
describe('WebGL resource reuse', () => {
  it('updates changed mesh buffers without reallocating VAOs', async () => {
    const canvas = document.createElement('canvas');
    const calls = { createVertexArray: 0, deleteVertexArray: 0, bufferData: 0, drawType: 0 };
    const gl = {
      ARRAY_BUFFER: 0x8892,
      BLEND: 0x0be2,
      COLOR_BUFFER_BIT: 0x4000,
      COMPILE_STATUS: true,
      DEPTH_BUFFER_BIT: 0x0100,
      DEPTH_TEST: 0x0b71,
      ELEMENT_ARRAY_BUFFER: 0x8893,
      FLOAT: 0x1406,
      FRAGMENT_SHADER: 0x8b30,
      LEQUAL: 0x0203,
      LINK_STATUS: true,
      ONE_MINUS_SRC_ALPHA: 0x0303,
      POLYGON_OFFSET_FILL: 0x8037,
      SRC_ALPHA: 0x0302,
      STATIC_DRAW: 0x88e4,
      TRIANGLES: 0x0004,
      UNSIGNED_INT: 0x1405,
      UNSIGNED_SHORT: 0x1403,
      VERTEX_SHADER: 0x8b31,
      attachShader: () => {},
      bindBuffer: () => {},
      bindVertexArray: () => {},
      blendFuncSeparate: () => {},
      bufferData: () => {
        calls.bufferData += 1;
      },
      clear: () => {},
      clearColor: () => {},
      clearDepth: () => {},
      colorMask: () => {},
      compileShader: () => {},
      createBuffer: () => ({}),
      createProgram: () => ({}),
      createShader: () => ({}),
      createVertexArray: () => {
        calls.createVertexArray += 1;
        return {};
      },
      deleteBuffer: () => {},
      deleteProgram: () => {},
      deleteShader: () => {},
      deleteVertexArray: () => {
        calls.deleteVertexArray += 1;
      },
      depthFunc: () => {},
      depthMask: () => {},
      disable: () => {},
      drawArraysInstanced: () => {},
      drawElements: (_mode: number, _count: number, type: number) => {
        calls.drawType = type;
      },
      enable: () => {},
      enableVertexAttribArray: () => {},
      getProgramInfoLog: () => '',
      getProgramParameter: () => true,
      getShaderInfoLog: () => '',
      getShaderParameter: () => true,
      getUniformLocation: () => ({}),
      linkProgram: () => {},
      polygonOffset: () => {},
      shaderSource: () => {},
      uniform1f: () => {},
      uniform2f: () => {},
      uniform4fv: () => {},
      uniformMatrix4fv: () => {},
      useProgram: () => {},
      vertexAttribDivisor: () => {},
      vertexAttribPointer: () => {},
      viewport: () => {},
    } as unknown as WebGL2RenderingContext;
    const originalGetContext = canvas.getContext.bind(canvas);
    canvas.getContext = ((kind: string) =>
      kind === 'webgl2' ? gl : originalGetContext(kind as '2d')) as typeof canvas.getContext;
    const renderer = await createFigureWebGlRenderer(canvas);
    expect(renderer).not.toBeNull();
    renderer?.render(
      [mesh({ revision: '1', drawFaces: true, triangleIndices: new Uint16Array([0, 1, 1]) })],
      100,
      100,
      1
    );
    expect(calls.createVertexArray).toBe(3);
    expect(calls.drawType).toBe(0x1403);
    renderer?.render([mesh({ revision: '2', drawFaces: true })], 100, 100, 1);
    expect(calls.createVertexArray).toBe(3);
    expect(calls.deleteVertexArray).toBe(0);
    expect(calls.bufferData).toBeGreaterThan(4);
    renderer?.destroy();
  });

  it('renders wireframe meshes without vertex color buffers', async () => {
    const canvas = document.createElement('canvas');
    const calls = { createBuffer: 0, createVertexArray: 0 };
    const gl = {
      ARRAY_BUFFER: 0x8892,
      BLEND: 0x0be2,
      COLOR_BUFFER_BIT: 0x4000,
      COMPILE_STATUS: true,
      DEPTH_BUFFER_BIT: 0x0100,
      DEPTH_TEST: 0x0b71,
      ELEMENT_ARRAY_BUFFER: 0x8893,
      FLOAT: 0x1406,
      FRAGMENT_SHADER: 0x8b30,
      LEQUAL: 0x0203,
      LINK_STATUS: true,
      ONE_MINUS_SRC_ALPHA: 0x0303,
      POLYGON_OFFSET_FILL: 0x8037,
      SRC_ALPHA: 0x0302,
      STATIC_DRAW: 0x88e4,
      TRIANGLES: 0x0004,
      UNSIGNED_INT: 0x1405,
      UNSIGNED_SHORT: 0x1403,
      VERTEX_SHADER: 0x8b31,
      attachShader: () => {},
      bindBuffer: () => {},
      bindVertexArray: () => {},
      blendFuncSeparate: () => {},
      bufferData: () => {},
      clear: () => {},
      clearColor: () => {},
      clearDepth: () => {},
      colorMask: () => {},
      compileShader: () => {},
      createBuffer: () => {
        calls.createBuffer += 1;
        return {};
      },
      createProgram: () => ({}),
      createShader: () => ({}),
      createVertexArray: () => {
        calls.createVertexArray += 1;
        return {};
      },
      deleteBuffer: () => {},
      deleteProgram: () => {},
      deleteShader: () => {},
      deleteVertexArray: () => {},
      depthFunc: () => {},
      depthMask: () => {},
      disable: () => {},
      drawArraysInstanced: () => {},
      drawElements: () => {},
      enable: () => {},
      enableVertexAttribArray: () => {},
      getProgramInfoLog: () => '',
      getProgramParameter: () => true,
      getShaderInfoLog: () => '',
      getShaderParameter: () => true,
      getUniformLocation: () => ({}),
      linkProgram: () => {},
      polygonOffset: () => {},
      shaderSource: () => {},
      uniform1f: () => {},
      uniform2f: () => {},
      uniform4fv: () => {},
      uniformMatrix4fv: () => {},
      useProgram: () => {},
      vertexAttribDivisor: () => {},
      vertexAttribPointer: () => {},
      viewport: () => {},
    } as unknown as WebGL2RenderingContext;
    const originalGetContext = canvas.getContext.bind(canvas);
    canvas.getContext = ((kind: string) =>
      kind === 'webgl2' ? gl : originalGetContext(kind as '2d')) as typeof canvas.getContext;
    const renderer = await createFigureWebGlRenderer(canvas);
    expect(renderer).not.toBeNull();
    expect(() =>
      renderer?.render(
        [
          mesh({
            drawFaces: false,
            vertexColors: undefined,
            vertexColorsBase64: undefined,
          }),
        ],
        100,
        100,
        1
      )
    ).not.toThrow();
    expect(calls.createVertexArray).toBe(2);
    expect(calls.createBuffer).toBe(4);
    renderer?.destroy();
  });

  it('deletes buffers for meshes that leave the WebGL frame', async () => {
    const canvas = document.createElement('canvas');
    const calls = { createVertexArray: 0, deleteVertexArray: 0, deleteBuffer: 0 };
    const gl = {
      ARRAY_BUFFER: 0x8892,
      BLEND: 0x0be2,
      COLOR_BUFFER_BIT: 0x4000,
      COMPILE_STATUS: true,
      DEPTH_BUFFER_BIT: 0x0100,
      DEPTH_TEST: 0x0b71,
      ELEMENT_ARRAY_BUFFER: 0x8893,
      FLOAT: 0x1406,
      FRAGMENT_SHADER: 0x8b30,
      LEQUAL: 0x0203,
      LINK_STATUS: true,
      ONE_MINUS_SRC_ALPHA: 0x0303,
      POLYGON_OFFSET_FILL: 0x8037,
      SRC_ALPHA: 0x0302,
      STATIC_DRAW: 0x88e4,
      TRIANGLES: 0x0004,
      UNSIGNED_INT: 0x1405,
      UNSIGNED_SHORT: 0x1403,
      VERTEX_SHADER: 0x8b31,
      attachShader: () => {},
      bindBuffer: () => {},
      bindVertexArray: () => {},
      blendFuncSeparate: () => {},
      bufferData: () => {},
      clear: () => {},
      clearColor: () => {},
      clearDepth: () => {},
      colorMask: () => {},
      compileShader: () => {},
      createBuffer: () => ({}),
      createProgram: () => ({}),
      createShader: () => ({}),
      createVertexArray: () => {
        calls.createVertexArray += 1;
        return {};
      },
      deleteBuffer: () => {
        calls.deleteBuffer += 1;
      },
      deleteProgram: () => {},
      deleteShader: () => {},
      deleteVertexArray: () => {
        calls.deleteVertexArray += 1;
      },
      depthFunc: () => {},
      depthMask: () => {},
      disable: () => {},
      drawArraysInstanced: () => {},
      drawElements: () => {},
      enable: () => {},
      enableVertexAttribArray: () => {},
      getProgramInfoLog: () => '',
      getProgramParameter: () => true,
      getShaderInfoLog: () => '',
      getShaderParameter: () => true,
      getUniformLocation: () => ({}),
      linkProgram: () => {},
      polygonOffset: () => {},
      shaderSource: () => {},
      uniform1f: () => {},
      uniform2f: () => {},
      uniform4fv: () => {},
      uniformMatrix4fv: () => {},
      useProgram: () => {},
      vertexAttribDivisor: () => {},
      vertexAttribPointer: () => {},
      viewport: () => {},
    } as unknown as WebGL2RenderingContext;
    const originalGetContext = canvas.getContext.bind(canvas);
    canvas.getContext = ((kind: string) =>
      kind === 'webgl2' ? gl : originalGetContext(kind as '2d')) as typeof canvas.getContext;
    const renderer = await createFigureWebGlRenderer(canvas);
    expect(renderer).not.toBeNull();
    renderer?.render(
      [mesh({ id: 1, drawFaces: true }), mesh({ id: 2, drawFaces: true })],
      100,
      100,
      1
    );
    expect(calls.createVertexArray).toBe(6);
    renderer?.render([mesh({ id: 2, drawFaces: true })], 100, 100, 1);
    expect(calls.createVertexArray).toBe(6);
    expect(calls.deleteVertexArray).toBe(3);
    expect(calls.deleteBuffer).toBe(5);
    renderer?.destroy();
  });
});
//=============================================================================
describe('retainFigureWebGlGeometry', () => {
  it('detects whether a startup frame contains every WebGL buffer', () => {
    expect(hasCompleteFigureWebGlGeometry([mesh()])).toBe(true);
    expect(
      hasCompleteFigureWebGlGeometry([
        mesh({
          positions: undefined,
          vertexColors: undefined,
          triangleIndices: undefined,
          lineIndices: undefined,
        }),
      ])
    ).toBe(false);
  });

  it('hydrates a compact display list immediately after the full snapshot', () => {
    const retained = new Map();
    const full: FigureDisplayListResult = {
      width: 100,
      height: 100,
      primitives: [],
      webglMeshes: [mesh({ id: 4, revision: '8' })],
    };
    retainFigureDisplayListWebGlGeometry(full, retained);
    const compact = retainFigureDisplayListWebGlGeometry(
      {
        ...full,
        webglMeshes: [
          mesh({
            id: 4,
            revision: '8',
            positions: undefined,
            vertexColors: undefined,
            triangleIndices: undefined,
            lineIndices: undefined,
          }),
        ],
      },
      retained
    );
    expect(compact.webglMeshes?.[0].positions).toBeInstanceOf(Float32Array);
    expect(compact.webglMeshes?.[0].triangleIndices).toBeInstanceOf(Uint32Array);
  });

  it('reuses geometry for matrix-only frames with the same revision', () => {
    const retained = new Map();
    retainFigureWebGlGeometry([mesh()], retained);
    const next = retainFigureWebGlGeometry(
      [
        mesh({
          positions: undefined,
          vertexColors: undefined,
          triangleIndices: undefined,
          lineIndices: undefined,
          lineColor: undefined,
          lineWidth: undefined,
          drawFaces: undefined,
          model: new Array(16).fill(2),
        }),
      ],
      retained
    );
    expect(Array.from(next[0].positions ?? [])).toEqual([0, 0, 0, 1, 1, 1]);
    expect(Array.from(next[0].vertexColors ?? [])).toEqual([1, 0, 0, 1, 0, 1, 0, 1]);
    expect(Array.from(next[0].triangleIndices ?? [])).toEqual([0, 1, 1]);
    expect(Array.from(next[0].lineIndices ?? [])).toEqual([0, 1]);
    expect(next[0].lineColor).toEqual([0, 0, 0, 1]);
    expect(next[0].lineWidth).toBe(1);
    expect(next[0].drawFaces).toBe(false);
    expect(next[0].model[0]).toBe(2);
  });

  it('decodes compact buffers once and reuses them for matrix-only frames', () => {
    const retained = new Map();
    const first = retainFigureWebGlGeometry(
      [
        mesh({
          positions: undefined,
          vertexColors: undefined,
          triangleIndices: undefined,
          lineIndices: undefined,
          positionsBase64: float32Base64([0, 0, 0, 2, 2, 2]),
          vertexColorsBase64: float32Base64([1, 1, 0, 1, 0, 0, 1, 1]),
          triangleIndicesBase64: uint32Base64([0, 1, 1]),
          lineIndicesBase64: uint32Base64([1, 0]),
        }),
      ],
      retained
    );
    expect(first[0].positions).toBeInstanceOf(Float32Array);
    expect(first[0].positionsBase64).toBeUndefined();
    expect(Array.from(first[0].positions ?? [])).toEqual([0, 0, 0, 2, 2, 2]);
    const next = retainFigureWebGlGeometry(
      [
        mesh({
          positions: undefined,
          vertexColors: undefined,
          triangleIndices: undefined,
          lineIndices: undefined,
          model: new Array(16).fill(3),
        }),
      ],
      retained
    );
    expect(Array.from(next[0].positions ?? [])).toEqual([0, 0, 0, 2, 2, 2]);
    expect(Array.from(next[0].vertexColors ?? [])).toEqual([1, 1, 0, 1, 0, 0, 1, 1]);
    expect(Array.from(next[0].triangleIndices ?? [])).toEqual([0, 1, 1]);
    expect(Array.from(next[0].lineIndices ?? [])).toEqual([1, 0]);
    expect(next[0].model[0]).toBe(3);
  });

  it('decodes compact uint16 index buffers', () => {
    const retained = new Map();
    const first = retainFigureWebGlGeometry(
      [
        mesh({
          positions: undefined,
          vertexColors: undefined,
          triangleIndices: undefined,
          lineIndices: undefined,
          positionsBase64: float32Base64([0, 0, 0, 2, 2, 2]),
          vertexColorsBase64: float32Base64([1, 1, 0, 1, 0, 0, 1, 1]),
          triangleIndicesBase64: uint16Base64([0, 1, 1]),
          triangleIndexType: 'uint16',
          lineIndicesBase64: uint16Base64([1, 0]),
          lineIndexType: 'uint16',
        }),
      ],
      retained
    );
    expect(first[0].triangleIndices).toBeInstanceOf(Uint16Array);
    expect(first[0].lineIndices).toBeInstanceOf(Uint16Array);
    expect(Array.from(first[0].triangleIndices ?? [])).toEqual([0, 1, 1]);
    expect(Array.from(first[0].lineIndices ?? [])).toEqual([1, 0]);
  });

  it('retains wireframe geometry without vertex colors', () => {
    const retained = new Map();
    const first = retainFigureWebGlGeometry(
      [
        mesh({
          drawFaces: false,
          vertexColors: undefined,
          positionsBase64: float32Base64([0, 0, 0, 2, 2, 2]),
          triangleIndicesBase64: uint16Base64([0, 1, 1]),
          triangleIndexType: 'uint16',
          lineIndicesBase64: uint16Base64([1, 0]),
          lineIndexType: 'uint16',
        }),
      ],
      retained
    );
    expect(first[0].positions).toBeInstanceOf(Float32Array);
    expect(first[0].vertexColors).toBeUndefined();
    expect(first[0].triangleIndices).toBeInstanceOf(Uint16Array);
    const next = retainFigureWebGlGeometry(
      [
        mesh({
          drawFaces: false,
          positions: undefined,
          vertexColors: undefined,
          triangleIndices: undefined,
          lineIndices: undefined,
        }),
      ],
      retained
    );
    expect(next[0].positions).toBeInstanceOf(Float32Array);
    expect(next[0].vertexColors).toBeUndefined();
    expect(next[0].lineIndices).toBeInstanceOf(Uint16Array);
  });

  it('does not reuse stale geometry after a revision change', () => {
    const retained = new Map();
    retainFigureWebGlGeometry([mesh()], retained);
    const next = retainFigureWebGlGeometry(
      [
        mesh({
          revision: '2',
          positions: undefined,
          vertexColors: undefined,
          triangleIndices: undefined,
          lineIndices: undefined,
        }),
      ],
      retained
    );
    expect(next[0].positions).toBeUndefined();
    expect(next[0].vertexColors).toBeUndefined();
    expect(next[0].triangleIndices).toBeUndefined();
    expect(next[0].lineIndices).toBeUndefined();
  });

  it('drops retained geometry for meshes that are no longer in the frame', () => {
    const retained = new Map();
    retainFigureWebGlGeometry([mesh({ id: 1 }), mesh({ id: 2 })], retained);
    expect(retained.has(1)).toBe(true);
    expect(retained.has(2)).toBe(true);
    retainFigureWebGlGeometry(
      [
        mesh({
          id: 2,
          positions: undefined,
          vertexColors: undefined,
          triangleIndices: undefined,
          lineIndices: undefined,
        }),
      ],
      retained
    );
    expect(retained.has(1)).toBe(false);
    expect(retained.has(2)).toBe(true);
  });
});
//=============================================================================
describe('queueLatestWebGlAnimationFrame', () => {
  it('keeps the newest pending WebGL animation frame only', () => {
    const first = [mesh({ revision: '1' })];
    const second = [mesh({ revision: '2' })];
    expect(queueLatestWebGlAnimationFrame([], first)).toEqual([first]);
    expect(queueLatestWebGlAnimationFrame([first], second)).toEqual([second]);
  });
});
//=============================================================================
describe('pending figure WebGL frames', () => {
  it('retains the latest matrix-only frame until the WebGL canvas is mounted', () => {
    const first = [mesh({ revision: '1' })];
    const second = [mesh({ revision: '2' })];
    queueFigureWebGlAnimationFrameForHandle(42, first);
    queueFigureWebGlAnimationFrameForHandle(42, second);
    expect(takePendingFigureWebGlAnimationFrame(42)).toEqual(second);
    expect(takePendingFigureWebGlAnimationFrame(42)).toBeNull();
  });
});
//=============================================================================
describe('isMatrixOnlyWebGlFrame', () => {
  const displayList = (webglMesh: FigureWebGlMesh): FigureDisplayListResult => ({
    handle: 1,
    width: 100,
    height: 100,
    primitives: [{ kind: 'rect', x: 0, y: 0, w: 100, h: 100, fill: '#fff' }],
    controls: [],
    webglMeshes: [webglMesh],
  });

  it('accepts unchanged static content with matrix-only meshes', () => {
    const current = displayList(mesh());
    const next = displayList(
      mesh({
        positions: undefined,
        vertexColors: undefined,
        triangleIndices: undefined,
        lineIndices: undefined,
      })
    );
    expect(isMatrixOnlyWebGlFrame(current, next)).toBe(true);
  });

  it('uses display signatures before comparing full primitives', () => {
    const current = displayList(mesh());
    current.displaySignature = '42';
    const next = displayList(
      mesh({
        positions: undefined,
        vertexColors: undefined,
        triangleIndices: undefined,
        lineIndices: undefined,
      })
    );
    next.displaySignature = '42';
    next.primitives = [];
    next.controls = [];
    expect(isMatrixOnlyWebGlFrame(current, next)).toBe(true);
    next.displaySignature = '43';
    expect(isMatrixOnlyWebGlFrame(current, next)).toBe(false);
  });

  it('accepts compact backend matrix-only frames', () => {
    const current = displayList(mesh());
    const next = displayList(
      mesh({
        positions: undefined,
        vertexColors: undefined,
        triangleIndices: undefined,
        lineIndices: undefined,
      })
    );
    next.matrixOnly = true;
    next.primitives = [];
    next.controls = [];
    expect(isMatrixOnlyWebGlFrame(current, next)).toBe(true);
    expect(
      isMatrixOnlyWebGlFrame(
        current,
        displayList(
          mesh({
            positions: undefined,
            positionsBase64: float32Base64([0, 0, 0]),
          })
        )
      )
    ).toBe(false);
    next.width = 120;
    expect(isMatrixOnlyWebGlFrame(current, next)).toBe(false);
  });

  it('rejects changed primitives and refreshed geometry', () => {
    const current = displayList(mesh());
    const changed = displayList(
      mesh({
        positions: undefined,
        vertexColors: undefined,
        triangleIndices: undefined,
        lineIndices: undefined,
      })
    );
    changed.primitives = [];
    expect(isMatrixOnlyWebGlFrame(current, changed)).toBe(false);
    expect(isMatrixOnlyWebGlFrame(current, displayList(mesh({ revision: '2' })))).toBe(false);
  });
});
//=============================================================================
