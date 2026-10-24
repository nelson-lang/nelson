//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { FigureWebGlMesh } from '../rpc/types';
//=============================================================================
export type FigureWebGlBackend = 'webgpu' | 'webgl2' | 'software';
export type FigureWebGlCanvasSource = HTMLCanvasElement | OffscreenCanvas;
export interface FigureWebGlRenderer {
  readonly backend: FigureWebGlBackend;
  render(
    meshes: FigureWebGlMesh[],
    width: number,
    height: number,
    dpr: number,
    flush?: boolean
  ): void;
  destroy(): void;
}
//=============================================================================
export function expandLineSegmentEndpoints(
  positions: ArrayLike<number>,
  indices: ArrayLike<number>
): Float32Array<ArrayBuffer> {
  const out = new Float32Array((indices.length / 2) * 6);
  let target = 0;
  for (let index = 0; index + 1 < indices.length; index += 2) {
    const start = indices[index] * 3;
    const end = indices[index + 1] * 3;
    out[target++] = positions[start];
    out[target++] = positions[start + 1];
    out[target++] = positions[start + 2];
    out[target++] = positions[end];
    out[target++] = positions[end + 1];
    out[target++] = positions[end + 2];
  }
  return out;
}
//=============================================================================
export function needsDepthPrepass(meshes: Pick<FigureWebGlMesh, 'drawFaces'>[]): boolean {
  return meshes.some((mesh) => !mesh.drawFaces);
}
//=============================================================================
const LINE_SEGMENT_CORNERS = new Float32Array([0, -1, 1, -1, 0, 1, 0, 1, 1, -1, 1, 1]);
type CachedWebGlMesh = {
  revision: string;
  depthVao: WebGLVertexArrayObject;
  fillVao?: WebGLVertexArrayObject;
  lineVao: WebGLVertexArrayObject;
  vertices: WebGLBuffer;
  colors?: WebGLBuffer;
  triangles: WebGLBuffer;
  lineEndpoints: WebGLBuffer;
  lineCorners: WebGLBuffer;
  triangleIndexType: number;
  triangleCount: number;
  lineSegmentCount: number;
};
//=============================================================================
const compileShader = (gl: WebGL2RenderingContext, type: number, source: string): WebGLShader => {
  const shader = gl.createShader(type);
  if (!shader) throw new Error('Cannot allocate WebGL shader');
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    const message = gl.getShaderInfoLog(shader) ?? 'WebGL shader compilation failed';
    gl.deleteShader(shader);
    throw new Error(message);
  }
  return shader;
};
//=============================================================================
const linkProgram = (
  gl: WebGL2RenderingContext,
  vertexSource: string,
  fragmentSource: string
): WebGLProgram => {
  const vertex = compileShader(gl, gl.VERTEX_SHADER, vertexSource);
  const fragment = compileShader(gl, gl.FRAGMENT_SHADER, fragmentSource);
  const program = gl.createProgram();
  if (!program) throw new Error('Cannot allocate WebGL program');
  gl.attachShader(program, vertex);
  gl.attachShader(program, fragment);
  gl.linkProgram(program);
  gl.deleteShader(vertex);
  gl.deleteShader(fragment);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    throw new Error(gl.getProgramInfoLog(program) ?? 'WebGL program link failed');
  }
  return program;
};
//=============================================================================
const uniformLocation = (
  gl: WebGL2RenderingContext,
  program: WebGLProgram,
  name: string
): WebGLUniformLocation => {
  const location = gl.getUniformLocation(program, name);
  if (location === null) throw new Error(`WebGL uniform ${name} unavailable`);
  return location;
};
//=============================================================================
class WebGl2Renderer implements FigureWebGlRenderer {
  readonly backend = 'webgl2' as const;
  private readonly gl: WebGL2RenderingContext;
  private readonly depthProgram: WebGLProgram;
  private readonly fillProgram: WebGLProgram;
  private readonly lineProgram: WebGLProgram;
  private readonly depthModelLocation: WebGLUniformLocation;
  private readonly depthProjectionLocation: WebGLUniformLocation;
  private readonly fillModelLocation: WebGLUniformLocation;
  private readonly fillProjectionLocation: WebGLUniformLocation;
  private readonly lineModelLocation: WebGLUniformLocation;
  private readonly lineProjectionLocation: WebGLUniformLocation;
  private readonly lineColorLocation: WebGLUniformLocation;
  private readonly lineViewportLocation: WebGLUniformLocation;
  private readonly lineWidthLocation: WebGLUniformLocation;
  private readonly meshes = new Map<number, CachedWebGlMesh>();
  private readonly activeMeshIds = new Set<number>();
  private readonly renderResources: CachedWebGlMesh[] = [];
  constructor(private readonly canvas: FigureWebGlCanvasSource) {
    const gl = canvas.getContext('webgl2', {
      alpha: true,
      antialias: true,
      premultipliedAlpha: true,
      preserveDrawingBuffer: false,
    }) as WebGL2RenderingContext | null;
    if (!gl) throw new Error('WebGL2 unavailable');
    this.gl = gl;
    this.depthProgram = linkProgram(
      gl,
      `#version 300 es
       layout(location=0) in vec3 position;
       uniform mat4 model;
       uniform mat4 projection;
       void main() {
         gl_Position = projection * model * vec4(position, 1.0);
       }`,
      `#version 300 es
       precision highp float;
       out vec4 color;
       void main() { color = vec4(0.0); }`
    );
    this.fillProgram = linkProgram(
      gl,
      `#version 300 es
       layout(location=0) in vec3 position;
       layout(location=1) in vec4 vertexColor;
       uniform mat4 model;
       uniform mat4 projection;
       out vec4 fillColor;
       void main() {
         gl_Position = projection * model * vec4(position, 1.0);
         fillColor = vertexColor;
       }`,
      `#version 300 es
       precision highp float;
       in vec4 fillColor;
       out vec4 color;
       void main() { color = fillColor; }`
    );
    this.lineProgram = linkProgram(
      gl,
      `#version 300 es
       layout(location=0) in vec3 startPosition;
       layout(location=1) in vec3 endPosition;
       layout(location=2) in vec2 corner;
       uniform mat4 model;
       uniform mat4 projection;
       uniform vec2 viewportSize;
       uniform float lineWidth;
       out float edge;
       void main() {
         vec4 startClip = projection * model * vec4(startPosition, 1.0);
         vec4 endClip = projection * model * vec4(endPosition, 1.0);
         vec4 clip = corner.x > 0.5 ? endClip : startClip;
         vec2 deltaPixels = (endClip.xy / endClip.w - startClip.xy / startClip.w)
           * viewportSize * 0.5;
         float lengthPixels = max(length(deltaPixels), 0.0001);
         vec2 normalPixels = vec2(-deltaPixels.y, deltaPixels.x) / lengthPixels;
         float halfWidth = max(0.25, lineWidth * 0.5);
         float extent = halfWidth + 0.75;
         clip.xy += normalPixels * corner.y * extent * 2.0 / viewportSize * clip.w;
         clip.z -= 0.00002 * clip.w;
         gl_Position = clip;
         edge = corner.y;
       }`,
      `#version 300 es
       precision highp float;
       uniform vec4 lineColor;
       uniform float lineWidth;
       in float edge;
       out vec4 color;
       void main() {
         float halfWidth = max(0.25, lineWidth * 0.5);
         float core = halfWidth / (halfWidth + 0.75);
         float coverage = 1.0 - smoothstep(core, 1.0, abs(edge));
         color = vec4(lineColor.rgb, lineColor.a * coverage);
       }`
    );
    this.depthModelLocation = uniformLocation(gl, this.depthProgram, 'model');
    this.depthProjectionLocation = uniformLocation(gl, this.depthProgram, 'projection');
    this.fillModelLocation = uniformLocation(gl, this.fillProgram, 'model');
    this.fillProjectionLocation = uniformLocation(gl, this.fillProgram, 'projection');
    this.lineModelLocation = uniformLocation(gl, this.lineProgram, 'model');
    this.lineProjectionLocation = uniformLocation(gl, this.lineProgram, 'projection');
    this.lineColorLocation = uniformLocation(gl, this.lineProgram, 'lineColor');
    this.lineViewportLocation = uniformLocation(gl, this.lineProgram, 'viewportSize');
    this.lineWidthLocation = uniformLocation(gl, this.lineProgram, 'lineWidth');
  }
  //=============================================================================
  private allocateMeshResource(mesh: FigureWebGlMesh): CachedWebGlMesh {
    const gl = this.gl;
    const depthVao = gl.createVertexArray();
    const lineVao = gl.createVertexArray();
    const vertices = gl.createBuffer();
    const triangles = gl.createBuffer();
    const lineEndpoints = gl.createBuffer();
    const lineCorners = gl.createBuffer();
    if (!depthVao || !lineVao || !vertices || !triangles || !lineEndpoints || !lineCorners) {
      throw new Error('Cannot allocate WebGL mesh');
    }
    gl.bindVertexArray(depthVao);
    gl.bindBuffer(gl.ARRAY_BUFFER, vertices);
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 12, 0);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, triangles);

    gl.bindVertexArray(lineVao);
    gl.bindBuffer(gl.ARRAY_BUFFER, lineEndpoints);
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 24, 0);
    gl.vertexAttribDivisor(0, 1);
    gl.enableVertexAttribArray(1);
    gl.vertexAttribPointer(1, 3, gl.FLOAT, false, 24, 12);
    gl.vertexAttribDivisor(1, 1);
    gl.bindBuffer(gl.ARRAY_BUFFER, lineCorners);
    gl.bufferData(gl.ARRAY_BUFFER, LINE_SEGMENT_CORNERS, gl.STATIC_DRAW);
    gl.enableVertexAttribArray(2);
    gl.vertexAttribPointer(2, 2, gl.FLOAT, false, 8, 0);
    gl.bindVertexArray(null);

    const resource = {
      revision: mesh.revision,
      depthVao,
      lineVao,
      vertices,
      triangles,
      lineEndpoints,
      lineCorners,
      triangleIndexType: gl.UNSIGNED_INT,
      triangleCount: 0,
      lineSegmentCount: 0,
    };
    this.meshes.set(mesh.id, resource);
    return resource;
  }
  //=============================================================================
  private ensureFillResource(mesh: CachedWebGlMesh): void {
    if (mesh.fillVao && mesh.colors) return;
    const gl = this.gl;
    const fillVao = gl.createVertexArray();
    const colors = gl.createBuffer();
    if (!fillVao || !colors) throw new Error('Cannot allocate WebGL fill mesh');
    gl.bindVertexArray(fillVao);
    gl.bindBuffer(gl.ARRAY_BUFFER, mesh.vertices);
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 12, 0);
    gl.bindBuffer(gl.ARRAY_BUFFER, colors);
    gl.enableVertexAttribArray(1);
    gl.vertexAttribPointer(1, 4, gl.FLOAT, false, 16, 0);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.triangles);
    gl.bindVertexArray(null);
    mesh.fillVao = fillVao;
    mesh.colors = colors;
  }
  //=============================================================================
  private resource(mesh: FigureWebGlMesh): CachedWebGlMesh {
    const cached = this.meshes.get(mesh.id);
    if (cached?.revision === mesh.revision) return cached;
    if (
      !mesh.positions ||
      (mesh.drawFaces && !mesh.vertexColors) ||
      !mesh.triangleIndices ||
      !mesh.lineIndices
    ) {
      throw new Error('WebGL mesh geometry unavailable');
    }
    const gl = this.gl;
    const resource = cached ?? this.allocateMeshResource(mesh);
    const positionsData =
      mesh.positions instanceof Float32Array ? mesh.positions : new Float32Array(mesh.positions);
    const triangleData =
      mesh.triangleIndices instanceof Uint16Array || mesh.triangleIndices instanceof Uint32Array
        ? mesh.triangleIndices
        : mesh.triangleIndexType === 'uint16'
        ? new Uint16Array(mesh.triangleIndices)
        : new Uint32Array(mesh.triangleIndices);
    gl.bindBuffer(gl.ARRAY_BUFFER, resource.vertices);
    gl.bufferData(gl.ARRAY_BUFFER, positionsData, gl.STATIC_DRAW);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, resource.triangles);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, triangleData, gl.STATIC_DRAW);
    if (mesh.drawFaces && mesh.vertexColors) {
      this.ensureFillResource(resource);
      const colorData =
        mesh.vertexColors instanceof Float32Array
          ? mesh.vertexColors
          : new Float32Array(mesh.vertexColors);
      gl.bindBuffer(gl.ARRAY_BUFFER, resource.colors ?? null);
      gl.bufferData(gl.ARRAY_BUFFER, colorData, gl.STATIC_DRAW);
    }
    const lineData = expandLineSegmentEndpoints(positionsData, mesh.lineIndices);
    gl.bindBuffer(gl.ARRAY_BUFFER, resource.lineEndpoints);
    gl.bufferData(gl.ARRAY_BUFFER, lineData, gl.STATIC_DRAW);
    resource.revision = mesh.revision;
    resource.triangleIndexType =
      triangleData instanceof Uint16Array ? gl.UNSIGNED_SHORT : gl.UNSIGNED_INT;
    resource.triangleCount = triangleData.length;
    resource.lineSegmentCount = lineData.length / 6;
    return resource;
  }
  //=============================================================================
  private deleteMeshResource(mesh: CachedWebGlMesh): void {
    this.gl.deleteVertexArray(mesh.depthVao);
    if (mesh.fillVao) this.gl.deleteVertexArray(mesh.fillVao);
    this.gl.deleteVertexArray(mesh.lineVao);
    this.gl.deleteBuffer(mesh.vertices);
    if (mesh.colors) this.gl.deleteBuffer(mesh.colors);
    this.gl.deleteBuffer(mesh.triangles);
    this.gl.deleteBuffer(mesh.lineEndpoints);
    this.gl.deleteBuffer(mesh.lineCorners);
  }
  //=============================================================================
  render(
    meshes: FigureWebGlMesh[],
    width: number,
    height: number,
    dpr: number,
    flush = false
  ): void {
    const gl = this.gl;
    const targetWidth = Math.max(1, Math.round(width * dpr));
    const targetHeight = Math.max(1, Math.round(height * dpr));
    if (this.canvas.width !== targetWidth || this.canvas.height !== targetHeight) {
      this.canvas.width = targetWidth;
      this.canvas.height = targetHeight;
    }
    this.activeMeshIds.clear();
    this.renderResources.length = meshes.length;
    for (let i = 0; i < meshes.length; i++) {
      this.activeMeshIds.add(meshes[i].id);
      this.renderResources[i] = this.resource(meshes[i]);
    }
    for (const [id, resource] of this.meshes) {
      if (this.activeMeshIds.has(id)) continue;
      this.deleteMeshResource(resource);
      this.meshes.delete(id);
    }
    gl.clearColor(0, 0, 0, 0);
    gl.clearDepth(1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);
    gl.enable(gl.BLEND);
    // RGB is premultiplied by blending into the transparent canvas. Alpha must
    // accumulate coverage independently; multiplying it by SRC_ALPHA again
    // makes translucent faces too pale when the browser composites the canvas.
    gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA, gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
    if (needsDepthPrepass(meshes)) {
      gl.useProgram(this.depthProgram);
      gl.colorMask(false, false, false, false);
      gl.enable(gl.POLYGON_OFFSET_FILL);
      gl.polygonOffset(1, 1);
      for (let i = 0; i < meshes.length; i++) {
        const mesh = meshes[i];
        const resource = this.renderResources[i];
        if (mesh.drawFaces || resource.triangleCount === 0) continue;
        const [x, y, w, h] = mesh.viewport;
        gl.viewport(
          Math.round(x * dpr),
          Math.round(y * dpr),
          Math.round(w * dpr),
          Math.round(h * dpr)
        );
        gl.uniformMatrix4fv(this.depthModelLocation, false, mesh.model);
        gl.uniformMatrix4fv(this.depthProjectionLocation, false, mesh.projection);
        gl.bindVertexArray(resource.depthVao);
        gl.drawElements(gl.TRIANGLES, resource.triangleCount, resource.triangleIndexType, 0);
      }
      gl.colorMask(true, true, true, true);
      gl.disable(gl.POLYGON_OFFSET_FILL);
    }
    gl.depthMask(true);
    gl.useProgram(this.fillProgram);
    for (let i = 0; i < meshes.length; i++) {
      const mesh = meshes[i];
      const resource = this.renderResources[i];
      if (!mesh.drawFaces || resource.triangleCount === 0) continue;
      const [x, y, w, h] = mesh.viewport;
      gl.viewport(
        Math.round(x * dpr),
        Math.round(y * dpr),
        Math.round(w * dpr),
        Math.round(h * dpr)
      );
      gl.uniformMatrix4fv(this.fillModelLocation, false, mesh.model);
      gl.uniformMatrix4fv(this.fillProjectionLocation, false, mesh.projection);
      if (!resource.fillVao) continue;
      gl.bindVertexArray(resource.fillVao);
      gl.drawElements(gl.TRIANGLES, resource.triangleCount, resource.triangleIndexType, 0);
    }
    gl.depthMask(false);
    gl.useProgram(this.lineProgram);
    for (let i = 0; i < meshes.length; i++) {
      const mesh = meshes[i];
      const resource = this.renderResources[i];
      if (resource.lineSegmentCount === 0) continue;
      const [x, y, w, h] = mesh.viewport;
      gl.viewport(
        Math.round(x * dpr),
        Math.round(y * dpr),
        Math.round(w * dpr),
        Math.round(h * dpr)
      );
      gl.uniformMatrix4fv(this.lineModelLocation, false, mesh.model);
      gl.uniformMatrix4fv(this.lineProjectionLocation, false, mesh.projection);
      gl.uniform4fv(this.lineColorLocation, mesh.lineColor);
      gl.uniform2f(this.lineViewportLocation, w * dpr, h * dpr);
      gl.uniform1f(this.lineWidthLocation, mesh.lineWidth * dpr);
      gl.bindVertexArray(resource.lineVao);
      gl.drawArraysInstanced(gl.TRIANGLES, 0, 6, resource.lineSegmentCount);
    }
    gl.depthMask(true);
    gl.bindVertexArray(null);
    if (flush) gl.flush?.();
    this.renderResources.length = 0;
  }
  //=============================================================================
  destroy(): void {
    for (const mesh of this.meshes.values()) {
      this.deleteMeshResource(mesh);
    }
    this.meshes.clear();
    this.activeMeshIds.clear();
    this.renderResources.length = 0;
    this.gl.deleteProgram(this.depthProgram);
    this.gl.deleteProgram(this.fillProgram);
    this.gl.deleteProgram(this.lineProgram);
  }
}
//=============================================================================
const isKnownBackend = (value: string | null): value is FigureWebGlBackend =>
  value === 'webgpu' || value === 'webgl2' || value === 'software';
// 'auto' resolves to WebGL2 with a software fallback; WebGPU stays opt-in so it
// never regresses the default figures. The query/localStorage override forces a
// specific path ('webgpu' to try the experimental backend, 'software' to debug
// the CPU fallback).
export function requestedBackendForClient(
  queryValue: string | null,
  storedValue: string | null
): FigureWebGlBackend | 'auto' {
  if (isKnownBackend(queryValue)) return queryValue;
  if (isKnownBackend(storedValue)) return storedValue;
  return 'auto';
}
//=============================================================================
export const resolveRequestedBackend = (): FigureWebGlBackend | 'auto' => {
  const query = new URLSearchParams(window.location.search).get('renderer');
  const stored = window.localStorage.getItem('nelson.webRenderer');
  return requestedBackendForClient(query, stored);
};
//=============================================================================
function tryCreateWebGl2Renderer(canvas: FigureWebGlCanvasSource): FigureWebGlRenderer | null {
  try {
    return new WebGl2Renderer(canvas);
  } catch {
    return null;
  }
}
//=============================================================================
export async function createFigureWebGlRendererForCanvas(
  canvas: FigureWebGlCanvasSource,
  backend: FigureWebGlBackend | 'auto' = 'auto'
): Promise<FigureWebGlRenderer | null> {
  if (backend === 'software') return null;
  if (backend === 'webgpu') {
    // Lazy import keeps the WebGPU code out of the default bundle path until a
    // caller opts in. On failure, fall back to WebGL2 on the same canvas (the
    // WebGPU attempt leaves the element untouched when the device is missing).
    const { tryCreateWebGpuRenderer } = await import('./figureWebGpuRenderer');
    const webgpu = await tryCreateWebGpuRenderer(canvas);
    if (webgpu) return webgpu;
    return tryCreateWebGl2Renderer(canvas);
  }
  return tryCreateWebGl2Renderer(canvas);
}
//=============================================================================
export async function createFigureWebGlRenderer(
  canvas: HTMLCanvasElement,
  backend: FigureWebGlBackend | 'auto' = resolveRequestedBackend()
): Promise<FigureWebGlRenderer | null> {
  return createFigureWebGlRendererForCanvas(canvas, backend);
}
//=============================================================================
