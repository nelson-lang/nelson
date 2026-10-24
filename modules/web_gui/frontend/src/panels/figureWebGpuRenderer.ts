//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Experimental WebGPU backend for the figure vector (3D mesh) renderer. It
// consumes the same FigureWebGlMesh payload as the WebGL2 renderer
// (figureWebGlRenderer.ts) and translates the three GLSL ES 3.00 programs (depth
// prepass, per-vertex fill, instanced screen-space line) to WGSL. WebGL2 stays
// the default; this path is reached only when the caller asks for `webgpu` (see
// createFigureWebGlRendererForCanvas), so a quirk here cannot regress the default
// figures.
//
// Two coordinate-system differences versus WebGL are handled explicitly:
//   - WebGPU clip-space depth is [0, 1] (WebGL is [-1, 1]); the engine's
//     projection matrices target WebGL, so every vertex shader remaps
//     `z = (z + w) * 0.5`.
//   - WebGPU framebuffer viewport origin is top-left (WebGL is bottom-left), so
//     per-mesh viewports are flipped vertically.
//
import type { FigureWebGlMesh } from '../rpc/types';
import {
  expandLineSegmentEndpoints,
  needsDepthPrepass,
  type FigureWebGlCanvasSource,
  type FigureWebGlRenderer,
} from './figureWebGlRenderer';
//=============================================================================
// The project does not depend on @webgpu/types; declare only the surface used
// here so the module type-checks without pulling the whole API in.
type GPUTextureFormat = string;
type GPUBufferUsageFlags = number;
type GPUTextureUsageFlags = number;
interface GPUObjectBase {
  destroy?: () => void;
}
//=============================================================================
interface GPUQueue {
  writeBuffer(buffer: GPUBuffer, bufferOffset: number, data: ArrayBufferView): void;
  submit(commandBuffers: GPUCommandBuffer[]): void;
}
//=============================================================================
interface GPUBuffer extends GPUObjectBase {
  destroy(): void;
}
//=============================================================================
interface GPUTextureView {
  readonly __brand?: 'GPUTextureView';
}
//=============================================================================
interface GPUTexture extends GPUObjectBase {
  createView(): GPUTextureView;
  destroy(): void;
}
//=============================================================================
interface GPUShaderModule {
  readonly __brand?: 'GPUShaderModule';
}
//=============================================================================
interface GPUBindGroupLayout {
  readonly __brand?: 'GPUBindGroupLayout';
}
//=============================================================================
interface GPUPipelineLayout {
  readonly __brand?: 'GPUPipelineLayout';
}
//=============================================================================
interface GPUBindGroup {
  readonly __brand?: 'GPUBindGroup';
}
//=============================================================================
interface GPURenderPipeline {
  readonly __brand?: 'GPURenderPipeline';
}
//=============================================================================
interface GPUCommandBuffer {
  readonly __brand?: 'GPUCommandBuffer';
}
//=============================================================================
interface GPURenderPassEncoder {
  setPipeline(pipeline: GPURenderPipeline): void;
  setBindGroup(index: number, bindGroup: GPUBindGroup, dynamicOffsets?: number[]): void;
  setVertexBuffer(slot: number, buffer: GPUBuffer, offset?: number): void;
  setIndexBuffer(buffer: GPUBuffer, indexFormat: 'uint16' | 'uint32', offset?: number): void;
  setViewport(
    x: number,
    y: number,
    width: number,
    height: number,
    minDepth: number,
    maxDepth: number
  ): void;
  draw(vertexCount: number, instanceCount?: number): void;
  drawIndexed(indexCount: number, instanceCount?: number): void;
  end(): void;
}
//=============================================================================
interface GPUCommandEncoder {
  beginRenderPass(descriptor: unknown): GPURenderPassEncoder;
  finish(): GPUCommandBuffer;
}
//=============================================================================
interface GPUCanvasContext {
  configure(configuration: {
    device: GPUDevice;
    format: GPUTextureFormat;
    alphaMode?: 'opaque' | 'premultiplied';
  }): void;
  getCurrentTexture(): GPUTexture;
}
//=============================================================================
interface GPUDeviceLostInfo {
  readonly reason: string;
  readonly message: string;
}
//=============================================================================
interface GPUDevice {
  readonly queue: GPUQueue;
  readonly lost: Promise<GPUDeviceLostInfo>;
  createShaderModule(descriptor: { code: string }): GPUShaderModule;
  createBindGroupLayout(descriptor: unknown): GPUBindGroupLayout;
  createPipelineLayout(descriptor: { bindGroupLayouts: GPUBindGroupLayout[] }): GPUPipelineLayout;
  createRenderPipeline(descriptor: unknown): GPURenderPipeline;
  createBuffer(descriptor: { size: number; usage: GPUBufferUsageFlags }): GPUBuffer;
  createTexture(descriptor: {
    size: { width: number; height: number };
    format: GPUTextureFormat;
    usage: GPUTextureUsageFlags;
  }): GPUTexture;
  createBindGroup(descriptor: unknown): GPUBindGroup;
  createCommandEncoder(): GPUCommandEncoder;
  destroy(): void;
}
//=============================================================================
interface GPUAdapter {
  requestDevice(): Promise<GPUDevice>;
}
//=============================================================================
interface GPU {
  requestAdapter(options?: { powerPreference?: string }): Promise<GPUAdapter | null>;
  getPreferredCanvasFormat(): GPUTextureFormat;
}
//=============================================================================
declare const GPUBufferUsage: {
  VERTEX: number;
  INDEX: number;
  UNIFORM: number;
  COPY_DST: number;
};
//=============================================================================
declare const GPUTextureUsage: { RENDER_ATTACHMENT: number };
declare const GPUShaderStage: { VERTEX: number; FRAGMENT: number };
function webGpuApi(): GPU | null {
  const gpu = (globalThis.navigator as unknown as { gpu?: GPU } | undefined)?.gpu;
  return gpu ?? null;
}
//=============================================================================
export function isWebGpuAvailable(): boolean {
  return webGpuApi() !== null;
}
//=============================================================================
// Shared uniform block (std140 layout, one 256-byte slot per mesh addressed
// through a dynamic offset): model, projection, line colour, and the packed
// (viewportW, viewportH, lineWidth) the screen-space line expansion needs.
const UNIFORM_WGSL = `
struct Uniforms {
  model: mat4x4<f32>,
  projection: mat4x4<f32>,
  lineColor: vec4<f32>,
  viewportLine: vec4<f32>,
};
@group(0) @binding(0) var<uniform> u: Uniforms;
`;
const DEPTH_WGSL = `${UNIFORM_WGSL}
@vertex
fn vs_main(@location(0) position: vec3<f32>) -> @builtin(position) vec4<f32> {
  var clip = u.projection * u.model * vec4<f32>(position, 1.0);
  clip.z = (clip.z + clip.w) * 0.5;
  return clip;
}
@fragment
fn fs_main() -> @location(0) vec4<f32> {
  return vec4<f32>(0.0);
}
`;
const FILL_WGSL = `${UNIFORM_WGSL}
struct VSOut {
  @builtin(position) position: vec4<f32>,
  @location(0) color: vec4<f32>,
};
@vertex
fn vs_main(@location(0) position: vec3<f32>, @location(1) vertexColor: vec4<f32>) -> VSOut {
  var out: VSOut;
  var clip = u.projection * u.model * vec4<f32>(position, 1.0);
  clip.z = (clip.z + clip.w) * 0.5;
  out.position = clip;
  out.color = vertexColor;
  return out;
}
@fragment
fn fs_main(in: VSOut) -> @location(0) vec4<f32> {
  return in.color;
}
`;
const LINE_WGSL = `${UNIFORM_WGSL}
struct VSOut {
  @builtin(position) position: vec4<f32>,
  @location(0) edge: f32,
};
@vertex
fn vs_main(
  @location(0) startPosition: vec3<f32>,
  @location(1) endPosition: vec3<f32>,
  @location(2) corner: vec2<f32>
) -> VSOut {
  let viewportSize = u.viewportLine.xy;
  let lineWidth = u.viewportLine.z;
  let startClip = u.projection * u.model * vec4<f32>(startPosition, 1.0);
  let endClip = u.projection * u.model * vec4<f32>(endPosition, 1.0);
  var clip = select(startClip, endClip, corner.x > 0.5);
  let deltaPixels = (endClip.xy / endClip.w - startClip.xy / startClip.w) * viewportSize * 0.5;
  let lengthPixels = max(length(deltaPixels), 0.0001);
  let normalPixels = vec2<f32>(-deltaPixels.y, deltaPixels.x) / lengthPixels;
  let halfWidth = max(0.25, lineWidth * 0.5);
  let extent = halfWidth + 0.75;
  let offsetXy = clip.xy + normalPixels * corner.y * extent * 2.0 / viewportSize * clip.w;
  var depth = clip.z - 0.00002 * clip.w;
  depth = (depth + clip.w) * 0.5;
  var out: VSOut;
  out.position = vec4<f32>(offsetXy, depth, clip.w);
  out.edge = corner.y;
  return out;
}
@fragment
fn fs_main(in: VSOut) -> @location(0) vec4<f32> {
  let lineWidth = u.viewportLine.z;
  let halfWidth = max(0.25, lineWidth * 0.5);
  let core = halfWidth / (halfWidth + 0.75);
  let coverage = 1.0 - smoothstep(core, 1.0, abs(in.edge));
  return vec4<f32>(u.lineColor.rgb, u.lineColor.a * coverage);
}
`;
const LINE_SEGMENT_CORNERS = new Float32Array([0, -1, 1, -1, 0, 1, 0, 1, 1, -1, 1, 1]);
const UNIFORM_SLOT_SIZE = 256; // minUniformBufferOffsetAlignment default
const UNIFORM_FLOATS_PER_SLOT = UNIFORM_SLOT_SIZE / 4;
const PREMULTIPLIED_BLEND = {
  color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' },
  alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' },
};
//=============================================================================
type CachedWebGpuMesh = {
  revision: string;
  vertices: GPUBuffer;
  colors?: GPUBuffer;
  triangles?: GPUBuffer;
  lineEndpoints?: GPUBuffer;
  triangleIndexFormat: 'uint16' | 'uint32';
  triangleCount: number;
  lineSegmentCount: number;
};
//=============================================================================
const align4 = (value: number): number => (value + 3) & ~3;
function toFloat32(data: ArrayLike<number> | Float32Array | undefined): Float32Array {
  if (data instanceof Float32Array) return data;
  return new Float32Array(data ?? []);
}
//=============================================================================
class WebGpuRenderer implements FigureWebGlRenderer {
  readonly backend = 'webgpu' as const;
  private readonly meshes = new Map<number, CachedWebGpuMesh>();
  private readonly activeMeshIds = new Set<number>();
  private depthTexture: GPUTexture | null = null;
  private depthWidth = 0;
  private depthHeight = 0;
  private uniformBuffer: GPUBuffer | null = null;
  private uniformBindGroup: GPUBindGroup | null = null;
  private uniformCapacity = 0;
  private lost = false;
  constructor(
    private readonly canvas: FigureWebGlCanvasSource,
    private readonly device: GPUDevice,
    private readonly context: GPUCanvasContext,
    private readonly bindGroupLayout: GPUBindGroupLayout,
    private readonly cornerBuffer: GPUBuffer,
    private readonly depthPipeline: GPURenderPipeline,
    private readonly fillPipeline: GPURenderPipeline,
    private readonly linePipeline: GPURenderPipeline
  ) {
    void this.device.lost.then((info) => {
      // A lost device cannot be reused; mark the renderer so the next render()
      // throws and the canvas falls back to a fresh backend.
      this.lost = true;
      void info;
    });
  }
  //=============================================================================
  private createBuffer(data: ArrayBufferView, usage: number): GPUBuffer {
    const size = align4(data.byteLength);
    const buffer = this.device.createBuffer({ size, usage: usage | GPUBufferUsage.COPY_DST });
    this.device.queue.writeBuffer(buffer, 0, data);
    return buffer;
  }
  //=============================================================================
  private ensureDepthTexture(width: number, height: number): GPUTextureView {
    if (!this.depthTexture || this.depthWidth !== width || this.depthHeight !== height) {
      this.depthTexture?.destroy();
      this.depthTexture = this.device.createTexture({
        size: { width, height },
        format: 'depth24plus',
        usage: GPUTextureUsage.RENDER_ATTACHMENT,
      });
      this.depthWidth = width;
      this.depthHeight = height;
    }
    return this.depthTexture.createView();
  }
  //=============================================================================
  private ensureUniformBuffer(meshCount: number): void {
    const needed = Math.max(1, meshCount);
    if (this.uniformBuffer && this.uniformCapacity >= needed) return;
    this.uniformBuffer?.destroy();
    this.uniformCapacity = needed;
    this.uniformBuffer = this.device.createBuffer({
      size: needed * UNIFORM_SLOT_SIZE,
      usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    });
    this.uniformBindGroup = this.device.createBindGroup({
      layout: this.bindGroupLayout,
      entries: [
        {
          binding: 0,
          resource: { buffer: this.uniformBuffer, offset: 0, size: UNIFORM_SLOT_SIZE },
        },
      ],
    });
  }
  //=============================================================================
  private resource(mesh: FigureWebGlMesh): CachedWebGpuMesh {
    const cached = this.meshes.get(mesh.id);
    if (cached?.revision === mesh.revision) return cached;
    cached?.vertices.destroy();
    cached?.colors?.destroy();
    cached?.triangles?.destroy();
    cached?.lineEndpoints?.destroy();
    if (!mesh.positions || !mesh.triangleIndices || !mesh.lineIndices) {
      throw new Error('WebGPU mesh geometry unavailable');
    }
    const positions = toFloat32(mesh.positions);
    const triangles =
      mesh.triangleIndices instanceof Uint16Array
        ? mesh.triangleIndices
        : mesh.triangleIndices instanceof Uint32Array
        ? mesh.triangleIndices
        : mesh.triangleIndexType === 'uint16'
        ? new Uint16Array(mesh.triangleIndices)
        : new Uint32Array(mesh.triangleIndices);
    const lineEndpoints = expandLineSegmentEndpoints(positions, mesh.lineIndices);
    const next: CachedWebGpuMesh = {
      revision: mesh.revision,
      vertices: this.createBuffer(positions, GPUBufferUsage.VERTEX),
      triangles:
        triangles.length > 0 ? this.createBuffer(triangles, GPUBufferUsage.INDEX) : undefined,
      lineEndpoints:
        lineEndpoints.length > 0
          ? this.createBuffer(lineEndpoints, GPUBufferUsage.VERTEX)
          : undefined,
      triangleIndexFormat: triangles instanceof Uint16Array ? 'uint16' : 'uint32',
      triangleCount: triangles.length,
      lineSegmentCount: lineEndpoints.length / 6,
    };
    if (mesh.drawFaces && mesh.vertexColors) {
      next.colors = this.createBuffer(toFloat32(mesh.vertexColors), GPUBufferUsage.VERTEX);
    }
    this.meshes.set(mesh.id, next);
    return next;
  }
  //=============================================================================
  private writeUniforms(meshes: FigureWebGlMesh[], dpr: number): Float32Array {
    const staging = new Float32Array(Math.max(1, meshes.length) * UNIFORM_FLOATS_PER_SLOT);
    for (let i = 0; i < meshes.length; i++) {
      const mesh = meshes[i];
      const base = i * UNIFORM_FLOATS_PER_SLOT;
      staging.set(mesh.model, base);
      staging.set(mesh.projection, base + 16);
      staging.set(mesh.lineColor, base + 32);
      staging[base + 36] = mesh.viewport[2] * dpr;
      staging[base + 37] = mesh.viewport[3] * dpr;
      staging[base + 38] = mesh.lineWidth * dpr;
    }
    return staging;
  }
  //=============================================================================
  private meshViewport(
    mesh: FigureWebGlMesh,
    targetHeight: number,
    dpr: number
  ): [number, number, number, number] {
    const [x, y, w, h] = mesh.viewport;
    const width = Math.max(1, Math.round(w * dpr));
    const height = Math.max(1, Math.round(h * dpr));
    // WebGL viewport y is bottom-left; WebGPU is top-left, so flip.
    const top = Math.max(0, targetHeight - Math.round((y + h) * dpr));
    return [Math.round(x * dpr), top, width, height];
  }
  //=============================================================================
  render(meshes: FigureWebGlMesh[], width: number, height: number, dpr: number): void {
    if (this.lost) throw new Error('WebGPU device lost');
    const targetWidth = Math.max(1, Math.round(width * dpr));
    const targetHeight = Math.max(1, Math.round(height * dpr));
    if (this.canvas.width !== targetWidth) this.canvas.width = targetWidth;
    if (this.canvas.height !== targetHeight) this.canvas.height = targetHeight;

    this.activeMeshIds.clear();
    const resources = meshes.map((mesh) => {
      this.activeMeshIds.add(mesh.id);
      return this.resource(mesh);
    });
    for (const [id, resource] of this.meshes) {
      if (this.activeMeshIds.has(id)) continue;
      resource.vertices.destroy();
      resource.colors?.destroy();
      resource.triangles?.destroy();
      resource.lineEndpoints?.destroy();
      this.meshes.delete(id);
    }

    this.ensureUniformBuffer(meshes.length);
    const uniformBuffer = this.uniformBuffer;
    const bindGroup = this.uniformBindGroup;
    if (!uniformBuffer || !bindGroup) throw new Error('WebGPU uniforms unavailable');
    this.device.queue.writeBuffer(uniformBuffer, 0, this.writeUniforms(meshes, dpr));

    const depthView = this.ensureDepthTexture(targetWidth, targetHeight);
    const encoder = this.device.createCommandEncoder();
    const pass = encoder.beginRenderPass({
      colorAttachments: [
        {
          view: this.context.getCurrentTexture().createView(),
          clearValue: { r: 0, g: 0, b: 0, a: 0 },
          loadOp: 'clear',
          storeOp: 'store',
        },
      ],
      depthStencilAttachment: {
        view: depthView,
        depthClearValue: 1,
        depthLoadOp: 'clear',
        depthStoreOp: 'store',
      },
    });

    const setViewport = (mesh: FigureWebGlMesh): void => {
      const [vx, vy, vw, vh] = this.meshViewport(mesh, targetHeight, dpr);
      pass.setViewport(vx, vy, vw, vh, 0, 1);
    };

    // Depth prepass for wireframe meshes so their own faces occlude lines.
    if (needsDepthPrepass(meshes)) {
      pass.setPipeline(this.depthPipeline);
      for (let i = 0; i < meshes.length; i++) {
        const mesh = meshes[i];
        const resource = resources[i];
        if (mesh.drawFaces || !resource.triangles || resource.triangleCount === 0) continue;
        setViewport(mesh);
        pass.setBindGroup(0, bindGroup, [i * UNIFORM_SLOT_SIZE]);
        pass.setVertexBuffer(0, resource.vertices);
        pass.setIndexBuffer(resource.triangles, resource.triangleIndexFormat);
        pass.drawIndexed(resource.triangleCount);
      }
    }

    pass.setPipeline(this.fillPipeline);
    for (let i = 0; i < meshes.length; i++) {
      const mesh = meshes[i];
      const resource = resources[i];
      if (!mesh.drawFaces || !resource.triangles || !resource.colors) continue;
      setViewport(mesh);
      pass.setBindGroup(0, bindGroup, [i * UNIFORM_SLOT_SIZE]);
      pass.setVertexBuffer(0, resource.vertices);
      pass.setVertexBuffer(1, resource.colors);
      pass.setIndexBuffer(resource.triangles, resource.triangleIndexFormat);
      pass.drawIndexed(resource.triangleCount);
    }

    pass.setPipeline(this.linePipeline);
    for (let i = 0; i < meshes.length; i++) {
      const mesh = meshes[i];
      const resource = resources[i];
      if (!resource.lineEndpoints || resource.lineSegmentCount === 0) continue;
      setViewport(mesh);
      pass.setBindGroup(0, bindGroup, [i * UNIFORM_SLOT_SIZE]);
      pass.setVertexBuffer(0, resource.lineEndpoints);
      pass.setVertexBuffer(1, this.cornerBuffer);
      pass.draw(6, resource.lineSegmentCount);
    }

    pass.end();
    this.device.queue.submit([encoder.finish()]);
  }
  //=============================================================================
  destroy(): void {
    for (const resource of this.meshes.values()) {
      resource.vertices.destroy();
      resource.colors?.destroy();
      resource.triangles?.destroy();
      resource.lineEndpoints?.destroy();
    }
    this.meshes.clear();
    this.activeMeshIds.clear();
    this.cornerBuffer.destroy();
    this.uniformBuffer?.destroy();
    this.depthTexture?.destroy();
    this.uniformBuffer = null;
    this.uniformBindGroup = null;
    this.depthTexture = null;
    if (!this.lost) this.device.destroy();
  }
}
//=============================================================================
function buildPipelines(
  device: GPUDevice,
  format: GPUTextureFormat
): {
  bindGroupLayout: GPUBindGroupLayout;
  depthPipeline: GPURenderPipeline;
  fillPipeline: GPURenderPipeline;
  linePipeline: GPURenderPipeline;
} {
  const bindGroupLayout = device.createBindGroupLayout({
    entries: [
      {
        binding: 0,
        visibility: GPUShaderStage.VERTEX | GPUShaderStage.FRAGMENT,
        buffer: { type: 'uniform', hasDynamicOffset: true, minBindingSize: UNIFORM_SLOT_SIZE },
      },
    ],
  });
  const layout = device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] });
  const positionLayout = {
    arrayStride: 12,
    attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x3' }],
  };
  const colorLayout = {
    arrayStride: 16,
    attributes: [{ shaderLocation: 1, offset: 0, format: 'float32x4' }],
  };
  const endpointLayout = {
    arrayStride: 24,
    stepMode: 'instance',
    attributes: [
      { shaderLocation: 0, offset: 0, format: 'float32x3' },
      { shaderLocation: 1, offset: 12, format: 'float32x3' },
    ],
  };
  const cornerLayout = {
    arrayStride: 8,
    attributes: [{ shaderLocation: 2, offset: 0, format: 'float32x2' }],
  };

  const depthModule = device.createShaderModule({ code: DEPTH_WGSL });
  const fillModule = device.createShaderModule({ code: FILL_WGSL });
  const lineModule = device.createShaderModule({ code: LINE_WGSL });

  const depthPipeline = device.createRenderPipeline({
    layout,
    vertex: { module: depthModule, entryPoint: 'vs_main', buffers: [positionLayout] },
    fragment: {
      module: depthModule,
      entryPoint: 'fs_main',
      targets: [{ format, writeMask: 0 }],
    },
    primitive: { topology: 'triangle-list' },
    depthStencil: {
      format: 'depth24plus',
      depthWriteEnabled: true,
      depthCompare: 'less-equal',
      depthBias: 1,
      depthBiasSlopeScale: 1,
    },
  });

  const fillPipeline = device.createRenderPipeline({
    layout,
    vertex: { module: fillModule, entryPoint: 'vs_main', buffers: [positionLayout, colorLayout] },
    fragment: {
      module: fillModule,
      entryPoint: 'fs_main',
      targets: [{ format, blend: PREMULTIPLIED_BLEND }],
    },
    primitive: { topology: 'triangle-list' },
    depthStencil: { format: 'depth24plus', depthWriteEnabled: true, depthCompare: 'less-equal' },
  });

  const linePipeline = device.createRenderPipeline({
    layout,
    vertex: { module: lineModule, entryPoint: 'vs_main', buffers: [endpointLayout, cornerLayout] },
    fragment: {
      module: lineModule,
      entryPoint: 'fs_main',
      targets: [{ format, blend: PREMULTIPLIED_BLEND }],
    },
    primitive: { topology: 'triangle-list' },
    depthStencil: { format: 'depth24plus', depthWriteEnabled: false, depthCompare: 'less-equal' },
  });

  return { bindGroupLayout, depthPipeline, fillPipeline, linePipeline };
}
//=============================================================================
export async function tryCreateWebGpuRenderer(
  canvas: FigureWebGlCanvasSource
): Promise<FigureWebGlRenderer | null> {
  const gpu = webGpuApi();
  if (!gpu) return null;
  try {
    // Acquire the device BEFORE touching the canvas: if this fails the canvas
    // keeps no 'webgpu' context, so the caller can still fall back to WebGL2 on
    // the same element.
    const adapter = await gpu.requestAdapter({ powerPreference: 'high-performance' });
    if (!adapter) return null;
    const device = await adapter.requestDevice();
    const context = (
      canvas as unknown as {
        getContext(id: 'webgpu'): GPUCanvasContext | null;
      }
    ).getContext('webgpu');
    if (!context) {
      device.destroy();
      return null;
    }
    const format = gpu.getPreferredCanvasFormat();
    context.configure({ device, format, alphaMode: 'premultiplied' });
    const { bindGroupLayout, depthPipeline, fillPipeline, linePipeline } = buildPipelines(
      device,
      format
    );
    const cornerBuffer = device.createBuffer({
      size: LINE_SEGMENT_CORNERS.byteLength,
      usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
    });
    device.queue.writeBuffer(cornerBuffer, 0, LINE_SEGMENT_CORNERS);
    return new WebGpuRenderer(
      canvas,
      device,
      context,
      bindGroupLayout,
      cornerBuffer,
      depthPipeline,
      fillPipeline,
      linePipeline
    );
  } catch {
    return null;
  }
}
//=============================================================================
