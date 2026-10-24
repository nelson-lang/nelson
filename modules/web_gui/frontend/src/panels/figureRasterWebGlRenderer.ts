//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DrawPrimitive } from '../rpc/types';
import { rgbaBytes } from './figureCanvasPainter';
import {
  addFigureCanvasValue,
  measureFigureCanvas,
  recordFigureCanvasMaximum,
} from './figureCanvasProfile';
//=============================================================================
type RasterImagePrimitive = Extract<DrawPrimitive, { kind: 'image' }>;
export type FigureRasterWebGlBackend = 'webgl2';
export interface FigureRasterWebGlRenderer {
  readonly backend: FigureRasterWebGlBackend;
  render(
    primitives: DrawPrimitive[],
    width: number,
    height: number,
    dpr: number,
    panOffset: { dx: number; dy: number } | null
  ): void;
  destroy(): void;
}
//=============================================================================
type CachedTexture = {
  sourceId?: string;
  width: number;
  height: number;
  bytes: number;
  textures: WebGLTexture[];
  activeIndex: number;
};
//=============================================================================
type Rect = [number, number, number, number];
const QUAD = new Float32Array([0, 0, 1, 0, 0, 1, 1, 1]);
const RASTER_TEXTURE_RING_SIZE = 2;
export function rasterTextureCacheKey(primitive: RasterImagePrimitive, index = 0): string | null {
  if (
    (!primitive.rgba && !primitive.rgbaBytes) ||
    !primitive.imageWidth ||
    !primitive.imageHeight
  ) {
    return null;
  }
  return `raster-slot:${index}:${primitive.imageWidth}x${primitive.imageHeight}:${primitive.x},${primitive.y},${primitive.w},${primitive.h}`;
}
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
function scissorRect(rect: Rect, height: number, dpr: number): Rect {
  const [x, y, w, h] = rect;
  return [
    Math.round(x * dpr),
    Math.round((height - y - h) * dpr),
    Math.round(w * dpr),
    Math.round(h * dpr),
  ];
}
//=============================================================================
class WebGl2RasterRenderer implements FigureRasterWebGlRenderer {
  readonly backend = 'webgl2' as const;
  private readonly gl: WebGL2RenderingContext;
  private readonly program: WebGLProgram;
  private readonly vao: WebGLVertexArrayObject;
  private readonly quadBuffer: WebGLBuffer;
  private readonly canvasSizeLocation: WebGLUniformLocation;
  private readonly rectLocation: WebGLUniformLocation;
  private readonly useTextureLocation: WebGLUniformLocation;
  private readonly solidColorLocation: WebGLUniformLocation;
  private readonly textures = new Map<string, CachedTexture>();
  constructor(private readonly canvas: HTMLCanvasElement) {
    const gl = canvas.getContext('webgl2', {
      alpha: true,
      antialias: false,
      premultipliedAlpha: false,
      preserveDrawingBuffer: false,
    });
    if (!gl) throw new Error('WebGL2 unavailable');
    this.gl = gl;
    this.program = linkProgram(
      gl,
      `#version 300 es
       layout(location=0) in vec2 corner;
       uniform vec2 canvasSize;
       uniform vec4 rect;
       out vec2 texCoord;
       void main() {
         vec2 pixel = rect.xy + corner * rect.zw;
         vec2 ndc = vec2(pixel.x / canvasSize.x * 2.0 - 1.0,
                         1.0 - pixel.y / canvasSize.y * 2.0);
         gl_Position = vec4(ndc, 0.0, 1.0);
         texCoord = corner;
       }`,
      `#version 300 es
       precision mediump float;
       uniform sampler2D imageTexture;
       uniform bool useTexture;
       uniform vec4 solidColor;
       in vec2 texCoord;
       out vec4 color;
       void main() {
         color = useTexture ? texture(imageTexture, texCoord) : solidColor;
       }`
    );
    this.canvasSizeLocation = uniformLocation(gl, this.program, 'canvasSize');
    this.rectLocation = uniformLocation(gl, this.program, 'rect');
    this.useTextureLocation = uniformLocation(gl, this.program, 'useTexture');
    this.solidColorLocation = uniformLocation(gl, this.program, 'solidColor');
    const vao = gl.createVertexArray();
    const quadBuffer = gl.createBuffer();
    if (!vao || !quadBuffer) throw new Error('Cannot allocate WebGL raster quad');
    this.vao = vao;
    this.quadBuffer = quadBuffer;
    gl.bindVertexArray(vao);
    gl.bindBuffer(gl.ARRAY_BUFFER, quadBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, QUAD, gl.STATIC_DRAW);
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 8, 0);
    gl.bindVertexArray(null);
    gl.useProgram(this.program);
    const sampler = gl.getUniformLocation(this.program, 'imageTexture');
    if (sampler !== null) gl.uniform1i(sampler, 0);
  }
  //=============================================================================
  private deleteTexture(texture: CachedTexture): void {
    for (const handle of texture.textures) {
      this.gl.deleteTexture(handle);
    }
  }
  //=============================================================================
  private createTexture(): WebGLTexture | null {
    const texture = this.gl.createTexture();
    if (!texture) return null;
    this.gl.bindTexture(this.gl.TEXTURE_2D, texture);
    this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MIN_FILTER, this.gl.NEAREST);
    this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MAG_FILTER, this.gl.NEAREST);
    this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_WRAP_S, this.gl.CLAMP_TO_EDGE);
    this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_WRAP_T, this.gl.CLAMP_TO_EDGE);
    return texture;
  }
  //=============================================================================
  private color(value: string): [number, number, number, number] {
    if (value.startsWith('#') && (value.length === 7 || value.length === 9)) {
      const red = Number.parseInt(value.slice(1, 3), 16) / 255;
      const green = Number.parseInt(value.slice(3, 5), 16) / 255;
      const blue = Number.parseInt(value.slice(5, 7), 16) / 255;
      const alpha = value.length === 9 ? Number.parseInt(value.slice(7, 9), 16) / 255 : 1;
      if ([red, green, blue, alpha].every(Number.isFinite)) return [red, green, blue, alpha];
    }
    return [0, 0, 0, 1];
  }
  //=============================================================================
  private applyClip(clipRect: Rect | null, height: number, dpr: number): void {
    if (clipRect) {
      const [x, y, w, h] = scissorRect(clipRect, height, dpr);
      this.gl.enable(this.gl.SCISSOR_TEST);
      this.gl.scissor(x, y, Math.max(0, w), Math.max(0, h));
    } else {
      this.gl.disable(this.gl.SCISSOR_TEST);
    }
  }
  //=============================================================================
  private textureFor(key: string, primitive: RasterImagePrimitive): CachedTexture | null {
    if (!primitive.imageWidth || !primitive.imageHeight) return null;
    const imageWidth = primitive.imageWidth;
    const imageHeight = primitive.imageHeight;
    const bytes = rgbaBytes(primitive);
    const expected = imageWidth * imageHeight * 4;
    if (!bytes || bytes.byteLength !== expected) return null;
    let cached = this.textures.get(key);
    if (
      cached &&
      cached.width === imageWidth &&
      cached.height === imageHeight &&
      primitive.imageId !== undefined &&
      cached.sourceId === primitive.imageId
    ) {
      this.gl.bindTexture(this.gl.TEXTURE_2D, cached.textures[cached.activeIndex]);
      addFigureCanvasValue('rasterWebGlRetainedBytes', bytes.byteLength);
      addFigureCanvasValue('rasterWebGlTextureHits', 1);
      return cached;
    }
    addFigureCanvasValue('rasterWebGlTextureMisses', 1);
    if (!cached || cached.width !== imageWidth || cached.height !== imageHeight) {
      if (cached) this.deleteTexture(cached);
      const texture = this.createTexture();
      if (!texture) return null;
      cached = {
        sourceId: primitive.imageId,
        width: imageWidth,
        height: imageHeight,
        bytes: bytes.byteLength,
        textures: [texture],
        activeIndex: 0,
      };
      this.textures.set(key, cached);
      addFigureCanvasValue('rasterWebGlUploadBytes', bytes.byteLength);
      measureFigureCanvas('rasterWebGlUploadCalls', 'rasterWebGlUploadMs', () =>
        this.gl.texImage2D(
          this.gl.TEXTURE_2D,
          0,
          this.gl.RGBA,
          imageWidth,
          imageHeight,
          0,
          this.gl.RGBA,
          this.gl.UNSIGNED_BYTE,
          bytes
        )
      );
      return cached;
    }
    let initializeActiveTexture = false;
    if (cached.textures.length < RASTER_TEXTURE_RING_SIZE) {
      const texture = this.createTexture();
      if (texture) {
        cached.textures.push(texture);
        cached.activeIndex = cached.textures.length - 1;
        initializeActiveTexture = true;
      }
    } else {
      cached.activeIndex = (cached.activeIndex + 1) % cached.textures.length;
    }
    this.gl.bindTexture(this.gl.TEXTURE_2D, cached.textures[cached.activeIndex]);
    cached.sourceId = primitive.imageId;
    cached.bytes = bytes.byteLength;
    addFigureCanvasValue('rasterWebGlUploadBytes', bytes.byteLength);
    if (initializeActiveTexture) {
      measureFigureCanvas('rasterWebGlUploadCalls', 'rasterWebGlUploadMs', () =>
        this.gl.texImage2D(
          this.gl.TEXTURE_2D,
          0,
          this.gl.RGBA,
          imageWidth,
          imageHeight,
          0,
          this.gl.RGBA,
          this.gl.UNSIGNED_BYTE,
          bytes
        )
      );
    } else {
      measureFigureCanvas('rasterWebGlUploadCalls', 'rasterWebGlUploadMs', () =>
        this.gl.texSubImage2D(
          this.gl.TEXTURE_2D,
          0,
          0,
          0,
          imageWidth,
          imageHeight,
          this.gl.RGBA,
          this.gl.UNSIGNED_BYTE,
          bytes
        )
      );
    }
    return cached;
  }
  //=============================================================================
  render(
    primitives: DrawPrimitive[],
    width: number,
    height: number,
    dpr: number,
    panOffset: { dx: number; dy: number } | null
  ): void {
    const gl = this.gl;
    const targetWidth = Math.max(1, Math.round(width * dpr));
    const targetHeight = Math.max(1, Math.round(height * dpr));
    if (this.canvas.width !== targetWidth) this.canvas.width = targetWidth;
    if (this.canvas.height !== targetHeight) this.canvas.height = targetHeight;

    gl.viewport(0, 0, targetWidth, targetHeight);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);
    gl.disable(gl.DEPTH_TEST);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
    gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
    gl.useProgram(this.program);
    gl.uniform2f(this.canvasSizeLocation, width, height);
    gl.bindVertexArray(this.vao);
    gl.activeTexture(gl.TEXTURE0);

    const liveKeys = new Set<string>();
    const clipStack: (Rect | null)[] = [];
    let clipRect: Rect | null = null;
    let imageIndex = 0;
    for (const primitive of primitives) {
      switch (primitive.kind) {
        case 'clip':
          clipRect = [primitive.x, primitive.y, primitive.w, primitive.h];
          break;
        case 'clipSave':
          clipStack.push(clipRect);
          break;
        case 'clipRestore':
          clipRect = clipStack.pop() ?? null;
          break;
        case 'rect': {
          gl.uniform1i(this.useTextureLocation, 0);
          gl.uniform4fv(this.solidColorLocation, this.color(primitive.fill));
          gl.uniform4f(this.rectLocation, primitive.x, primitive.y, primitive.w, primitive.h);
          this.applyClip(clipRect, height, dpr);
          gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
          break;
        }
        case 'image': {
          const key = rasterTextureCacheKey(primitive, imageIndex);
          imageIndex++;
          if (!key) break;
          liveKeys.add(key);
          const cached = this.textureFor(key, primitive);
          if (!cached) break;
          const dx = panOffset && clipRect && clipRect[2] < width - 1 ? panOffset.dx : 0;
          const dy = panOffset && clipRect && clipRect[2] < width - 1 ? panOffset.dy : 0;
          gl.bindTexture(gl.TEXTURE_2D, cached.textures[cached.activeIndex]);
          gl.uniform1i(this.useTextureLocation, 1);
          gl.uniform4f(
            this.rectLocation,
            primitive.x + dx,
            primitive.y + dy,
            primitive.w,
            primitive.h
          );
          this.applyClip(clipRect, height, dpr);
          gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
          break;
        }
        default:
          break;
      }
    }
    gl.disable(gl.SCISSOR_TEST);
    gl.bindVertexArray(null);

    for (const [key, cached] of this.textures) {
      if (liveKeys.has(key)) continue;
      this.deleteTexture(cached);
      this.textures.delete(key);
    }
    let liveBytes = 0;
    for (const texture of this.textures.values()) liveBytes += texture.bytes;
    recordFigureCanvasMaximum('rasterWebGlMaxLiveBytes', liveBytes);
  }
  //=============================================================================
  destroy(): void {
    for (const texture of this.textures.values()) this.deleteTexture(texture);
    this.textures.clear();
    this.gl.deleteBuffer(this.quadBuffer);
    this.gl.deleteVertexArray(this.vao);
    this.gl.deleteProgram(this.program);
  }
}
//=============================================================================
export function createFigureRasterWebGlRenderer(
  canvas: HTMLCanvasElement
): FigureRasterWebGlRenderer | null {
  try {
    return new WebGl2RasterRenderer(canvas);
  } catch {
    return null;
  }
}
//=============================================================================
