//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Client-only replayer for a standalone Nelson figure scene. It renders the
// JSON written by the savefigurescene builtin with no engine and no network:
// the 2D display list on a canvas, the 3D meshes on a WebGL canvas, and offline
// gestures (2D pan, wheel zoom, 3D rotate, reset) computed from the scene data.
// The same display list drives the live desktop panel, so both stay in step.
//=============================================================================
import {
  paintFigure2D,
  figureBaseLayerPrimitives,
  figureTextOverlayPrimitives,
  imageCacheKey,
  rgbaImageData,
  canCreateRgbaImageBitmap,
  type CachedImageSource,
} from '../panels/figureCanvasPainter';
import { retainFigureWebGlGeometry } from '../panels/figureWebGlGeometry';
import {
  createFigureWebGlRendererForCanvas,
  type FigureWebGlRenderer,
} from '../panels/figureWebGlRenderer';
import type { DrawPrimitive, FigureWebGlMesh } from '../rpc/types';
//=============================================================================
// The highest scene version this replayer understands. A newer file falls back
// to its baked raster image so a figure is never blank. Version 2 adds the
// axes3d block (3D axis box/grid/panes/labels); an older replayer sees version 2
// as too new and rasters, which is correct because a v2 scene moves the 3D axis
// decorations out of the 2D primitives into axes3d.
export const SUPPORTED_SCENE_VERSION = 2;
//=============================================================================
export interface RotatableAxesEntry {
  handle: number;
  is3d: boolean;
  rect: [number, number, number, number];
  view: [number, number];
  interactive?: boolean;
}
//=============================================================================
// Scene v2 (axes3d): the 3D axis decorations of a rotatable axes, in data-space
// geometry plus the axes base transform, so the replayer can reproject the whole
// box/grid/panes/labels under one camera together with the WebGL surface. A v1
// scene has no axes3d and replays exactly as before.
export interface SceneAxes3dSegments {
  // flat line segments: [ax,ay,az, bx,by,bz, ...] (6 numbers per segment)
  segments: number[];
  color?: string;
  width?: number;
}
export interface SceneAxes3dFaces {
  // filled polygons (back panes): flat xyz, `sizes` gives the vertex count per face
  positions: number[];
  sizes: number[];
  color?: string;
}
export interface SceneAxes3dLabel {
  text: string;
  pos: [number, number, number];
  color?: string;
  size?: number;
  align?: CanvasTextAlign;
  baseline?: CanvasTextBaseline;
  font?: string;
  bold?: boolean;
  italic?: boolean;
  rotation?: number; // degrees, counter-clockwise
}
export interface SceneAxes3dEntry {
  handle?: number;
  model: number[]; // axes base modelview, column-major 4x4
  projection: number[]; // column-major 4x4
  viewport: [number, number, number, number]; // x,y,w,h in figure pixels (GL bottom-origin)
  boxCentre?: [number, number, number]; // orbit pivot in data coords
  panes?: SceneAxes3dFaces;
  grid?: SceneAxes3dSegments;
  box?: SceneAxes3dSegments;
  labels?: SceneAxes3dLabel[];
}
//=============================================================================
export interface NelsonFigureScene {
  format: string;
  version: number;
  width: number;
  height: number;
  devicePixelRatio?: number;
  background?: string;
  primitives: DrawPrimitive[];
  controls?: unknown[];
  webglMeshes?: FigureWebGlMesh[];
  softwareFallbackImage?: string;
  rotatableAxes?: RotatableAxesEntry[];
  axes3d?: SceneAxes3dEntry[];
  is3d?: boolean;
  // The engine figure handle this scene was captured from. A frame-kernel scene
  // carries it so the output can go live; a saved standalone scene may not, and
  // then the output only ever replays static.
  figureHandle?: number;
  handle?: number;
}
//=============================================================================
export interface SceneReplayHandle {
  destroy(): void;
}
//=============================================================================
export function isNelsonFigureScene(value: unknown): value is NelsonFigureScene {
  if (typeof value !== 'object' || value === null) return false;
  const scene = value as Partial<NelsonFigureScene>;
  return (
    scene.format === 'nelson-figure' &&
    typeof scene.width === 'number' &&
    typeof scene.height === 'number' &&
    Array.isArray(scene.primitives)
  );
}
//=============================================================================
// The engine figure handle a scene carries, or -1 when it has none. The engine
// writes it as `figureHandle`; `handle` is accepted as well so either naming
// goes live. A scene with no handle can only be replayed static.
export function sceneFigureHandle(scene: NelsonFigureScene): number {
  const value = typeof scene.figureHandle === 'number' ? scene.figureHandle : scene.handle;
  return typeof value === 'number' && Number.isFinite(value) && value >= 0 ? value : -1;
}
//=============================================================================
// Decode the raster image primitives into a cache the painter can draw. Vector
// scenes have none, so the map is empty and nothing is awaited.
async function buildImageCache(
  primitives: DrawPrimitive[]
): Promise<Map<string, CachedImageSource>> {
  const images = new Map<string, CachedImageSource>();
  let index = 0;
  const pending: Promise<void>[] = [];
  for (const primitive of primitives) {
    if (primitive.kind !== 'image') continue;
    const key = imageCacheKey(primitive, index);
    index++;
    if (!key || images.has(key)) continue;
    if ((primitive.rgba || primitive.rgbaBytes) && canCreateRgbaImageBitmap()) {
      const data = rgbaImageData(primitive);
      if (data) {
        pending.push(
          createImageBitmap(data).then((bitmap) => {
            images.set(key, { source: bitmap });
          })
        );
        continue;
      }
    }
    const url = primitive.image;
    if (typeof url === 'string' && url.length > 0) {
      const element = new Image();
      element.src = url;
      pending.push(
        element
          .decode()
          .then(() => {
            images.set(key, { source: element });
          })
          .catch(() => {
            /* a broken data URI is skipped, not fatal */
          })
      );
    }
  }
  await Promise.all(pending);
  return images;
}
//=============================================================================
// Minimal column-major 4x4 helpers (same layout as the mesh model/projection
// matrices). Only what the offline orbit needs.
type Mat4 = number[];
function mat4Identity(): Mat4 {
  return [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
}
function mat4Multiply(a: Mat4, b: Mat4): Mat4 {
  const out = new Array<number>(16).fill(0);
  for (let col = 0; col < 4; col++) {
    for (let row = 0; row < 4; row++) {
      let sum = 0;
      for (let k = 0; k < 4; k++) sum += a[k * 4 + row] * b[col * 4 + k];
      out[col * 4 + row] = sum;
    }
  }
  return out;
}
function mat4Translation(x: number, y: number, z: number): Mat4 {
  const m = mat4Identity();
  m[12] = x;
  m[13] = y;
  m[14] = z;
  return m;
}
function mat4RotationX(angle: number): Mat4 {
  const c = Math.cos(angle);
  const s = Math.sin(angle);
  return [1, 0, 0, 0, 0, c, s, 0, 0, -s, c, 0, 0, 0, 0, 1];
}
function mat4RotationY(angle: number): Mat4 {
  const c = Math.cos(angle);
  const s = Math.sin(angle);
  return [c, 0, -s, 0, 0, 1, 0, 0, s, 0, c, 0, 0, 0, 0, 1];
}
// Transform a point by a column-major 4x4, returning the full homogeneous vec4.
function mat4ApplyToPoint(
  m: Mat4,
  x: number,
  y: number,
  z: number
): [number, number, number, number] {
  return [
    m[0] * x + m[4] * y + m[8] * z + m[12],
    m[1] * x + m[5] * y + m[9] * z + m[13],
    m[2] * x + m[6] * y + m[10] * z + m[14],
    m[3] * x + m[7] * y + m[11] * z + m[15],
  ];
}
// A centred dolly: scaling clip-space x/y zooms the whole scene about the
// viewport centre. Left-multiplied onto the orthographic projection, this is a
// real camera zoom (geometry re-rasterizes crisply), not a bitmap scale.
function mat4ScaleXY(s: number): Mat4 {
  const m = mat4Identity();
  m[0] = s;
  m[5] = s;
  return m;
}
//=============================================================================
// Centroid of a decoded mesh in the space its model matrix consumes.
function meshCentroid(positions: ArrayLike<number> | undefined): [number, number, number] {
  if (!positions || positions.length < 3) return [0, 0, 0];
  let minX = Infinity;
  let minY = Infinity;
  let minZ = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;
  let maxZ = -Infinity;
  for (let i = 0; i + 2 < positions.length; i += 3) {
    const x = positions[i];
    const y = positions[i + 1];
    const z = positions[i + 2];
    if (x < minX) minX = x;
    if (y < minY) minY = y;
    if (z < minZ) minZ = z;
    if (x > maxX) maxX = x;
    if (y > maxY) maxY = y;
    if (z > maxZ) maxZ = z;
  }
  return [(minX + maxX) / 2, (minY + maxY) / 2, (minZ + maxZ) / 2];
}
//=============================================================================
export async function renderNelsonFigureScene(
  container: HTMLElement,
  scene: NelsonFigureScene
): Promise<SceneReplayHandle> {
  const width = Math.max(1, Math.round(scene.width));
  const height = Math.max(1, Math.round(scene.height));
  const dpr = scene.devicePixelRatio && scene.devicePixelRatio > 0 ? scene.devicePixelRatio : 1;
  const tooNew = scene.version > SUPPORTED_SCENE_VERSION;

  // A fixed-size stage keeps the figure at its authored pixel size; the wheel
  // zoom scales this stage, never the page around it.
  const stage = document.createElement('div');
  stage.style.position = 'relative';
  stage.style.width = `${width}px`;
  stage.style.height = `${height}px`;
  stage.style.overflow = 'hidden';
  stage.style.touchAction = 'none';
  if (scene.background) stage.style.background = scene.background;

  const sizeCanvas = (canvas: HTMLCanvasElement) => {
    canvas.width = Math.round(width * dpr);
    canvas.height = Math.round(height * dpr);
    canvas.style.position = 'absolute';
    canvas.style.left = '0';
    canvas.style.top = '0';
    canvas.style.width = `${width}px`;
    canvas.style.height = `${height}px`;
  };

  const meshes = scene.webglMeshes ?? [];
  const hasMeshes = meshes.length > 0 && !tooNew;

  // A 3D figure composites in three layers, as the live panel does: the opaque
  // back panes and grid on a base 2D canvas, the WebGL surface above them so it
  // hides the panes it should, and the text (titles and axis labels) on a top
  // canvas so it stays readable over the surface. A flat 2D figure uses the top
  // canvas alone.
  let baseCanvas: HTMLCanvasElement | null = null;
  let webglCanvas: HTMLCanvasElement | null = null;
  if (hasMeshes) {
    baseCanvas = document.createElement('canvas');
    sizeCanvas(baseCanvas);
    stage.appendChild(baseCanvas);
    webglCanvas = document.createElement('canvas');
    sizeCanvas(webglCanvas);
    stage.appendChild(webglCanvas);
  }
  const topCanvas = document.createElement('canvas');
  sizeCanvas(topCanvas);
  stage.appendChild(topCanvas);
  const ctx = topCanvas.getContext('2d');
  const baseCtx = baseCanvas ? baseCanvas.getContext('2d') : null;

  container.appendChild(stage);

  let webglRenderer: FigureWebGlRenderer | null = null;
  let decodedMeshes: FigureWebGlMesh[] = [];
  const baseModels: Mat4[] = [];
  const baseProjections: Mat4[] = [];
  const centroids: [number, number, number][] = [];
  if (webglCanvas) {
    webglRenderer = await createFigureWebGlRendererForCanvas(webglCanvas, 'webgl2');
    if (webglRenderer) {
      decodedMeshes = retainFigureWebGlGeometry(meshes, new Map());
      for (const mesh of decodedMeshes) {
        baseModels.push(mesh.model.slice());
        baseProjections.push(mesh.projection.slice());
        centroids.push(meshCentroid(mesh.positions as ArrayLike<number> | undefined));
      }
    }
  }

  // Scene v2 camera. When the scene carries axes3d geometry, one shared camera
  // orbits the whole axes (box, grid, panes, labels AND the WebGL surface) about
  // the data-box centre and dollies the orthographic projection, so the offline
  // figure rotates and zooms like the live engine instead of spinning only the
  // surface inside a frozen box. Absent axes3d, every path below stays v1.
  // One camera PER 3D axes. A figure can hold several (subplots), each with its
  // own base transform and viewport; the orbit/dolly drives only the axes under
  // the pointer, and every axes reprojects its meshes + decorations under its own
  // camera, so subplots rotate independently like the live engine.
  interface Axes3dCtx {
    entry: SceneAxes3dEntry;
    baseModel: Mat4;
    baseProj: Mat4;
    viewport: [number, number, number, number];
    // The orbit pivot in view/eye space (after the axes base model), so yaw/pitch
    // are screen-aligned. The same transform is applied to every piece of geometry.
    pivotView: [number, number, number, number];
    meshIdx: number[]; // indices into decodedMeshes that belong to this axes
    yaw: number;
    pitch: number;
    zoom: number;
  }
  const sameViewport = (a: readonly number[], b: readonly number[]): boolean =>
    a.length >= 4 &&
    b.length >= 4 &&
    a[0] === b[0] &&
    a[1] === b[1] &&
    a[2] === b[2] &&
    a[3] === b[3];
  const axes3dCtxs: Axes3dCtx[] = [];
  if (!tooNew && webglRenderer && Array.isArray(scene.axes3d)) {
    for (const entry of scene.axes3d) {
      if (
        !entry ||
        !Array.isArray(entry.model) ||
        !Array.isArray(entry.projection) ||
        !Array.isArray(entry.viewport)
      ) {
        continue;
      }
      const baseModel = entry.model.slice() as Mat4;
      const bc = entry.boxCentre ?? [0, 0, 0];
      const vp = entry.viewport as [number, number, number, number];
      const meshIdx: number[] = [];
      for (let i = 0; i < decodedMeshes.length; i++) {
        if (sameViewport(decodedMeshes[i].viewport, vp)) meshIdx.push(i);
      }
      axes3dCtxs.push({
        entry,
        baseModel,
        baseProj: entry.projection.slice() as Mat4,
        viewport: vp,
        pivotView: mat4ApplyToPoint(baseModel, bc[0], bc[1], bc[2]),
        meshIdx,
        yaw: 0,
        pitch: 0,
        zoom: 1,
      });
    }
  }
  const hasAxes3d = axes3dCtxs.length > 0;

  // When WebGL is unavailable, the baked raster carries the whole figure, so the
  // 2D display list is not drawn on top of a blank mesh layer.
  const useRasterFallback =
    (hasMeshes && !webglRenderer && !!scene.softwareFallbackImage) || tooNew;
  let fallbackImage: HTMLImageElement | null = null;
  if (useRasterFallback && scene.softwareFallbackImage) {
    fallbackImage = new Image();
    fallbackImage.src = scene.softwareFallbackImage;
    try {
      await fallbackImage.decode();
    } catch {
      fallbackImage = null;
    }
  }

  const images = useRasterFallback
    ? new Map<string, CachedImageSource>()
    : await buildImageCache(scene.primitives);

  // Interaction state. 2D pan slides the plot content; wheel zoom scales the
  // whole stage; 3D rotate orbits the meshes about their centroid.
  let panOffset: { dx: number; dy: number } | null = null;
  let zoom = 1;
  let yaw = 0;
  let pitch = 0;

  // The orbit delta in view space for one axes (about its box centre): screen-
  // aligned yaw/pitch, applied identically to that axes' geometry.
  const orbitDeltaFor = (c: Axes3dCtx): Mat4 => {
    const rot = mat4Multiply(mat4RotationY(c.yaw), mat4RotationX(c.pitch));
    return mat4Multiply(
      mat4Translation(c.pivotView[0], c.pivotView[1], c.pivotView[2]),
      mat4Multiply(rot, mat4Translation(-c.pivotView[0], -c.pivotView[1], -c.pivotView[2]))
    );
  };
  // Full data -> clip matrix for an axes' decorations: dolly * orbit * axesModel.
  const sceneMatrixFor = (c: Axes3dCtx): Mat4 =>
    mat4Multiply(
      mat4Multiply(mat4ScaleXY(c.zoom), c.baseProj),
      mat4Multiply(orbitDeltaFor(c), c.baseModel)
    );
  // Project a data point to canvas pixels through an axes' viewport (GL
  // bottom-origin -> canvas top-origin).
  const toCanvas = (
    full: Mat4,
    vp: [number, number, number, number],
    x: number,
    y: number,
    z: number
  ): { x: number; y: number } => {
    const clip = mat4ApplyToPoint(full, x, y, z);
    const w = clip[3] || 1e-9;
    return {
      x: vp[0] + (clip[0] / w) * 0.5 * vp[2] + 0.5 * vp[2],
      y: height - (vp[1] + (clip[1] / w) * 0.5 * vp[3] + 0.5 * vp[3]),
    };
  };
  const drawAxes3dSegments = (
    c2d: CanvasRenderingContext2D,
    full: Mat4,
    vp: [number, number, number, number],
    seg: SceneAxes3dSegments
  ) => {
    const s = seg.segments;
    if (s.length < 6) return;
    c2d.strokeStyle = seg.color ?? 'rgba(51,51,51,1)';
    c2d.lineWidth = seg.width ?? 1;
    c2d.beginPath();
    for (let i = 0; i + 5 < s.length; i += 6) {
      const a = toCanvas(full, vp, s[i], s[i + 1], s[i + 2]);
      const b = toCanvas(full, vp, s[i + 3], s[i + 4], s[i + 5]);
      c2d.moveTo(a.x, a.y);
      c2d.lineTo(b.x, b.y);
    }
    c2d.stroke();
  };
  const drawAxes3dFaces = (
    c2d: CanvasRenderingContext2D,
    full: Mat4,
    vp: [number, number, number, number],
    faces: SceneAxes3dFaces
  ) => {
    c2d.fillStyle = faces.color ?? 'rgba(255,255,255,1)';
    let o = 0;
    for (const n of faces.sizes) {
      if (n < 3) {
        o += n * 3;
        continue;
      }
      c2d.beginPath();
      for (let k = 0; k < n; k++) {
        const p = toCanvas(
          full,
          vp,
          faces.positions[o],
          faces.positions[o + 1],
          faces.positions[o + 2]
        );
        o += 3;
        if (k === 0) c2d.moveTo(p.x, p.y);
        else c2d.lineTo(p.x, p.y);
      }
      c2d.closePath();
      c2d.fill();
    }
  };
  const drawAxes3dLabels = (
    c2d: CanvasRenderingContext2D,
    full: Mat4,
    vp: [number, number, number, number],
    labels: SceneAxes3dLabel[]
  ) => {
    for (const label of labels) {
      const p = toCanvas(full, vp, label.pos[0], label.pos[1], label.pos[2]);
      c2d.save();
      c2d.fillStyle = label.color ?? 'rgba(0,0,0,1)';
      const weight = `${label.italic ? 'italic ' : ''}${label.bold ? 'bold ' : ''}`;
      c2d.font = `${weight}${label.size ?? 11}px ${label.font ?? 'sans-serif'}`;
      c2d.textAlign = label.align ?? 'center';
      c2d.textBaseline = label.baseline ?? 'middle';
      if (label.rotation) {
        c2d.translate(p.x, p.y);
        c2d.rotate((-label.rotation * Math.PI) / 180);
        c2d.fillText(label.text, 0, 0);
      } else {
        c2d.fillText(label.text, p.x, p.y);
      }
      c2d.restore();
    }
  };
  // Reproject every 3D axes under its own camera: panes, grid and box on the base
  // canvas (below the surface), labels on the top canvas (above it). The static 2D
  // primitives the exporter kept (background and non-rotating decorations such as
  // the title) are painted once; only the axes content moves with the camera.
  const paintAxes3dScene = () => {
    if (!ctx || !baseCtx) return;
    paintFigure2D(baseCtx, scene.primitives, width, height, dpr, images, null);
    baseCtx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, width, height);
    for (const c of axes3dCtxs) {
      const full = sceneMatrixFor(c);
      if (c.entry.panes) drawAxes3dFaces(baseCtx, full, c.viewport, c.entry.panes);
      if (c.entry.grid) drawAxes3dSegments(baseCtx, full, c.viewport, c.entry.grid);
      if (c.entry.box) drawAxes3dSegments(baseCtx, full, c.viewport, c.entry.box);
      if (c.entry.labels) drawAxes3dLabels(ctx, full, c.viewport, c.entry.labels);
    }
  };

  const paint2d = () => {
    if (!ctx) return;
    if (useRasterFallback) {
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      ctx.clearRect(0, 0, width, height);
      if (fallbackImage) ctx.drawImage(fallbackImage, 0, 0, width, height);
      return;
    }
    if (hasAxes3d) {
      // Scene v2: the 3D axes is reprojected under the shared camera every frame.
      paintAxes3dScene();
      return;
    }
    if (webglRenderer && baseCtx) {
      // Three-layer 3D compositing: panes and grid below the surface, text above.
      paintFigure2D(
        baseCtx,
        figureBaseLayerPrimitives(scene.primitives),
        width,
        height,
        dpr,
        images,
        null
      );
      paintFigure2D(
        ctx,
        figureTextOverlayPrimitives(scene.primitives),
        width,
        height,
        dpr,
        images,
        null
      );
      return;
    }
    paintFigure2D(ctx, scene.primitives, width, height, dpr, images, panOffset);
  };

  const renderMeshes = () => {
    if (!webglRenderer || decodedMeshes.length === 0) return;
    if (hasAxes3d) {
      // Per-axes camera: each axes' orbit/dolly moves its own meshes together with
      // its box, grid and labels, so subplots rotate independently and coherently.
      for (const c of axes3dCtxs) {
        const orbit = orbitDeltaFor(c);
        const dolly = mat4ScaleXY(c.zoom);
        for (const i of c.meshIdx) {
          decodedMeshes[i].model = mat4Multiply(orbit, baseModels[i]);
          decodedMeshes[i].projection = mat4Multiply(dolly, baseProjections[i]);
        }
      }
      webglRenderer.render(decodedMeshes, width, height, dpr);
      return;
    }
    for (let i = 0; i < decodedMeshes.length; i++) {
      const [cx, cy, cz] = centroids[i];
      const orbit = mat4Multiply(
        mat4Translation(cx, cy, cz),
        mat4Multiply(mat4RotationY(yaw), mat4Translation(-cx, -cy, -cz))
      );
      const tilt = mat4Multiply(
        mat4Translation(cx, cy, cz),
        mat4Multiply(mat4RotationX(pitch), mat4Translation(-cx, -cy, -cz))
      );
      decodedMeshes[i].model = mat4Multiply(baseModels[i], mat4Multiply(orbit, tilt));
    }
    webglRenderer.render(decodedMeshes, width, height, dpr);
  };

  const applyZoom = () => {
    // Scene v2 zoom is a camera dolly drawn into the canvas, not a CSS scale of
    // the stage bitmap, so the stage transform stays identity.
    if (hasAxes3d) return;
    stage.style.transform = zoom === 1 ? '' : `scale(${zoom})`;
    stage.style.transformOrigin = 'center center';
  };

  const firstAxes = scene.rotatableAxes?.find((axis) => axis.interactive !== false);
  const canRotate =
    hasAxes3d || (hasMeshes && !!webglRenderer && (firstAxes?.is3d ?? scene.is3d ?? false));
  const canPan = !hasMeshes && !useRasterFallback && !!firstAxes && !(firstAxes.is3d ?? false);

  // Pointer gestures. 3D figures rotate, flat 2D figures pan; both reset on a
  // double click. Wheel zoom is image-grade and works in every case.
  let dragging = false;
  let startX = 0;
  let startY = 0;
  let baseYaw = 0;
  let basePitch = 0;
  let basePan = { dx: 0, dy: 0 };
  // The axes being dragged, and a hit-test mapping a pointer to the axes whose
  // viewport rect (canvas, top-origin) contains it.
  let activeCtx: Axes3dCtx | null = null;
  const axesAt = (clientX: number, clientY: number): Axes3dCtx | null => {
    const rect = stage.getBoundingClientRect();
    const sx = rect.width > 0 ? width / rect.width : 1;
    const sy = rect.height > 0 ? height / rect.height : 1;
    const px = (clientX - rect.left) * sx;
    const py = (clientY - rect.top) * sy;
    for (const c of axes3dCtxs) {
      const [vx, vy, vw, vh] = c.viewport;
      const top = height - (vy + vh);
      const bottom = height - vy;
      if (px >= vx && px <= vx + vw && py >= top && py <= bottom) return c;
    }
    return null;
  };

  const onPointerDown = (event: PointerEvent) => {
    if (!canRotate && !canPan) return;
    dragging = true;
    startX = event.clientX;
    startY = event.clientY;
    if (hasAxes3d) {
      // Drag the axes under the pointer; fall back to the first when outside any.
      activeCtx = axesAt(event.clientX, event.clientY) ?? axes3dCtxs[0] ?? null;
      if (activeCtx) {
        baseYaw = activeCtx.yaw;
        basePitch = activeCtx.pitch;
      }
    } else {
      baseYaw = yaw;
      basePitch = pitch;
    }
    basePan = panOffset ? { ...panOffset } : { dx: 0, dy: 0 };
    stage.setPointerCapture(event.pointerId);
  };
  const onPointerMove = (event: PointerEvent) => {
    if (!dragging) return;
    if (hasAxes3d) {
      if (!activeCtx) return;
      const dx = (event.clientX - startX) / activeCtx.zoom;
      const dy = (event.clientY - startY) / activeCtx.zoom;
      activeCtx.yaw = baseYaw + dx * 0.01;
      activeCtx.pitch = basePitch + dy * 0.01;
      // Reproject that axes' box, grid, labels and meshes under its own camera.
      renderMeshes();
      paint2d();
      return;
    }
    const dx = (event.clientX - startX) / zoom;
    const dy = (event.clientY - startY) / zoom;
    if (canRotate) {
      yaw = baseYaw + dx * 0.01;
      pitch = basePitch + dy * 0.01;
      renderMeshes();
    } else if (canPan) {
      panOffset = { dx: basePan.dx + dx, dy: basePan.dy + dy };
      paint2d();
    }
  };
  const onPointerUp = (event: PointerEvent) => {
    dragging = false;
    try {
      stage.releasePointerCapture(event.pointerId);
    } catch {
      /* capture may already be gone */
    }
  };
  const onWheel = (event: WheelEvent) => {
    event.preventDefault();
    const factor = event.deltaY < 0 ? 1.1 : 1 / 1.1;
    if (hasAxes3d) {
      // Real camera dolly on the axes under the pointer: scale its orthographic
      // projection about its viewport centre and redraw at the new scale.
      const c = axesAt(event.clientX, event.clientY) ?? axes3dCtxs[0];
      if (c) {
        c.zoom = Math.min(8, Math.max(0.2, c.zoom * factor));
        renderMeshes();
        paint2d();
      }
      return;
    }
    zoom = Math.min(8, Math.max(1, zoom * factor));
    applyZoom();
  };
  const onDoubleClick = (event: MouseEvent) => {
    if (hasAxes3d) {
      // Reset the axes under the pointer (or all, when outside any) to export view.
      const hit = axesAt(event.clientX, event.clientY);
      const targets = hit ? [hit] : axes3dCtxs;
      for (const c of targets) {
        c.yaw = 0;
        c.pitch = 0;
        c.zoom = 1;
      }
      paint2d();
      renderMeshes();
      return;
    }
    panOffset = null;
    zoom = 1;
    yaw = 0;
    pitch = 0;
    applyZoom();
    paint2d();
    renderMeshes();
  };

  stage.addEventListener('pointerdown', onPointerDown);
  stage.addEventListener('pointermove', onPointerMove);
  stage.addEventListener('pointerup', onPointerUp);
  stage.addEventListener('pointercancel', onPointerUp);
  stage.addEventListener('wheel', onWheel, { passive: false });
  stage.addEventListener('dblclick', onDoubleClick);
  if (canRotate || canPan) stage.style.cursor = 'grab';

  paint2d();
  renderMeshes();

  return {
    destroy() {
      stage.removeEventListener('pointerdown', onPointerDown);
      stage.removeEventListener('pointermove', onPointerMove);
      stage.removeEventListener('pointerup', onPointerUp);
      stage.removeEventListener('pointercancel', onPointerUp);
      stage.removeEventListener('wheel', onWheel);
      stage.removeEventListener('dblclick', onDoubleClick);
      webglRenderer?.destroy();
      if (stage.parentNode === container) container.removeChild(stage);
    },
  };
}
//=============================================================================
