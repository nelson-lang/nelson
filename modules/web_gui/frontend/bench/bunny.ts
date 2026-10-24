//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Dev-only benchmark harness: drives the real figure WebGL2 and WebGPU
// renderers with an engine-produced Stanford bunny display list and compares
// the per-frame CPU cost. Not part of the app bundle. Safe to delete.
import { retainFigureDisplayListWebGlGeometry } from '../src/panels/figureWebGlGeometry';
import {
  createFigureWebGlRendererForCanvas,
  type FigureWebGlBackend,
} from '../src/panels/figureWebGlRenderer';
import type { FigureDisplayListResult, FigureWebGlMesh } from '../src/rpc/types';
//=============================================================================
const logEl = document.getElementById('log') as HTMLPreElement;
logEl.textContent = '';
const log = (line: string): void => {
  logEl.textContent += `${line}\n`;
};
//=============================================================================
function decodeMeshes(dl: FigureDisplayListResult): FigureWebGlMesh[] {
  const retained = new Map();
  return retainFigureDisplayListWebGlGeometry(dl, retained).webglMeshes ?? [];
}
//=============================================================================
function median(values: number[]): number {
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
}
//=============================================================================
interface BenchResult {
  actual: string;
  drawMs: number;
  uploadMs: number;
}
//=============================================================================
async function bench(
  backend: Extract<FigureWebGlBackend, 'webgl2' | 'webgpu'>,
  canvas: HTMLCanvasElement,
  dl: FigureDisplayListResult
): Promise<BenchResult | null> {
  const meshes = decodeMeshes(dl);
  const width = dl.width;
  const height = dl.height;
  const dpr = 1;
  const renderer = await createFigureWebGlRendererForCanvas(canvas, backend);
  if (!renderer) {
    log(`${backend}: renderer unavailable`);
    return null;
  }
  for (let i = 0; i < 15; i++) renderer.render(meshes, width, height, dpr, true);

  // Phase A: steady-state draw submission (buffers cached). Amplified ×R per
  // frame so the per-call cost clears the performance.now() resolution floor.
  const repeats = 25;
  const batches = 30;
  const framesPerBatch = 4;
  const drawSamples: number[] = [];
  for (let b = 0; b < batches; b++) {
    const start = performance.now();
    for (let i = 0; i < framesPerBatch; i++) {
      for (let r = 0; r < repeats; r++) renderer.render(meshes, width, height, dpr, false);
    }
    drawSamples.push((performance.now() - start) / (framesPerBatch * repeats));
  }
  const drawMs = median(drawSamples);

  // Phase B: re-upload the geometry every frame (new revision forces the buffer
  // rebuild path), modelling an animation/streaming workload.
  const uploadSamples: number[] = [];
  let revision = 1000;
  for (let b = 0; b < batches; b++) {
    const start = performance.now();
    for (let i = 0; i < framesPerBatch; i++) {
      revision++;
      const churned = meshes.map((mesh) => ({ ...mesh, revision: `bench-${revision}` }));
      renderer.render(churned, width, height, dpr, false);
    }
    uploadSamples.push((performance.now() - start) / framesPerBatch);
  }
  const uploadMs = median(uploadSamples);

  // Leave a correct frame on screen for the visual parity check.
  renderer.render(meshes, width, height, dpr, true);
  log(
    `${backend} → actual=${renderer.backend}: draw-submit ${drawMs.toFixed(
      4
    )} ms/draw  |  re-upload ${uploadMs.toFixed(3)} ms/frame`
  );
  return { actual: renderer.backend, drawMs, uploadMs };
}
//=============================================================================
(async () => {
  try {
    const dataFile =
      new URLSearchParams(window.location.search).get('data') ?? 'bunny-displaylist.json';
    log(`dataset: ${dataFile}`);
    const response = await fetch(`./${dataFile}`);
    const dl = (await response.json()) as FigureDisplayListResult;
    const meshes = dl.webglMeshes ?? [];
    const triangles = meshes.reduce((sum, m) => sum + (m.triangleIndicesLength ?? 0) / 3, 0);
    const lines = meshes.reduce((sum, m) => sum + (m.lineIndicesLength ?? 0) / 2, 0);
    log(
      `loaded ${dl.width}×${dl.height}, meshes=${meshes.length}, ~${triangles | 0} triangles, ~${
        lines | 0
      } line segments\n`
    );
    const gl = await bench('webgl2', document.getElementById('gl') as HTMLCanvasElement, dl);
    const gpu = await bench('webgpu', document.getElementById('gpu') as HTMLCanvasElement, dl);
    if (gl && gpu && gpu.actual === 'webgpu') {
      const drawRatio = gl.drawMs / gpu.drawMs;
      const uploadRatio = gl.uploadMs / gpu.uploadMs;
      log(
        `\nCPU main-thread (higher ratio = WebGPU cheaper):` +
          `\n  draw-submit  webgl2/webgpu = ${drawRatio.toFixed(2)}×` +
          `\n  re-upload    webgl2/webgpu = ${uploadRatio.toFixed(2)}×`
      );
    } else if (gpu && gpu.actual !== 'webgpu') {
      log(`\nWebGPU unavailable (fell back to ${gpu.actual}); no comparison.`);
    }
    (window as unknown as { __benchDone?: boolean }).__benchDone = true;
  } catch (error) {
    log(`FAILED: ${String(error)}`);
    (window as unknown as { __benchError?: string }).__benchError = String(error);
  }
})();
//=============================================================================
