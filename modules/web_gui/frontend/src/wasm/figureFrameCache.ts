//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { FigureDisplayListResult, FigureWebGlMesh } from '../rpc/types';
//=============================================================================
// A compact matrix-only animation frame (WebGL view change) carries the mesh
// matrices and the figure size only: no primitives, uicontrols, menus or layout
// signature. The desktop keeps the latest frame as the figure's display list
// (figure.getDisplayList and replies sent while the interpreter is busy), so
// caching the compact frame as is would drop the controls of a running demo.
// The compact frame is merged into the last complete one instead.
export function mergeMatrixOnlyFrame(
  complete: FigureDisplayListResult,
  frame: FigureDisplayListResult
): FigureDisplayListResult {
  const updates = new Map<number, FigureWebGlMesh>();
  for (const mesh of frame.webglMeshes ?? []) updates.set(mesh.id, mesh);
  const webglMeshes = complete.webglMeshes?.map((mesh) => {
    const update = updates.get(mesh.id);
    return update ? { ...mesh, ...update } : mesh;
  });
  const { matrixOnly: _matrixOnly, ...rest } = complete;
  return {
    ...rest,
    width: frame.width,
    height: frame.height,
    ...(webglMeshes ? { webglMeshes } : {}),
  };
}
//=============================================================================
export function isMatrixOnlyFrame(frame: FigureDisplayListResult): boolean {
  return frame.matrixOnly === true;
}
//=============================================================================
