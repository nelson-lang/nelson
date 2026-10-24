//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { fmuRenderer, fmuResizer } from './fmu.js';
import { createBlockRenderer } from '../generic-block-renderer.js';
import type { RenderDescriptor } from '../block-descriptor.js';
//=============================================================================
// Keeps `new URL(rel, base)` resolved at runtime instead of being rewritten by
// Vite's build-time asset analysis.
const MODULE_URL: string = import.meta.url;
// The Modelica bridge block is an icon node (its ports come from the manifest);
// sim() compiles it to an FMU. It reuses the generic image renderer with the icon
// src resolved to the fmi library directory.
function modelicaRenderer() {
  const desc = { type: 'image', src: 'modelica.svg' } as RenderDescriptor & { _src?: string };
  try {
    const base = new URL('../libraries/fmi/', MODULE_URL);
    desc._src = new URL('modelica.svg', base).href;
  } catch {
    // fall back to the plain src; createBlockRenderer also probes exports/
  }
  return createBlockRenderer(desc, 'modelica', 'fmi');
}
//=============================================================================
export const blockDefinitions = {
  modelica: { render: modelicaRenderer() },
  fmu: { render: fmuRenderer, resize: fmuResizer },
  // Model Exchange shares the FMU renderer and resizer (ports follow params.numInputs / numOutputs).
  fmuMe: { render: fmuRenderer, resize: fmuResizer },
};
//=============================================================================
