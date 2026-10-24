//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Dynamic-port lookup blocks: lookupND and directLookup take one input per
// table dimension, so they morph their input ports on NumberOfTableDimensions.
// The image renderer is reused from the manifest so the palette icon is
// preserved (the module registry overrides the manifest entry).
/* eslint-disable @typescript-eslint/no-explicit-any */
import lookupManifest from '../../../../../nflow_blocks/libraries/lookup/library.json';
import { buildRegistryFromManifest } from '../generic-block-renderer.js';
//=============================================================================
const WIDTH = 90;
const HEIGHT = 80;
function dimInputsResizer(blockDef: any): void {
  const n = Math.max(
    1,
    Math.min(30, parseInt(String(blockDef.params?.NumberOfTableDimensions ?? 2), 10) || 2)
  );
  blockDef.width = WIDTH;
  blockDef.height = Math.max(HEIGHT, 20 * (n + 1));
  const h = blockDef.height;
  blockDef.dynamicInputs = Array.from({ length: n }, (_, i) => ({
    x: 0,
    y: h / 2 + (i - (n - 1) / 2) * 20,
    side: 'left',
  }));
  blockDef.dynamicOutputs = [{ x: WIDTH, y: h / 2, side: 'right' }];
}
//=============================================================================
export const lookupNDResizer = dimInputsResizer;
export const directLookupResizer = dimInputsResizer;
const manifestDefs = buildRegistryFromManifest(lookupManifest as any).getDefinitions();
export const blockDefinitions = {
  lookupND: {
    render: manifestDefs['lookupND']?.render,
    resize: lookupNDResizer,
  },
  directLookup: {
    render: manifestDefs['directLookup']?.render,
    resize: directLookupResizer,
  },
};
//=============================================================================
