//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Manifests are data owned by the nflow_blocks module (renderers stay here).
import sourceManifest from '../../../../nflow_blocks/libraries/source/library.json';
import continuousManifest from '../../../../nflow_blocks/libraries/continuous/library.json';
import discreteManifest from '../../../../nflow_blocks/libraries/discrete/library.json';
import logicManifest from '../../../../nflow_blocks/libraries/logic/library.json';
import mathManifest from '../../../../nflow_blocks/libraries/math/library.json';
import lookupManifest from '../../../../nflow_blocks/libraries/lookup/library.json';
import nonlinearManifest from '../../../../nflow_blocks/libraries/nonlinear/library.json';
import sinkManifest from '../../../../nflow_blocks/libraries/sink/library.json';
import utilityManifest from '../../../../nflow_blocks/libraries/utility/library.json';
import fmiManifest from '../../../../nflow_blocks/libraries/fmi/library.json';
import userdefinedManifest from '../../../../nflow_blocks/libraries/userdefined/library.json';
import dashboardManifest from '../../../../nflow_blocks/libraries/dashboard/library.json';
import { buildRegistryFromManifest, mergeRegistries } from '../libraries/generic-block-renderer.js';
import type { LibraryManifest } from '../libraries/block-descriptor.js';
import * as sourceBlocks from '../libraries/source/blocks.js';
import * as sinkBlocks from '../libraries/sink/blocks.js';
import * as utilityBlocks from '../libraries/utility/blocks.js';
import * as fmiBlocks from '../libraries/fmi/blocks.js';
import * as mathBlocks from '../libraries/math/blocks.js';
import * as lookupBlocks from '../libraries/lookup/blocks.js';
import * as dashboardBlocks from '../libraries/dashboard/blocks.js';
import BlockRegistry, { BlockDefinition } from './block-registry.js';
//=============================================================================
const manifestRegistries = [
  buildRegistryFromManifest(sourceManifest as LibraryManifest),
  buildRegistryFromManifest(continuousManifest as LibraryManifest),
  buildRegistryFromManifest(discreteManifest as LibraryManifest),
  buildRegistryFromManifest(logicManifest as LibraryManifest),
  buildRegistryFromManifest(mathManifest as LibraryManifest),
  buildRegistryFromManifest(lookupManifest as LibraryManifest),
  buildRegistryFromManifest(nonlinearManifest as LibraryManifest),
  buildRegistryFromManifest(sinkManifest as LibraryManifest),
  buildRegistryFromManifest(utilityManifest as LibraryManifest),
  buildRegistryFromManifest(fmiManifest as LibraryManifest),
  buildRegistryFromManifest(userdefinedManifest as LibraryManifest),
  buildRegistryFromManifest(dashboardManifest as LibraryManifest),
];
//=============================================================================
const manifestRegistry = mergeRegistries(...manifestRegistries);
// Prefer module renderers for blocks that need richer DOM/behavior.
// Collect `blockDefinitions` exported by library-level `blocks.ts` modules.
type RendererFunction = (...args: unknown[]) => unknown;
type LibDefs = Record<string, Partial<BlockDefinition> | RendererFunction>;
interface LibraryModule {
  blockDefinitions?: LibDefs;
  blockRegistry?: LibDefs;
  renderers?: LibDefs;
  default?: { blockDefinitions?: LibDefs; blockRegistry?: LibDefs; renderers?: LibDefs };
}
//=============================================================================
const moduleDefs: Record<string, BlockDefinition> = {};
const manifestDefs = manifestRegistry.getDefinitions();
const collectDefs = (mod?: LibraryModule) => {
  if (!mod) return;
  let defs: LibDefs | undefined = mod.blockDefinitions ?? mod.blockRegistry ?? mod.renderers;
  if (!defs && mod.default)
    defs = mod.default.blockDefinitions ?? mod.default.blockRegistry ?? mod.default.renderers;
  if (!defs) return;

  for (const [k, v] of Object.entries(defs)) {
    if (!v) continue;

    if (typeof v === 'function') {
      const def: BlockDefinition = {
        type: k,
        width: manifestDefs[k]?.width,
        height: manifestDefs[k]?.height,
        inputs: manifestDefs[k]?.inputs,
        outputs: manifestDefs[k]?.outputs,
        defaultParams: manifestDefs[k]?.defaultParams,
        render: v as RendererFunction,
        resize: undefined,
        createTemplate: undefined,
      };
      moduleDefs[def.type as string] = def;
      continue;
    }

    const obj = v as Partial<BlockDefinition>;
    const def: BlockDefinition = {
      type: (obj.type as string) || k,
      width: obj.width ?? manifestDefs[k]?.width,
      height: obj.height ?? manifestDefs[k]?.height,
      inputs: obj.inputs ?? manifestDefs[k]?.inputs,
      outputs: obj.outputs ?? manifestDefs[k]?.outputs,
      defaultParams: obj.defaultParams ?? manifestDefs[k]?.defaultParams,
      render: obj.render ?? undefined,
      resize: obj.resize ?? undefined,
      createTemplate: obj.createTemplate ?? undefined,
    };
    moduleDefs[def.type as string] = def;
  }
};
//=============================================================================
collectDefs(sourceBlocks);
collectDefs(sinkBlocks);
collectDefs(utilityBlocks);
collectDefs(fmiBlocks);
collectDefs(mathBlocks);
collectDefs(lookupBlocks);
collectDefs(dashboardBlocks);
const moduleRegistry = new BlockRegistry(moduleDefs);
export const builtinRegistry = mergeRegistries(manifestRegistry, moduleRegistry);
export default builtinRegistry;
//=============================================================================
