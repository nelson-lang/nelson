//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
export type BlockDefinition = {
  type?: string;
  width?: number;
  height?: number;
  inputs?: any[];
  outputs?: any[];
  defaultParams?: Record<string, unknown> | null;
  render?: (block: any, helpers: any) => any;
  resize?: (block: any, helpers: any) => any;
  // Acausal (physical) blocks: undirected pins + domain, carried from the
  // manifest so the renderer can draw connectable 'phys' ports.
  physicalPins?: any[];
  acausal?: boolean;
  domain?: string;
  // legacy: allow modules to provide a createTemplate-like factory
  createTemplate?: (helpers: any) => any;
};
//=============================================================================
export type BlockDefinitionsMap = Record<string, BlockDefinition>;
export class BlockRegistry {
  private defs: BlockDefinitionsMap;
  constructor(initial?: BlockDefinitionsMap) {
    this.defs = {};
    if (initial) this.registerMany(initial);
  }
  //=============================================================================
  register(def: BlockDefinition) {
    const type = def.type;
    if (!type) throw new Error('BlockDefinition.type is required');
    this.defs[type] = def;
  }
  //=============================================================================
  registerMany(map: BlockDefinitionsMap) {
    for (const [k, v] of Object.entries(map)) {
      if (!v) continue;
      if (!v.type) v.type = k;
      this.defs[v.type] = v;
    }
  }
  //=============================================================================
  getDefinitions(): BlockDefinitionsMap {
    return { ...this.defs };
  }
}
//=============================================================================
export default BlockRegistry;
//=============================================================================
