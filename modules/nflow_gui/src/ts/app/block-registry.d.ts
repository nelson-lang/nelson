//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
declare module './block-registry.js' {
  export type BlockDefinition = {
    type?: string;
    width?: number;
    height?: number;
    inputs?: any[];
    outputs?: any[];
    defaultParams?: Record<string, unknown> | null;
    render?: (block: any, helpers: any) => any;
    resize?: (block: any, helpers: any) => any;
    physicalPins?: any[];
    acausal?: boolean;
    domain?: string;
    createTemplate?: (helpers: any) => any;
  };

  export type BlockDefinitionsMap = Record<string, BlockDefinition>;

  export class BlockRegistry {
    constructor(initial?: BlockDefinitionsMap);
    register(def: BlockDefinition): void;
    registerMany(map: BlockDefinitionsMap): void;
    getDefinitions(): BlockDefinitionsMap;
  }

  export default BlockRegistry;
}
