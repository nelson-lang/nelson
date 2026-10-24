// ============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================

export declare function discoverBuiltinLibraries(): Promise<unknown[]>;
export declare function discoverFromPath(dirUrl: string): Promise<unknown[]>;
export declare function loadLibrary(id: string): Promise<unknown>;
export declare function loadAllDiscovered(): Promise<void>;
export declare function unloadLibrary(id: string): void;
export declare function getLoadedLibraries(): unknown[];
export declare function getDiscoveredManifests(): unknown[];
export declare function isLibraryLoaded(id: string): boolean;
export declare function getMissingBlockTypes(
  types: string[]
): { type: string; libraryId?: string; libraryTitle?: string }[];
export declare function buildBlockLibrary(): unknown;
export declare function buildAllBlockTemplates(helpers: unknown): unknown;
export declare function getSearchPaths(): string[];
export declare function setSearchPaths(paths: string[]): void;
export declare function addSearchPath(path: string): void;
export declare function initLibraries(): Promise<void>;
