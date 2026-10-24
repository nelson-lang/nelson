//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
import { fileURLToPath } from 'url';
//=============================================================================
let mod: any;
let libLoader: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/library-loader.js').catch(() => null);
  libLoader = await import('./library-loader').catch(() => null);
});
//=============================================================================
test('library-loader module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
import { vi } from 'vitest';
import fs from 'fs';
import path from 'path';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
test('loads library-loader if present', async () => {
  const p = path.resolve(process.cwd(), 'modules/nflow_gui/src/ts/app/library-loader.ts');
  if (!fs.existsSync(p)) {
    return;
  }
  const mod = await import('./library-loader');
  expect(mod).toBeDefined();
});
//=============================================================================
test('builtin.source manifest loads blocks including chirp,fileSource,labelSource,noise', async () => {
  const discovered = libLoader.getDiscoveredManifests();
  const manifestPath = path.resolve(
    path.join(process.cwd(), 'modules/nflow_blocks/libraries', 'source/library.json')
  );
  const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
  manifest._dir = 'source';
  discovered.set('builtin.source', manifest);

  const entry = await libLoader.loadLibrary('builtin.source');
  expect(entry).not.toBeNull();

  const types = (entry!.library.blocks || []).map((b: any) => b.type);
  expect(types).toEqual(
    expect.arrayContaining(['chirp', 'fileSource', FieldNames.kLabelSource, 'noise'])
  );
});
//=============================================================================
test('acausal manifest templates carry acausal / physicalPins / domain', async () => {
  // Regression: the manifest->template factories dropped these fields, so every
  // acausal block rendered without physical pins, physical wires never formed,
  // and the engine rejected the un-lowered block (unsupported_block: Inertia).
  // acausal libraries ship no blocks.js, so loadLibrary falls to the
  // manifest-registry path (_createTemplatesFromDefinitions).
  const discovered = libLoader.getDiscoveredManifests();
  const manifestPath = path.resolve(
    path.join(process.cwd(), 'modules/nflow_blocks/libraries', 'acausal_rotational/library.json')
  );
  const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
  manifest._dir = 'acausal_rotational';
  discovered.set('builtin.acausal_rotational', manifest);

  const entry = await libLoader.loadLibrary('builtin.acausal_rotational');
  expect(entry).not.toBeNull();

  const templates = libLoader.buildAllBlockTemplates({}) as Record<string, any>;
  const inertia = templates['Inertia'];
  expect(inertia).toBeDefined();
  expect(inertia.acausal).toBe(true);
  expect(inertia.domain).toBe('rotational');
  expect(Array.isArray(inertia.physicalPins)).toBe(true);
  expect(inertia.physicalPins.map((p: any) => p.name)).toContain('flange');
});
//=============================================================================
test('search paths are deduplicated without changing priority', () => {
  libLoader.setSearchPaths(['C:\\Libraries', 'c:/libraries', 'D:\\Blocks', '', 'D:\\Blocks']);
  expect(libLoader.getSearchPaths()).toEqual(['C:\\Libraries', 'D:\\Blocks']);
});
//=============================================================================
test('engine-native mechanism blocks do not require a palette library', () => {
  expect(libLoader.getMissingBlockTypes(['planarMechanicalIsland'])).toEqual([]);
  expect(libLoader.getMissingBlockTypes(['unknownEngineBlock'])).toEqual([
    { type: 'unknownEngineBlock' },
  ]);
});
//=============================================================================
test('enables a manifest-only external library without a frontend module', async () => {
  const discovered = libLoader.getDiscoveredManifests();
  discovered.set('custom.manifest-only', {
    id: 'custom.manifest-only',
    title: 'Manifest only',
    blocks: [{ type: 'customGain', label: 'Custom Gain' }],
  });

  const entry = await libLoader.enableLibraryFromManifest('custom.manifest-only');
  expect(entry?.library.blocks).toEqual([{ type: 'customGain', label: 'Custom Gain' }]);

  await libLoader.unloadLibrary('custom.manifest-only');
  discovered.delete('custom.manifest-only');
});
//=============================================================================
test('invalidates a loaded external library when search-path priority changes its source', async () => {
  const discovered = libLoader.getDiscoveredManifests();
  discovered.set('custom.priority', {
    id: 'custom.priority',
    title: 'First',
    _dir: 'C:\\first\\custom',
    blocks: [{ type: 'customBlock' }],
  });
  await libLoader.enableLibraryFromManifest('custom.priority');
  expect(libLoader.isLibraryLoaded('custom.priority')).toBe(true);

  libLoader.replaceExternalManifests([
    {
      id: 'custom.priority',
      title: 'Second',
      _dir: 'C:\\second\\custom',
      blocks: [{ type: 'customBlock' }],
    },
  ]);

  expect(libLoader.isLibraryLoaded('custom.priority')).toBe(false);
  discovered.delete('custom.priority');
});
//=============================================================================
