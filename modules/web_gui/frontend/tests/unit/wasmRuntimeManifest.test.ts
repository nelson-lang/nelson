//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import {
  loadExpectedWasmBytes,
  loadRuntimeCapabilities,
  parseBrowserManifest,
  parseRuntimeCapabilities,
  startupDownloadPlan,
  wasmBytesFromManifest,
} from '../../src/wasm/runtimeManifest';
//=============================================================================
describe('WebAssembly deployment manifest', () => {
  const manifestUrl = 'https://example.test/releases/v1/manifest.json';
  const wasmUrl = 'https://example.test/releases/v1/nelson-portable.wasm';

  it('finds the decoded WebAssembly byte size', () => {
    expect(
      wasmBytesFromManifest(
        {
          files: [
            { file: 'index.html', bytes: 100 },
            { file: 'nelson-portable.wasm', bytes: 36_000_000 },
          ],
        },
        manifestUrl,
        wasmUrl
      )
    ).toBe(36_000_000);
  });

  it('falls back when manifest metadata is unavailable', async () => {
    const fetchImpl = vi.fn(async () => new Response('', { status: 404 }));
    await expect(loadExpectedWasmBytes(manifestUrl, wasmUrl, fetchImpl)).resolves.toBeUndefined();
  });

  it('derives startup and deferred byte budgets from the delivery plan', () => {
    const manifest = parseBrowserManifest({
      formatVersion: 1,
      files: [
        { file: 'index.html', bytes: 1_000 },
        { file: 'nelson-portable.wasm', bytes: 36_000_000 },
        { file: 'nelson-portable.data', bytes: 7_000_000 },
        { file: 'nflow/app.js', bytes: 2_000_000 },
      ],
      delivery: {
        formatVersion: 1,
        startup: [
          {
            id: 'engine',
            mode: 'startup',
            files: ['nelson-portable.wasm'],
            bytes: 36_000_000,
          },
          {
            id: 'resources',
            mode: 'startup',
            files: ['nelson-portable.data'],
            bytes: 7_000_000,
          },
          { id: 'shell', mode: 'startup', files: ['index.html'], bytes: 1_000 },
        ],
        onDemand: [
          {
            id: 'nflow',
            mode: 'on-demand',
            trigger: 'nflow',
            files: ['nflow/app.js'],
            bytes: 2_000_000,
          },
        ],
      },
    });

    expect(startupDownloadPlan(manifest, manifestUrl, wasmUrl)).toEqual({
      wasmBytes: 36_000_000,
      resourceBytes: 7_000_000,
      startupBytes: 43_001_000,
      onDemandBytes: 2_000_000,
    });
  });

  it('rejects a delivery plan that omits a packaged file', () => {
    expect(() =>
      parseBrowserManifest({
        formatVersion: 1,
        files: [
          { file: 'index.html', bytes: 1_000 },
          { file: 'nelson-portable.wasm', bytes: 36_000_000 },
        ],
        delivery: {
          formatVersion: 1,
          startup: [{ id: 'shell', mode: 'startup', files: ['index.html'], bytes: 1_000 }],
          onDemand: [],
        },
      })
    ).toThrow('does not cover every packaged file');
  });

  it('loads and validates the generated runtime capabilities', async () => {
    const value = {
      formatVersion: 1,
      runtime: 'webassembly',
      profile: 'portable-nflow',
      modules: ['core', 'nflow_engine'],
      capabilities: ['nflow', 'portable', 'worker'],
      features: {
        blas: true,
        controlSystem: true,
        lapack: true,
        nativeWasmExceptions: false,
        nflow: true,
        odeSolvers: true,
        packageManager: true,
        plots: true,
        slicot: true,
        threadedWasm: false,
        virtualFilesystem: true,
        worker: true,
      },
      limits: {
        maxNflowJsonBytes: 8_388_608,
        maxSourceBytes: 4_194_304,
        maxVariableBlockCells: 1_000_000,
        maxVirtualFileBytes: 33_554_432,
        maxWasmBytes: 134_217_728,
      },
      unavailable: ['native-windows'],
    } as const;
    const fetchImpl = vi.fn(async () => Response.json(value));
    await expect(
      loadRuntimeCapabilities('https://example.test/capabilities.json', fetchImpl)
    ).resolves.toEqual(value);
  });

  it('keeps older runtime capabilities compatible with the native exception flag', () => {
    const manifest = parseRuntimeCapabilities({
      formatVersion: 1,
      runtime: 'webassembly',
      profile: 'portable',
      modules: ['core'],
      capabilities: ['portable', 'worker'],
      features: {
        blas: false,
        controlSystem: false,
        lapack: false,
        nflow: false,
        odeSolvers: false,
        packageManager: false,
        plots: false,
        slicot: false,
        virtualFilesystem: true,
        worker: true,
      },
      limits: {
        maxNflowJsonBytes: 8_388_608,
        maxSourceBytes: 4_194_304,
        maxVariableBlockCells: 1_000_000,
        maxVirtualFileBytes: 33_554_432,
        maxWasmBytes: 134_217_728,
      },
      unavailable: ['native-dynamic-libraries'],
    });

    expect(manifest.features.nativeWasmExceptions).toBe(false);
    expect(manifest.features.threadedWasm).toBe(false);
  });

  it('rejects an incomplete runtime capability contract', () => {
    expect(() => parseRuntimeCapabilities({ formatVersion: 1 })).toThrow(
      'Unsupported WebAssembly capability manifest'
    );
  });
});
//=============================================================================
