//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import {
  resolveVersionedAssetUrl,
  resolveWasmRuntimeUrls,
  supportsWasmJspi,
} from '../../src/wasm/runtimeUrls';
//=============================================================================
describe('WebAssembly runtime URLs', () => {
  it('uses artifacts next to the browser application for legacy single-engine packages', () => {
    expect(resolveWasmRuntimeUrls('https://example.test/app/?runtime=wasm')).toEqual({
      moduleUrl: 'https://example.test/app/nelson-portable.mjs',
      wasmUrl: 'https://example.test/app/nelson-portable.wasm',
      workerUrl: 'https://example.test/app/nelson-worker.mjs',
      sharedWorkerUrl: 'https://example.test/app/nelson-shared-worker.mjs',
    });
  });

  it('auto-selects a dual-engine package from the deployment manifest', () => {
    const files = [
      'engine/jspi/nelson-portable.mjs',
      'engine/jspi/nelson-portable.wasm',
      'engine/classic/nelson-portable.mjs',
      'engine/classic/nelson-portable.wasm',
    ];
    const engine = supportsWasmJspi() ? 'jspi' : 'classic';
    expect(
      resolveWasmRuntimeUrls('https://example.test/app/?runtime=wasm', undefined, files)
    ).toEqual({
      moduleUrl: `https://example.test/app/engine/${engine}/nelson-portable.mjs`,
      wasmUrl: `https://example.test/app/engine/${engine}/nelson-portable.wasm`,
      workerUrl: 'https://example.test/app/nelson-worker.mjs',
      sharedWorkerUrl: 'https://example.test/app/nelson-shared-worker.mjs',
    });
  });

  it('uses the classic engine when requested explicitly', () => {
    expect(resolveWasmRuntimeUrls('https://example.test/app/?runtime=wasm&engine=classic')).toEqual(
      {
        moduleUrl: 'https://example.test/app/engine/classic/nelson-portable.mjs',
        wasmUrl: 'https://example.test/app/engine/classic/nelson-portable.wasm',
        workerUrl: 'https://example.test/app/nelson-worker.mjs',
        sharedWorkerUrl: 'https://example.test/app/nelson-shared-worker.mjs',
      }
    );
  });

  it('uses the JSPI engine when requested explicitly', () => {
    expect(resolveWasmRuntimeUrls('https://example.test/app/?runtime=wasm&engine=jspi')).toEqual({
      moduleUrl: 'https://example.test/app/engine/jspi/nelson-portable.mjs',
      wasmUrl: 'https://example.test/app/engine/jspi/nelson-portable.wasm',
      workerUrl: 'https://example.test/app/nelson-worker.mjs',
      sharedWorkerUrl: 'https://example.test/app/nelson-shared-worker.mjs',
    });
  });

  it('accepts explicit module, Worker and SharedWorker URLs', () => {
    expect(
      resolveWasmRuntimeUrls(
        'https://example.test/app/?runtime=wasm&module=/runtime/nelson.mjs&wasm=/runtime/nelson.wasm&worker=/runtime/worker.mjs&sharedWorker=/runtime/shared.mjs'
      )
    ).toEqual({
      moduleUrl: 'https://example.test/runtime/nelson.mjs',
      wasmUrl: 'https://example.test/runtime/nelson.wasm',
      workerUrl: 'https://example.test/runtime/worker.mjs',
      sharedWorkerUrl: 'https://example.test/runtime/shared.mjs',
    });
  });

  it('omits the shared engine when the page opts out', () => {
    for (const value of ['0', 'false', 'off', 'dedicated']) {
      const urls = resolveWasmRuntimeUrls(`https://example.test/app/?sharedEngine=${value}`);
      expect(urls.sharedWorkerUrl).toBeUndefined();
      expect(urls.workerUrl).toBe('https://example.test/app/nelson-worker.mjs');
    }
    expect(resolveWasmRuntimeUrls('https://example.test/app/?sharedEngine=1').sharedWorkerUrl).toBe(
      'https://example.test/app/nelson-shared-worker.mjs'
    );
  });

  it('versions same-origin runtime assets with the packaged build identifier', () => {
    expect(
      resolveWasmRuntimeUrls(
        'https://example.test/app/?runtime=wasm&engine=classic',
        'https://example.test/app/app.js?build=abc123'
      )
    ).toEqual({
      moduleUrl: 'https://example.test/app/engine/classic/nelson-portable.mjs?build=abc123',
      wasmUrl: 'https://example.test/app/engine/classic/nelson-portable.wasm?build=abc123',
      workerUrl: 'https://example.test/app/nelson-worker.mjs?build=abc123',
      sharedWorkerUrl: 'https://example.test/app/nelson-shared-worker.mjs?build=abc123',
    });
  });

  it('does not rewrite explicitly hosted cross-origin runtime assets', () => {
    expect(
      resolveWasmRuntimeUrls(
        'https://example.test/app/?module=https://cdn.test/nelson.mjs&build=abc123'
      ).moduleUrl
    ).toBe('https://cdn.test/nelson.mjs');
  });

  it('propagates a build identifier to same-origin lazy assets', () => {
    expect(
      resolveVersionedAssetUrl(
        './catalog.json',
        'https://example.test/examples/index.html?build=abc123'
      )
    ).toBe('https://example.test/examples/catalog.json?build=abc123');
    expect(
      resolveVersionedAssetUrl(
        'https://cdn.test/catalog.json',
        'https://example.test/examples/index.html?build=abc123'
      )
    ).toBe('https://cdn.test/catalog.json');
  });
});
//=============================================================================
