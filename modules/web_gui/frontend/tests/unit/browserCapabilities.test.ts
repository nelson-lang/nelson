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
import { detectBrowserRuntimeDiagnostics } from '../../src/wasm/browserCapabilities';
//=============================================================================
function canvasFactory() {
  return {
    getContext: (name: string) => (name === 'webgl' || name === 'webgl2' ? {} : null),
  };
}
//=============================================================================
describe('browser runtime diagnostics', () => {
  it('detects the browser features needed by advanced WebAssembly modes', () => {
    const diagnostics = detectBrowserRuntimeDiagnostics({
      Atomics: {},
      AudioContext: function AudioContext() {},
      FileReader: function FileReader() {},
      SharedArrayBuffer: function SharedArrayBuffer() {},
      WebAssembly: {
        Exception: function Exception() {},
        Suspending: function Suspending() {},
        Tag: function Tag() {},
        instantiate: function instantiate() {},
        promising: function promising() {},
        validate: function validate() {},
      },
      caches: {},
      crossOriginIsolated: true,
      document: { createElement: canvasFactory },
      isSecureContext: true,
      navigator: {
        mediaDevices: { getUserMedia: function getUserMedia() {} },
        platform: 'UnitOS',
        serviceWorker: {},
        userAgent: 'NelsonTest/1.0',
      },
      showDirectoryPicker: function showDirectoryPicker() {},
      showOpenFilePicker: function showOpenFilePicker() {},
      showSaveFilePicker: function showSaveFilePicker() {},
    });

    expect(diagnostics.platform).toBe('UnitOS');
    expect(diagnostics.userAgent).toBe('NelsonTest/1.0');
    expect(diagnostics.features).toMatchObject({
      crossOriginIsolated: true,
      fileSystemAccess: true,
      localDirectoryAccess: true,
      pwaInstallSupport: true,
      wasmExceptions: true,
      wasmJspi: true,
      wasmThreads: true,
      webAudio: true,
      webGl: true,
      webGl2: true,
    });
    expect(diagnostics.unsupported).not.toContain('wasmThreads');
  });

  it('keeps portable fallbacks visible when browser-only APIs are unavailable', () => {
    const diagnostics = detectBrowserRuntimeDiagnostics({
      WebAssembly: {
        instantiate: function instantiate() {},
        validate: function validate() {},
      },
      crossOriginIsolated: false,
      navigator: { platform: 'FallbackOS' },
    });

    expect(diagnostics.features).toMatchObject({
      dragAndDropFiles: false,
      fileSystemAccess: false,
      wasmCore: true,
      wasmExceptions: false,
      wasmJspi: false,
      wasmThreads: false,
    });
    expect(diagnostics.unsupported).toEqual(
      expect.arrayContaining(['fileSystemAccess', 'wasmJspi'])
    );
    expect(diagnostics.notes).toEqual(
      expect.arrayContaining([
        'JSPI-dependent blocking input must stay disabled or use a fallback.',
        'Use drag-and-drop or file input fallback for local files in this browser.',
      ])
    );
  });
});
//=============================================================================
