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
import { onNFlowPush } from './native-rpc.js';
import { mountLibraryManager } from './library-manager.js';
import { renderBlockLibrary, setBlockLibrary } from './block-library-handlers.js';
import { emitAppEvent } from './app-events.js';
//=============================================================================
// Loads the block libraries in the background and keeps them in sync with
// libraries the host loads later. Resolves once the palette is rebuilt.
export function startLibraries(): Promise<void> {
  // Dynamically import the library-loader at runtime so the static module
  // import doesn't delay initial module evaluation and `app.init` start.
  let libStarter: Promise<void> | undefined = undefined;
  if ((window as any).__nf_init_libraries_promise) {
    libStarter = (window as any).__nf_init_libraries_promise;
  } else {
    try {
      emitAppEvent('nf-libraries-start');
      (window as any).__nf_init_libraries_promise = (async () => {
        const m = await import('./library-loader.js');
        return m.initLibraries();
      })();
      libStarter = (window as any).__nf_init_libraries_promise;
    } catch (e) {
      libStarter = Promise.resolve();
    }
  }

  const libPromise = Promise.resolve(libStarter)
    .then(async () => {
      try {
        const m = await import('./library-loader.js');
        const dynamicLib = m.buildBlockLibrary();
        setBlockLibrary(
          dynamicLib.length
            ? (dynamicLib as unknown as import('./block-library-handlers.js').BlockGroup[])
            : []
        );
        // Always refresh module-level blockTemplates so createBlock() can find templates
        // even when pasting from another window (renderBlockLibrary only sets window.blockTemplates).
        try {
          const renderMod = await import('../render/render.js');
          if (typeof renderMod.refreshBlockTemplates === 'function')
            renderMod.refreshBlockTemplates();
        } catch (_e) {}
        renderBlockLibrary();
        const libManagerContainer = document.getElementById('libraryManager');
        if (libManagerContainer) mountLibraryManager(libManagerContainer as HTMLElement);
      } catch (e) {
        setBlockLibrary([]);
        renderBlockLibrary();
      }
    })
    .catch(() => {
      setBlockLibrary([]);
      renderBlockLibrary();
    });

  // Exposes the lib-ready promise (includes refreshBlockTemplates) so tryToLoadFile
  // can await it before rendering a model file. Set it before nf-ready fires,
  // when initUI() runs.
  (window as any).__nf_lib_ready_promise = libPromise;

  // Called by the C++ host (via win->run()) to inject library manifests into the
  // BUNDLED library-loader instance. The host cannot reach that instance
  // directly: its import() calls fetch the external library-loader.js, which is
  // a different module instance.
  (window as any).__nf_loadLibraryManifests = async (rawManifests: unknown[]) => {
    try {
      const loader = await import('./library-loader.js');
      const parsed = rawManifests
        .map((manifest) => {
          try {
            return typeof manifest === 'string' ? JSON.parse(manifest) : manifest;
          } catch (_e) {
            return null;
          }
        })
        .filter(Boolean);
      loader.replaceExternalManifests(parsed as import('./library-loader.js').LibraryManifest[]);
      await loader.loadAllDiscovered();
      const lib = loader.buildBlockLibrary();
      setBlockLibrary(lib as unknown as import('./block-library-handlers.js').BlockGroup[]);
      try {
        const renderMod = await import('../render/render.js');
        if (typeof renderMod.refreshBlockTemplates === 'function')
          renderMod.refreshBlockTemplates();
      } catch (_e) {}
      renderBlockLibrary();
      try {
        const libManagerContainer = document.getElementById('libraryManager');
        if (libManagerContainer) mountLibraryManager(libManagerContainer as HTMLElement);
      } catch (_e) {}
    } catch (_e) {}
  };

  onNFlowPush('nflow.library.loaded', (payload: unknown) => {
    const raw = payload as { manifests?: unknown[] } | unknown[];
    const manifests = Array.isArray(raw) ? raw : Array.isArray(raw?.manifests) ? raw.manifests : [];
    void (window as any).__nf_loadLibraryManifests?.(manifests);
  });

  return libPromise;
}
//=============================================================================
