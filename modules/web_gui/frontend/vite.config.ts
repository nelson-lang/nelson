//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
//=============================================================================
// Shared, Qt-free dialog components live in the neutral webview module so that
// neither web front end depends on the other. See modules/webview/frontend.
// Resolve the absolute path without importing a Node builtin (this project's
// tsconfig scopes `types` and does not include @types/node): derive it from the
// config file URL and strip the leading slash Windows file URLs carry.
const dialogsAlias = new URL('../../webview/frontend/dialogs', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
const nativeMenuAlias = new URL(
  '../../webview/frontend/native-menu',
  import.meta.url
).pathname.replace(/^\/([A-Za-z]:)/, '$1');
//=============================================================================
const exampleGalleryAlias = new URL(
  '../../webview/frontend/examples',
  import.meta.url
).pathname.replace(/^\/([A-Za-z]:)/, '$1');
//=============================================================================
const packageManagerAlias = new URL('../../nmm_gui/frontend/src', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
// The shared dialog sources import `react`; force it to resolve from this
// package's node_modules (a single React copy) from anywhere.
const reactAlias = new URL('./node_modules/react', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
const reactDomAlias = new URL('./node_modules/react-dom', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
const repositoryRoot = new URL('../../..', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
// Assets are bundled and served by the WebUI/civetweb backend (nelson-webview).
// Relative base so the built index.html works when served from any mount point.
//
// Packaging goal: a minimal number of files in modules/web_gui/frontend/dist. We
// force a single JS chunk (inlineDynamicImports), a single CSS file (cssCodeSplit
// off), and inline every asset (fonts) as base64. A postbuild step then folds the
// CSS into index.html so the shipped bundle is a single self-contained file. The
// server resolves this dist from the module tree at runtime; no extra copy step.
export default defineConfig({
  base: './',
  plugins: [react()],
  resolve: {
    dedupe: ['react', 'react-dom'],
    alias: {
      '@nelson/dialogs': dialogsAlias,
      '@nelson/native-menu': nativeMenuAlias,
      '@nelson/example-gallery': exampleGalleryAlias,
      '@nelson/package-manager': packageManagerAlias,
      react: reactAlias,
      'react-dom': reactDomAlias,
    },
  },
  optimizeDeps: {
    // Scan only the desktop entry; the wasm entry imports @nelson/wasm-client,
    // which is produced by the wasm build and absent during a plain `npm run dev`.
    entries: ['index.html'],
    include: ['@blocknote/core', '@blocknote/react', '@blocknote/mantine', 'fflate'],
  },
  build: {
    outDir: 'dist',
    emptyOutDir: true,
    target: 'es2021',
    cssCodeSplit: false,
    assetsInlineLimit: 100_000_000,
    chunkSizeWarningLimit: 8000,
    rollupOptions: {
      output: {
        inlineDynamicImports: true,
        entryFileNames: 'app.js',
        assetFileNames: 'app.[ext]',
      },
    },
  },
  server: {
    port: 5199,
    strictPort: false,
    fs: { allow: [repositoryRoot] },
  },
});
//=============================================================================
