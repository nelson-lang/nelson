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
const reactAlias = new URL('./node_modules/react', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
const reactDomAlias = new URL('./node_modules/react-dom', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
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
const wasmClientAlias = new URL(
  '../../../tools/wasm/nelson-worker-client.mjs',
  import.meta.url
).pathname.replace(/^\/([A-Za-z]:)/, '$1');
//=============================================================================
export default defineConfig({
  base: './',
  define: {
    __NELSON_WASM__: true,
    __NELSON_WASM_NFLOW__: JSON.stringify(process.env.NELSON_WASM_NFLOW === '1'),
    __NELSON_WASM_NMM__: JSON.stringify(process.env.NELSON_WASM_NMM === '1'),
  },
  plugins: [react()],
  resolve: {
    dedupe: ['react', 'react-dom'],
    alias: {
      '@nelson/dialogs': dialogsAlias,
      '@nelson/native-menu': nativeMenuAlias,
      '@nelson/example-gallery': exampleGalleryAlias,
      '@nelson/package-manager': packageManagerAlias,
      '@nelson/wasm-client': wasmClientAlias,
      react: reactAlias,
      'react-dom': reactDomAlias,
    },
  },
  build: {
    outDir: 'dist-wasm',
    emptyOutDir: true,
    target: 'es2021',
    chunkSizeWarningLimit: 8000,
    cssCodeSplit: true,
    assetsInlineLimit: 100_000_000,
    rollupOptions: {
      input: 'wasm.html',
      output: {
        // Keep the shared panels in their own browser-loaded packs. The native
        // WebView build still emits one self-contained file; only the Wasm
        // delivery adapter chooses progressive chunks.
        entryFileNames: 'app.js',
        chunkFileNames: (chunk) => {
          if (chunk.name === 'EditorPanel') return 'packs/editor/[name]-[hash].js';
          if (chunk.name === 'ExamplesPanel') return 'packs/examples/[name]-[hash].js';
          if (chunk.name === 'PackageManagerPanel') return 'packs/nmm/[name]-[hash].js';
          if (chunk.name === 'mockEngine') return 'packs/mock/[name]-[hash].js';
          return 'chunks/[name]-[hash].js';
        },
        assetFileNames: (asset) => {
          if (asset.name === 'EditorPanel.css') return 'packs/editor/[name]-[hash][extname]';
          if (asset.name === 'ExamplesPanel.css') return 'packs/examples/[name]-[hash][extname]';
          if (asset.name === 'PackageManagerPanel.css') return 'packs/nmm/[name]-[hash][extname]';
          return 'app.[ext]';
        },
      },
    },
  },
});
//=============================================================================
