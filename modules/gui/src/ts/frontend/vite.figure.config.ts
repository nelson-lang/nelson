//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Builds the slim standalone figure page (figure.html + figure.js) into
// dist-figure/. A native figure window loads this instead of the desktop
// index.html, so it pulls in only the figure panel, not the Monaco editor, the
// xterm console or the dockview desktop shell. postbuild-figure.mjs then folds
// the CSS in, injects /webui.js, and copies the two files next to the desktop
// bundle in dist/ (and the runtime bin trees).
//=============================================================================
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { uihtmlBootstrapPlugin } from './vite.uihtml-bootstrap';
//=============================================================================
// Shared, Qt-free dialog/assistant components live in the neutral webview module;
// mirror the desktop config's aliases so the same sources resolve identically.
const dialogsAlias = new URL(
  '../../../../web_engine/frontend/dialogs',
  import.meta.url
).pathname.replace(/^\/([A-Za-z]:)/, '$1');
const nativeMenuAlias = new URL(
  '../../../../web_engine/frontend/native-menu',
  import.meta.url
).pathname.replace(/^\/([A-Za-z]:)/, '$1');
const assistantAlias = new URL(
  '../../../../web_engine/frontend/assistant',
  import.meta.url
).pathname.replace(/^\/([A-Za-z]:)/, '$1');
const exampleGalleryAlias = new URL(
  '../../../../web_engine/frontend/examples',
  import.meta.url
).pathname.replace(/^\/([A-Za-z]:)/, '$1');
const packageManagerAlias = new URL(
  '../../../../nmm_gui/frontend/src',
  import.meta.url
).pathname.replace(/^\/([A-Za-z]:)/, '$1');
const reactAlias = new URL('./node_modules/react', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
const reactDomAlias = new URL('./node_modules/react-dom', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
const figureEntry = new URL('./figure.html', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
export default defineConfig({
  base: './',
  plugins: [react(), uihtmlBootstrapPlugin()],
  resolve: {
    dedupe: ['react', 'react-dom'],
    alias: {
      '@nelson/dialogs': dialogsAlias,
      '@nelson/assistant': assistantAlias,
      '@nelson/native-menu': nativeMenuAlias,
      '@nelson/example-gallery': exampleGalleryAlias,
      '@nelson/package-manager': packageManagerAlias,
      react: reactAlias,
      'react-dom': reactDomAlias,
    },
  },
  build: {
    outDir: 'dist-figure',
    emptyOutDir: true,
    target: 'es2021',
    cssCodeSplit: false,
    assetsInlineLimit: 100_000_000,
    chunkSizeWarningLimit: 8000,
    rollupOptions: {
      input: figureEntry,
      output: {
        inlineDynamicImports: true,
        entryFileNames: 'figure.js',
        assetFileNames: 'figure.[ext]',
      },
    },
  },
});
//=============================================================================
