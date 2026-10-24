//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
// The bundle is served by WebUI from modules/nmm_gui/frontend/dist (see
// nmm_guiBuiltin.cpp): relative base, one JS file and one CSS file. The
// classic <script src="webui.js"> of index.html is injected by WebUI at run
// time and left untouched by the build.
export default defineConfig({
  base: './',
  plugins: [react()],
  build: {
    outDir: 'dist',
    emptyOutDir: true,
    target: 'es2021',
    cssCodeSplit: false,
    rollupOptions: {
      output: {
        inlineDynamicImports: true,
        entryFileNames: 'app.js',
        assetFileNames: 'app.[ext]',
      },
    },
  },
  server: { port: 5212, strictPort: false },
  preview: { port: 5211, strictPort: true },
  test: { include: ['tests/unit/**/*.test.ts'], environment: 'node' },
} as Parameters<typeof defineConfig>[0]);
//=============================================================================
