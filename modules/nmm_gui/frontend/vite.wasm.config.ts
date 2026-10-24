//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
//=============================================================================
export default defineConfig({
  base: './',
  plugins: [react()],
  build: {
    outDir: 'dist-wasm',
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
});
//=============================================================================
