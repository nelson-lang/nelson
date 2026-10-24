//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { defineConfig, mergeConfig } from 'vite';
import baseConfig from './vite.config';
//=============================================================================
const moduleRoot = path.dirname(fileURLToPath(import.meta.url));
export default defineConfig(
  mergeConfig(baseConfig, {
    resolve: {
      alias: {
        '@nelson/wasm-client': path.resolve(
          moduleRoot,
          '../../tools/wasm/nelson-worker-client.mjs'
        ),
      },
    },
    build: {
      outDir: 'dist-wasm',
      sourcemap: false,
      rollupOptions: {
        input: {
          app: path.join(moduleRoot, 'src/ts/wasm/main.ts'),
          'library-loader': path.join(moduleRoot, 'src/ts/app/library-loader.ts'),
        },
      },
    },
  })
);
//=============================================================================
