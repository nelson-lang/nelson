//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { defineConfig } from 'vitest/config';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
//=============================================================================
const moduleRoot = path.dirname(fileURLToPath(import.meta.url));
const srcTs = path.resolve(moduleRoot, 'src/ts');
export default defineConfig({
  resolve: {
    // The shared dialog sources live outside this package, so force React to
    // resolve from this module's node_modules (a single copy) from anywhere.
    dedupe: ['react', 'react-dom'],
    alias: [
      // Shared, Qt-free dialog components live in the neutral webview module.
      {
        find: '@nelson/dialogs',
        replacement: path.resolve(moduleRoot, '../webview/frontend/dialogs'),
      },
      {
        find: '@nelson/native-menu',
        replacement: path.resolve(moduleRoot, '../webview/frontend/native-menu'),
      },
      {
        find: '@nelson/example-gallery',
        replacement: path.resolve(moduleRoot, '../webview/frontend/examples'),
      },
      { find: 'react', replacement: path.resolve(moduleRoot, 'node_modules/react') },
      // Historical specs import build outputs (`../../../dist/x/y.js`).
      // dist/ mirrors src/ts/, so route them to the TypeScript sources;
      // no pre-build required to run the unit tests.
      { find: /^(?:\.\.\/)+dist\/(.*)\.js$/, replacement: `${srcTs}/$1` },
      // Runtime dynamic imports (variable specifiers) reach the resolver
      // already normalized against the project root.
      { find: /^\/dist\/(.*)\.js$/, replacement: `${srcTs}/$1` },
    ],
  },
  // Match the tsconfig ("jsx": "react-jsx"): use the automatic JSX runtime so source
  // components need no React import. The classic runtime throws "React is not
  // defined" at render time.
  esbuild: {
    jsx: 'automatic',
    jsxImportSource: 'react',
  },
  test: {
    environment: 'jsdom',
    globals: true,
    setupFiles: ['./vitest.setup.ts'],
    include: ['src/ts/**/*.spec.{ts,tsx}'],
    testTimeout: 20000,
    coverage: {
      provider: 'v8',
      include: ['src/ts/**/*.{ts,tsx}'],
      exclude: ['src/ts/**/*.spec.ts'],
      reporter: ['html', 'lcov', 'text'],
    },
  },
});
//=============================================================================
