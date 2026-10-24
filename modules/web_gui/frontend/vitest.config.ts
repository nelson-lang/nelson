//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { defineConfig } from 'vitest/config';
//=============================================================================
// Shared dialog components live in the neutral webview module. Resolve without a
// Node builtin import (tsconfig scopes `types`); strip the Windows file-URL slash.
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
const reactAlias = new URL('./node_modules/react', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
// Unit tests (vitest). E2E lives separately under tests/e2e (Playwright).
export default defineConfig({
  resolve: {
    dedupe: ['react', 'react-dom'],
    alias: {
      '@nelson/dialogs': dialogsAlias,
      '@nelson/native-menu': nativeMenuAlias,
      '@nelson/example-gallery': exampleGalleryAlias,
      '@nelson/package-manager': packageManagerAlias,
      react: reactAlias,
    },
  },
  test: {
    environment: 'jsdom',
    execArgv: ['--no-experimental-webstorage'],
    globals: true,
    include: ['tests/unit/**/*.test.{ts,tsx}'],
    coverage: {
      provider: 'v8',
      include: ['src/**/*.{ts,tsx}'],
      exclude: ['src/**/*.test.ts', 'src/**/*.test.tsx'],
      reporter: ['html', 'lcov', 'text'],
    },
  },
});
//=============================================================================
