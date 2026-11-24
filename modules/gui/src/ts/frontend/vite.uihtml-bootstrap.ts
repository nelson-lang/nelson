//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Builds the uihtml page bridge (src/panels/uihtmlBootstrap.entry.ts) into the
// standalone /uihtml-bootstrap.js asset. The served script must be a classic,
// self-contained script - it runs in arbitrary user pages on a different origin,
// with no module system and no bundler-injected helpers - so it is emitted on
// its own with esbuild in IIFE form rather than folded into the app bundle.
//
// One plugin covers both ends: a dev middleware answers /uihtml-bootstrap.js by
// compiling on demand, and generateBundle emits the same file into dist. The
// source stays typed and unit-tested under src/, not a hand-written asset in
// public/.
//=============================================================================
import type { Plugin } from 'vite';
import { build } from 'esbuild';
//=============================================================================
// Resolve the entry next to this config file without a Node builtin import (the
// tsconfig scopes `types` and omits @types/node); strip the leading slash that
// Windows file URLs carry.
const ENTRY = new URL('./src/panels/uihtmlBootstrap.entry.ts', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
const SERVE_PATH = '/uihtml-bootstrap.js';
//=============================================================================
async function compileBootstrap(): Promise<string> {
  const result = await build({
    entryPoints: [ENTRY],
    bundle: true,
    format: 'iife',
    // Conservative for an asset that may run in any browser or embedded WebView.
    target: 'es2019',
    platform: 'browser',
    legalComments: 'none',
    write: false,
  });
  return result.outputFiles[0].text;
}
//=============================================================================
export function uihtmlBootstrapPlugin(): Plugin {
  return {
    name: 'nelson:uihtml-bootstrap',
    configureServer(server) {
      server.middlewares.use((req, res, next) => {
        const url = req.url ? req.url.split('?')[0] : '';
        if (url !== SERVE_PATH) {
          next();
          return;
        }
        compileBootstrap()
          .then((code) => {
            res.setHeader('Content-Type', 'application/javascript; charset=utf-8');
            res.setHeader('Cache-Control', 'no-cache');
            res.end(code);
          })
          .catch((error: unknown) => {
            res.statusCode = 500;
            res.end(`/* uihtml-bootstrap build failed: ${String(error)} */`);
          });
      });
    },
    async generateBundle() {
      this.emitFile({
        type: 'asset',
        fileName: 'uihtml-bootstrap.js',
        source: await compileBootstrap(),
      });
    },
  };
}
//=============================================================================
