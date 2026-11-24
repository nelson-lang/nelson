//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Builds the notebook figure renderer into a single self-contained ES module
// (dist-replayer/nelson-figure-replayer.js). It renders a saved scene static
// with nothing behind it, and, when the host opens a messaging channel to a
// live kernel, mounts the interactive figure panel over that channel. The host
// loads one file, so the stylesheet is injected from script and the main-thread
// 2D path leaves no separate worker file behind.
//=============================================================================
import { defineConfig, type Plugin } from 'vite';
//=============================================================================
const ENTRY_FILE = 'nelson-figure-replayer.js';
//=============================================================================
// Fold the extracted stylesheet into the entry as an injected <style>, and drop
// the 2D canvas worker chunk the main-thread path never loads, so the output is
// the single file the host embeds.
function singleFileRenderer(): Plugin {
  return {
    name: 'nelson-single-file-renderer',
    enforce: 'post',
    generateBundle(_options, bundle) {
      let css = '';
      for (const [fileName, asset] of Object.entries(bundle)) {
        if (asset.type === 'asset' && fileName.endsWith('.css')) {
          css +=
            typeof asset.source === 'string'
              ? asset.source
              : Buffer.from(asset.source).toString('utf8');
          delete bundle[fileName];
        }
      }
      for (const fileName of Object.keys(bundle)) {
        if (fileName !== ENTRY_FILE && fileName.includes('figureCanvasWorker')) {
          delete bundle[fileName];
        }
      }
      const entry = bundle[ENTRY_FILE];
      if (entry && entry.type === 'chunk') {
        if (css) {
          const inject = `(function(){try{var s=document.createElement('style');s.textContent=${JSON.stringify(
            css
          )};document.head.appendChild(s);}catch(e){}})();\n`;
          entry.code = inject + entry.code;
        }
        // The host webview has no Node globals, so a surviving process.env read
        // crashes the renderer at load. The define folds NODE_ENV in; fail the
        // build loudly if a dependency reintroduces another process.env access.
        if (entry.code.includes('process.env')) {
          throw new Error(
            'nelson-figure-replayer: process.env survived bundling; add a define for it in vite.replayer.config.ts.'
          );
        }
      }
    },
  };
}
//=============================================================================
// Resolve the entry next to this config without a Node builtin import (the
// tsconfig scopes `types` and omits @types/node); strip the leading slash that
// Windows file URLs carry.
const ENTRY = new URL('./src/replayer/notebookRenderer.ts', import.meta.url).pathname.replace(
  /^\/([A-Za-z]:)/,
  '$1'
);
//=============================================================================
export default defineConfig({
  // A notebook renderer runs in a webview with no Node globals, so fold the
  // NODE_ENV check in at build time rather than touch process at runtime.
  define: {
    'process.env.NODE_ENV': JSON.stringify('production'),
  },
  plugins: [singleFileRenderer()],
  build: {
    // Flat module-root dist-replayer (modules/gui/dist-replayer): saveas(fig,
    // 'f.html') inlines this bundle, resolving it from the module tree.
    outDir: '../../../dist-replayer',
    emptyOutDir: true,
    target: 'es2020',
    cssCodeSplit: false,
    sourcemap: false,
    minify: 'esbuild',
    lib: {
      entry: ENTRY,
      formats: ['es'],
      fileName: () => 'nelson-figure-replayer.js',
    },
    rollupOptions: {
      // One file: a notebook renderer asset is loaded as a single module, and the
      // WebGPU path (an optional dynamic import) is folded in rather than split.
      output: { inlineDynamicImports: true },
    },
  },
});
//=============================================================================
