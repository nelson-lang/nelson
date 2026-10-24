//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Vite configuration for the NFlow front-end.
//
// Build: bundles the two ESM entries (app, library-loader) into dist/app/
// with stable (unhashed) names because the C++ host references them by
// path (`import('./app/library-loader.js')`, `location.replace('./nflow.html')`).
// Everything else (nflow.html, styles, libraries, KaTeX, locales, IIFE
// helper scripts) is produced by scripts/postbuild.mjs.
//
// Dev: `npm run dev` serves nflow.html from source with a stubbed webui.js
// bridge so the UI can be developed without a running Nelson instance.
//=============================================================================
import { defineConfig, type Plugin } from 'vite';
import react from '@vitejs/plugin-react';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
//=============================================================================
const moduleRoot = path.dirname(fileURLToPath(import.meta.url));
const srcTs = path.resolve(moduleRoot, 'src/ts');
// NFlow product version: single source of truth is the C++ header (shared with
// the codegen banners and FMU export), parsed here for the About dialog.
const nflowVersionHeader = fs.readFileSync(
  path.resolve(moduleRoot, '../nflow_engine/src/include/NFlowVersion.hpp'),
  'utf8'
);
//=============================================================================
const nflowProductVersion = /#define\s+NFLOW_PRODUCT_VERSION\s+"([^"]+)"/.exec(
  nflowVersionHeader
)?.[1];
//=============================================================================
if (!nflowProductVersion) {
  throw new Error('NFLOW_PRODUCT_VERSION not found in nflow_engine NFlowVersion.hpp');
}
//=============================================================================
const sharedExampleGalleryStyles = path.resolve(
  moduleRoot,
  '../webview/frontend/examples/gallery-shared.css'
);
/**
 * Dev-server plugin: serves the application shell and the static assets the
 * C++ host normally provides, so `npm run dev` works standalone.
 */
function nflowDevServer(): Plugin {
  const sendFile = (res: any, filePath: string, contentType: string): boolean => {
    if (!fs.existsSync(filePath)) return false;
    res.setHeader('Content-Type', contentType);
    res.end(fs.readFileSync(filePath));
    return true;
  };
  return {
    name: 'nflow-dev-server',
    apply: 'serve',
    configureServer(server) {
      server.middlewares.use(async (req, res, next) => {
        const url = (req.url || '/').split('?')[0];
        if (url === '/' || url === '/nflow.html') {
          let html = fs.readFileSync(path.join(srcTs, 'nflow.html'), 'utf8');
          // Point scripts at TypeScript sources served by Vite.
          html = html
            .replace(
              '<script src="./app/head-init.js"></script>',
              '<script type="module" src="/src/ts/app/head-init.ts"></script>'
            )
            .replace('<link rel="modulepreload" href="./app/app.js">', '')
            .replace(
              '<script src="./app/floating-toolbar.js" defer></script>',
              '<script type="module" src="/src/ts/app/floating-toolbar.ts"></script>'
            )
            .replace(
              '<script src="./app/wheel-prevent.js" async></script>',
              '<script type="module" src="/src/ts/app/wheel-prevent.ts"></script>'
            )
            .replace(
              '<script type="module" src="./app/app.js"></script>',
              '<script type="module" src="/src/ts/app/app.ts"></script>'
            )
            .replace(/href="(\.\/)?styles\//g, 'href="/src/ts/styles/')
            .replace('href="assets/katex/katex.min.css"', 'href="/@katex/katex.min.css"')
            .replace('src="assets/katex/katex.min.js"', 'src="/@katex/katex.min.js"');
          try {
            const transformedHtml = await server.transformIndexHtml(url, html);
            res.setHeader('Content-Type', 'text/html; charset=utf-8');
            res.end(transformedHtml);
          } catch (error) {
            next(error as Error);
          }
          return;
        }
        if (url === '/webui.js') {
          res.setHeader('Content-Type', 'application/javascript; charset=utf-8');
          res.end('// webui bridge stub (dev server, no Nelson host)\n');
          return;
        }
        if (url === '/webview/frontend/examples/gallery-shared.css') {
          if (sendFile(res, sharedExampleGalleryStyles, 'text/css; charset=utf-8')) return;
        }
        if (url.startsWith('/@katex/')) {
          const rel = url.slice('/@katex/'.length);
          const p = path.join(moduleRoot, 'node_modules/katex/dist', rel);
          const type = rel.endsWith('.css') ? 'text/css' : 'application/javascript';
          if (sendFile(res, p, `${type}; charset=utf-8`)) return;
        }
        if (url.startsWith('/src/ts/locale/')) {
          const rel = url.slice('/src/ts/locale/'.length);
          if (sendFile(res, path.join(moduleRoot, 'locale', rel), 'application/json')) return;
        }
        if (url.startsWith('/src/ts/libraries/')) {
          const rel = url.slice('/src/ts/libraries/'.length);
          if (rel.endsWith('/library.json')) {
            if (
              sendFile(
                res,
                path.join(moduleRoot, '..', 'nflow_blocks', 'libraries', rel),
                'application/json'
              )
            ) {
              return;
            }
          }
          if (rel.endsWith('/blocks.js')) {
            const tsRel = rel.replace(/\.js$/, '.ts');
            const tsPath = path.join(srcTs, 'libraries', tsRel);
            if (!fs.existsSync(tsPath)) {
              res.setHeader('Content-Type', 'application/javascript; charset=utf-8');
              res.end('export {};\n');
              return;
            }
            try {
              const result = await server.transformRequest(`/src/ts/libraries/${tsRel}`);
              if (result) {
                res.setHeader('Content-Type', 'application/javascript; charset=utf-8');
                res.end(result.code);
                return;
              }
            } catch (error) {
              next(error as Error);
              return;
            }
          }
          if (rel.endsWith('.svg')) {
            const direct = path.join(moduleRoot, '..', 'nflow_blocks', 'libraries', rel);
            const exported = path.join(
              moduleRoot,
              '..',
              'nflow_blocks',
              'libraries',
              path.dirname(rel),
              'exports',
              path.basename(rel)
            );
            if (sendFile(res, direct, 'image/svg+xml')) return;
            if (sendFile(res, exported, 'image/svg+xml')) return;
          }
        }
        if (url.startsWith('/assets/katex/')) {
          const rel = url.slice('/assets/katex/'.length);
          if (
            sendFile(
              res,
              path.join(moduleRoot, 'node_modules/katex/dist', rel),
              'application/octet-stream'
            )
          )
            return;
        }
        if (url.startsWith('/libraries/')) {
          const rel = url.slice('/libraries/'.length);
          const type = url.endsWith('.json')
            ? 'application/json'
            : url.endsWith('.svg')
            ? 'image/svg+xml'
            : 'application/octet-stream';
          if (sendFile(res, path.join(srcTs, 'libraries', rel), type)) return;
        }
        if (url.startsWith('/locale/')) {
          const rel = url.slice('/locale/'.length);
          if (sendFile(res, path.join(moduleRoot, 'locale', rel), 'application/json')) return;
        }
        if (url === '/nflow/ui-settings.json') {
          if (sendFile(res, path.join(srcTs, 'config/ui-settings.json'), 'application/json')) {
            return;
          }
        }
        if (url === '/styles/themes.css') {
          // theme-manager fetches this path relative to the page.
          if (sendFile(res, path.join(srcTs, 'styles/themes.css'), 'text/css; charset=utf-8')) {
            return;
          }
        }
        next();
      });
    },
  };
}
//=============================================================================
// Shared, Qt-free dialog components live in the neutral webview module (the
// shared WebUI layer both front ends already depend on), so nflow_gui does not
// depend on web_gui. Consumed source-only through the `@nelson/dialogs` alias.
const dialogsAlias = path.resolve(moduleRoot, '../webview/frontend/dialogs');
const nativeMenuAlias = path.resolve(moduleRoot, '../webview/frontend/native-menu');
const exampleGalleryAlias = path.resolve(moduleRoot, '../webview/frontend/examples');
export default defineConfig({
  root: moduleRoot,
  base: './',
  // Product version from modules/nflow_engine/src/include/NFlowVersion.hpp.
  define: {
    __NFLOW_VERSION__: JSON.stringify(nflowProductVersion),
  },
  plugins: [react(), nflowDevServer()],
  resolve: {
    // Force a single React copy (the shared dialog sources live outside this
    // package and would otherwise fail to resolve `react`).
    dedupe: ['react', 'react-dom'],
    alias: {
      '@nelson/dialogs': dialogsAlias,
      '@nelson/native-menu': nativeMenuAlias,
      '@nelson/example-gallery': exampleGalleryAlias,
      react: path.resolve(moduleRoot, 'node_modules/react'),
    },
  },
  build: {
    outDir: 'dist',
    emptyOutDir: true,
    target: 'es2020',
    chunkSizeWarningLimit: 600,
    minify: true,
    sourcemap: true,
    rollupOptions: {
      input: {
        app: path.join(srcTs, 'app/app.ts'),
        'library-loader': path.join(srcTs, 'app/library-loader.ts'),
      },
      output: {
        format: 'es',
        // The `app` entry is content-hashed so the chunk imports and the nflow.html
        // reference (rewritten by prepare-dist) resolve to the SAME URL. A stable app.js
        // copied to app.<hash>.js loaded the hashed copy while the shared chunks imported
        // ../app.js, which evaluated the entry graph twice (two startApp() boots, two
        // `state` instances). library-loader keeps its stable name because the C++ host
        // imports it by fixed path.
        entryFileNames: (chunk) =>
          chunk.name === 'app' ? 'app/[name]-[hash].js' : 'app/[name].js',
        chunkFileNames: 'app/chunks/[name]-[hash].js',
        assetFileNames: 'app/assets/[name]-[hash][extname]',
        // One shared chunk for the icon cache: it holds process-wide state, and a copy in
        // each of the `app` and `library-loader` bundles would warm one cache while the
        // palette/canvas read the other.
        manualChunks(id) {
          if (id.includes('libraries/icon-cache')) return 'icon-cache';
        },
      },
    },
  },
});
//=============================================================================
