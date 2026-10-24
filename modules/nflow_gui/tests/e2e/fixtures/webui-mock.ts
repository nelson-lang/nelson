//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/// <reference types="node" />
import { test as base, type Page } from '@playwright/test';
import * as path from 'path';
import * as url from 'url';
import * as fs from 'fs';
import { spawn } from 'child_process';
import http from 'http';
import https from 'https';
//=============================================================================
const __dirname = path.dirname(url.fileURLToPath(import.meta.url));
export const SIMPLE_DIAGRAM_PATH = path.join(__dirname, 'simple-diagram.nflow');
export const SIMPLE_DIAGRAM_CONTENT = fs.existsSync(SIMPLE_DIAGRAM_PATH)
  ? fs.readFileSync(SIMPLE_DIAGRAM_PATH, { encoding: 'utf8' })
  : '';
/**
 * Playwright fixture that installs a proxy around `window.webui.call` before
 * the page finishes loading.
 *
 * - `rpc/nflow.file.openDialog` resolves with SIMPLE_DIAGRAM_PATH, so tests can
 *   exercise "Open" without an OS dialog.
 * - `rpc/nflow.file.saveDialog` resolves with a writable temp path.
 * - All other calls go to the real `window.webui.call` (live C++ backend).
 *
 * `webui.js` creates `window.webui` asynchronously, after the init script runs,
 * so the proxy polls for it and replaces its `call` method once it appears.
 */
type WebuiMockFixture = {
  mockPage: Page;
  /** Path that `nflow.file.openDialog` returns. */
  openDialogPath: string;
};
//=============================================================================
type WebuiMockWorkerFixture = {
  nflowServer: { port: number; url: string };
};
//=============================================================================
export const test = base.extend<WebuiMockFixture, WebuiMockWorkerFixture>({
  // One static server per worker for the local `dist/` artefacts, so parallel
  // workers share no state.
  nflowServer: [
    async ({}, use) => {
      const initialTarget = process.env['NFLOW_URL'] ?? 'http://localhost:1973/nflow.html';
      // A non-localhost NFLOW_URL is left alone.
      let parsed: URL | null = null;
      try {
        parsed = new URL(initialTarget);
      } catch (_e) {
        parsed = null;
      }

      const isLocalHost =
        !parsed || parsed.hostname === 'localhost' || parsed.hostname === '127.0.0.1';
      const basePort = parsed && parsed.port ? Number(parsed.port) : 1973;
      const workerIndex = Number(process.env['PLAYWRIGHT_WORKER_INDEX'] || '0');
      const port = basePort + (isNaN(workerIndex) ? 0 : workerIndex);
      const targetUrl = `http://localhost:${port}/nflow.html`;

      async function probeUrl(target: string, timeoutMs = 1000) {
        let p: URL;
        try {
          p = new URL(target);
        } catch (_e) {
          return false;
        }
        const lib = p.protocol === 'https:' ? https : http;
        return new Promise((resolve) => {
          const req = lib.request(
            {
              method: 'GET',
              host: p.hostname,
              port: p.port,
              path: p.pathname + (p.search || ''),
              timeout: timeoutMs,
            },
            (res) =>
              resolve(Boolean(res.statusCode && res.statusCode >= 200 && res.statusCode < 500))
          );
          req.on('error', () => resolve(false));
          req.on('timeout', () => {
            req.destroy();
            resolve(false);
          });
          req.end();
        });
      }

      let childPid: number | null = null;
      if (isLocalHost) {
        const script = path.join(__dirname, '..', 'static-server.mjs');
        const root = path.resolve(__dirname, '..', '..', '..', 'dist');

        // Reuse a server that already answers on the port.
        const ok = await probeUrl(targetUrl, 500);
        if (!ok) {
          try {
            const env = {
              ...process.env,
              STATIC_SERVER_PORT: String(port),
              STATIC_SERVER_ROOT: root,
            };
            const child = spawn(process.execPath, [script], {
              env,
              stdio: 'ignore',
              detached: true,
            });
            try {
              child.unref();
            } catch (_e) {}
            childPid = child.pid ?? null;
            const deadline = Date.now() + 10_000;
            while (Date.now() < deadline) {
              const ok2 = await probeUrl(targetUrl, 500);
              if (ok2) break;
              await new Promise((r) => setTimeout(r, 200));
            }
          } catch (_e) {
            // ignore spawn failures; tests will report connection errors
          }
        }

        process.env['NFLOW_URL'] = targetUrl;
      }

      try {
        await use({ port, url: targetUrl });
      } finally {
        if (childPid) {
          try {
            process.kill(childPid);
          } catch (_e) {
            // ignore
          }
        }
      }
    },
    { scope: 'worker' },
  ],
  openDialogPath: [SIMPLE_DIAGRAM_PATH, { option: true }],

  mockPage: async ({ page, openDialogPath }, use) => {
    await page.addInitScript(
      ({
        openPath,
        openContent,
        openFileName,
        openFilePath,
        savePath,
        saveFileName,
      }: {
        openPath: string;
        openContent: string;
        openFileName: string;
        openFilePath: string;
        savePath: string;
        saveFileName: string;
      }) => {
        localStorage.setItem('nf-preferences-path', 'C:\\nflow-e2e-preferences');

        // The save/open/chooseDirectory dialogs are the in-window HTML
        // FilePickerDialog. It emits a 'nflow:file-picker' CustomEvent whose
        // detail carries a resolve(path) callback. Resolve it here with a preset
        // path per request mode, so e2e needs no modal clicking; a test can
        // override the path with window.__e2eFilePickerPath.
        function installPickerAutoResolver() {
          const w = window as any;
          if (w.__nflowE2ePickerResolver === true) return;
          w.__nflowE2ePickerResolver = true;
          const dispatchEvent = document.dispatchEvent.bind(document);
          document.dispatchEvent = (event: Event): boolean => {
            if (event.type !== 'nflow:file-picker') return dispatchEvent(event);
            const detail = (event as CustomEvent).detail;
            if (!detail || typeof detail.resolve !== 'function') return dispatchEvent(event);
            let path = w.__e2eFilePickerPath;
            if (typeof path !== 'string') {
              path =
                detail.mode === 'open' ? openFilePath : detail.mode === 'dir' ? openPath : savePath;
            }
            event.preventDefault();
            detail.resolve(path);
            return false;
          };
        }

        function normalizeLibraryPath(value: unknown): string {
          const path = String(value || '')
            .replace(/\//g, '\\')
            .replace(/\\+$/, '');
          return /^[a-zA-Z]:\\/.test(path) ? path.toLowerCase() : path;
        }

        const libraryCatalog: Record<string, Array<Record<string, unknown>>> = {
          'c:\\nflow-libraries': [
            {
              id: 'custom.example',
              title: 'Example Library',
              version: '1.2.3',
              _dir: 'C:\\nflow-libraries\\example',
              blocks: [{ type: 'exampleGain', label: 'Example Gain' }],
            },
          ],
          'c:\\priority-a': [
            {
              id: 'custom.priority',
              title: 'Priority A',
              version: '1.0.0',
              _dir: 'C:\\priority-a\\priority',
              blocks: [{ type: 'priorityBlock', label: 'Priority Block' }],
            },
          ],
          'c:\\priority-b': [
            {
              id: 'custom.priority',
              title: 'Priority B',
              version: '2.0.0',
              _dir: 'C:\\priority-b\\priority',
              blocks: [{ type: 'priorityBlock', label: 'Priority Block' }],
            },
          ],
        };

        function discoverLibraries(paths: string[]) {
          const manifests: Array<Record<string, unknown>> = [];
          const diagnostics: string[] = [];
          const seen = new Set<string>();
          const locations = paths.map((path) => {
            const key = normalizeLibraryPath(path);
            const available = key !== 'c:\\missing';
            const found = libraryCatalog[key] || [];
            if (!available) diagnostics.push(`Library location unavailable: ${path}`);
            for (const manifest of found) {
              const id = String(manifest.id || '');
              if (seen.has(id)) {
                diagnostics.push(`Duplicate library ignored: ${id} (${manifest._dir})`);
              } else {
                seen.add(id);
                manifests.push(manifest);
              }
            }
            return { path, available, libraryCount: found.length };
          });
          return { manifests, diagnostics, locations };
        }

        function installProxy() {
          const w = window as any;

          installPickerAutoResolver();

          if (!w.webui) w.webui = {};
          if (typeof w.webui.call !== 'function') {
            w.webui.call = async () => '';
          }
          if (w.webui.call.__nflowE2eRpcProxy === true) return;

          const originalCall = w.webui.call.bind(w.webui) as (
            fn: string,
            ...args: unknown[]
          ) => Promise<unknown>;

          w.webui.call = async (fn: string, ...args: unknown[]): Promise<unknown> => {
            if (fn === 'rpc' || fn === 'nflow_rpc') {
              try {
                const envelope = JSON.parse(String(args[0] || '{}')) as {
                  id?: number;
                  method?: string;
                  params?: Record<string, unknown>;
                };
                const params = envelope.params || {};
                const ok = (result: unknown) => JSON.stringify({ id: envelope.id, result });
                const fail = (message: string) =>
                  JSON.stringify({ id: envelope.id, error: { message } });

                if (envelope.method === 'nflow.host.mode') {
                  return ok({ mode: 'webview' });
                }

                if (envelope.method === 'nflow.file.openDialog') {
                  return ok(
                    JSON.stringify({
                      success: true,
                      fileName: openFileName,
                      filePath: openFilePath,
                      content: openContent,
                    })
                  );
                }

                if (envelope.method === 'nflow.file.saveDialog') {
                  if (typeof w.__e2eSaveDialogBackend === 'function') {
                    return ok(
                      await w.__e2eSaveDialogBackend(
                        params.typeOfFile || params.type || '',
                        params.content || '',
                        params.defaultName || ''
                      )
                    );
                  }
                  return ok(
                    JSON.stringify({
                      success: true,
                      fileName: saveFileName,
                      filePath: savePath,
                    })
                  );
                }

                if (envelope.method === 'nflow.file.chooseDirectory') {
                  if (typeof w.__e2eChooseDirectoryBackend === 'function') {
                    return ok(await w.__e2eChooseDirectoryBackend());
                  }
                  return ok(JSON.stringify({ success: true, dir: openPath }));
                }

                if (envelope.method === 'nflow.file.readText') {
                  if (String(params.path || '').endsWith('libraries.json')) {
                    const content =
                      localStorage.getItem('nflow-e2e-libraries-config') ||
                      JSON.stringify({ enabledLibraries: [], searchPaths: [] });
                    return ok(JSON.stringify({ success: true, content }));
                  }
                  if (typeof w.__e2eReadTextBackend === 'function') {
                    return ok(await w.__e2eReadTextBackend(params.path || ''));
                  }
                  return ok(
                    JSON.stringify({
                      success: true,
                      content: openContent,
                      filePath: params.path || openFilePath,
                    })
                  );
                }

                if (envelope.method === 'nflow.fs.list') {
                  // The picker is auto-resolved (installPickerAutoResolver), so an
                  // empty listing is enough.
                  return ok(JSON.stringify({ cwd: String(params.path || '.'), entries: [] }));
                }

                if (envelope.method === 'nflow.fs.dialogStart') {
                  const mode = String(params.mode || 'open');
                  const remembered = (w.__e2eDialogDirectories ||= {})[mode];
                  const requested = String(params.defaultPath || '');
                  const directory =
                    requested && requested !== '.' ? requested : remembered || openPath;
                  return ok(JSON.stringify({ directory }));
                }

                if (envelope.method === 'nflow.fs.dialogAccept') {
                  const mode = String(params.mode || 'open');
                  const path = String(params.path || '');
                  const slash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
                  const directory = mode === 'dir' ? path : slash > 0 ? path.slice(0, slash) : '';
                  if (directory) (w.__e2eDialogDirectories ||= {})[mode] = directory;
                  return ok(JSON.stringify({ ok: Boolean(directory) }));
                }

                if (envelope.method === 'nflow.file.writeText') {
                  if (String(params.path || '').endsWith('libraries.json')) {
                    localStorage.setItem(
                      'nflow-e2e-libraries-config',
                      String(params.content || '')
                    );
                    return ok(JSON.stringify({ success: true }));
                  }
                  if (typeof w.__e2eWriteTextBackend === 'function') {
                    return ok(
                      await w.__e2eWriteTextBackend(params.content || '', params.path || '')
                    );
                  }
                  if (typeof w.__e2eSaveDialogBackend === 'function') {
                    const path = String(params.path || savePath);
                    const slash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
                    const fileName = slash >= 0 ? path.slice(slash + 1) : path;
                    return ok(await w.__e2eSaveDialogBackend('', params.content || '', fileName));
                  }
                  return ok(JSON.stringify({ success: true, filePath: params.path || savePath }));
                }

                if (envelope.method === 'nflow.file.loadInitial') {
                  return ok(JSON.stringify({ success: false, canceled: true }));
                }

                if (envelope.method === 'nflow.library.list') {
                  const paths = JSON.parse(
                    localStorage.getItem('nflow-e2e-library-paths') || '[]'
                  ) as string[];
                  return ok(discoverLibraries(paths).manifests);
                }

                if (envelope.method === 'nflow.library.getSearchPaths') {
                  return ok(
                    w.__e2eLibraryPaths ||
                      JSON.parse(localStorage.getItem('nflow-e2e-library-paths') || '[]')
                  );
                }

                if (envelope.method === 'nflow.library.setSearchPaths') {
                  const paths = Array.isArray(params.paths) ? params.paths.map(String) : [];
                  const seen = new Set<string>();
                  w.__e2eLibraryPaths = paths.filter((path: string) => {
                    const key = normalizeLibraryPath(path);
                    if (!key || seen.has(key)) return false;
                    seen.add(key);
                    return true;
                  });
                  localStorage.setItem(
                    'nflow-e2e-library-paths',
                    JSON.stringify(w.__e2eLibraryPaths)
                  );
                  const discovery = discoverLibraries(w.__e2eLibraryPaths);
                  return ok({
                    success: true,
                    paths: w.__e2eLibraryPaths,
                    manifests: discovery.manifests,
                    locations: discovery.locations,
                    diagnostics: discovery.diagnostics,
                    message: 'Library locations scanned.',
                  });
                }

                if (envelope.method === 'nflow.library.load') {
                  return ok({ success: true, path: params.path || '', manifests: [] });
                }

                if (envelope.method === 'nflow.library.unload') {
                  return ok({ success: true, id: params.id || '' });
                }

                if (envelope.method === 'nflow.help.open') {
                  w.__e2eLastHelpTopic = String(params.topic || '');
                  return ok({ success: true, topic: w.__e2eLastHelpTopic });
                }

                if (envelope.method === 'nflow.modelica.info') {
                  return ok(
                    JSON.stringify({
                      available: true,
                      capable: true,
                      omc: 'C:/OpenModelica/bin/omc.exe',
                      version: 'OpenModelica 1.27.0',
                      reason: 'OpenModelica is ready to compile Modelica to FMU.',
                    })
                  );
                }

                if (envelope.method === 'nflow.sim.run') {
                  if (typeof w.__e2eRunSimulationBackend === 'function') {
                    return ok(
                      await w.__e2eRunSimulationBackend(params.diagramJson || params.diagram || '')
                    );
                  }
                }
                if (envelope.method === 'nflow.sim.pause') {
                  if (typeof w.__e2ePauseSimulationBackend === 'function') {
                    return ok(
                      await w.__e2ePauseSimulationBackend(params.pause || params.wantPause || false)
                    );
                  }
                }
                if (envelope.method === 'nflow.sim.stop') {
                  if (typeof w.__e2eStopSimulationBackend === 'function') {
                    return ok(await w.__e2eStopSimulationBackend());
                  }
                }
                if (envelope.method === 'nflow.codegen.generateToDir') {
                  if (typeof w.__e2eGenerateCodeToDirBackend === 'function') {
                    return ok(
                      await w.__e2eGenerateCodeToDirBackend(
                        params.diagramJson || '',
                        params.optionsJson || '{}',
                        params.destinationDirectory || ''
                      )
                    );
                  }
                  return ok(
                    JSON.stringify({
                      success: true,
                      outputDir: params.destinationDirectory || '',
                    })
                  );
                }
                if (envelope.method === 'nflow.clipboard.set') {
                  if (typeof w.__e2eSetClipboardBackend === 'function') {
                    return ok(await w.__e2eSetClipboardBackend(params.data || ''));
                  }
                  w.__e2eClipboardData = String(params.data || '');
                  return ok(JSON.stringify({ success: true }));
                }
                if (envelope.method === 'nflow.clipboard.get') {
                  if (typeof w.__e2eGetClipboardBackend === 'function') {
                    return ok(await w.__e2eGetClipboardBackend());
                  }
                  return ok(w.__e2eClipboardData || '');
                }
                return fail(`unhandled e2e rpc method: ${envelope.method || ''}`);
              } catch (e) {
                return JSON.stringify({
                  id: undefined,
                  error: { message: (e as Error)?.message || String(e) },
                });
              }
            }
            return originalCall(fn, ...args);
          };
          w.webui.call.__nflowE2eRpcProxy = true;
        }

        // Click "Don't Save" whenever the "Save Changes?" modal appears on
        // New/Exit, since tests expect an immediate new diagram.
        (function installAutoDontSave() {
          function tryDismiss() {
            try {
              const modal = document.getElementById('confirmSaveModal');
              const dont = document.getElementById('dontSaveConfirm');
              if (modal && dont && modal.classList.contains('show')) {
                dont.click();
                return true;
              }
            } catch (e) {
              // ignore DOM access errors
            }
            return false;
          }

          const obs = new MutationObserver(() => {
            if (tryDismiss()) obs.disconnect();
          });
          try {
            obs.observe(document, {
              childList: true,
              subtree: true,
              attributes: true,
              attributeFilter: ['class'],
            });
          } catch (_e) {
            // ignore
          }

          const interval = setInterval(() => {
            if (tryDismiss()) clearInterval(interval);
          }, 100);
        })();

        installProxy();
        const proxyTimer = setInterval(installProxy, 50);
        setTimeout(() => clearInterval(proxyTimer), 5000);

        // In-page simulation shim. Tests rely on `nf-sim-start`/`nf-sim-state`
        // events that the native C++ backend normally emits; hostless/static
        // runs have no backend, so answer here.
        (function installSimShim() {
          let running = false;
          let __e2e_sim_ctrl: { cancelled?: boolean; paused?: boolean } | null = null;

          // nflow.sim.run / pause / stop backends: a short session that tests
          // can pause or stop.
          try {
            if (typeof (window as any).__e2eRunSimulationBackend !== 'function') {
              (window as any).__e2eRunSimulationBackend = (diagramJson: string) => {
                __e2e_sim_ctrl = { cancelled: false, paused: false };
                return new Promise<string>((resolve) => {
                  document.dispatchEvent(new CustomEvent('nf-sim-start'));
                  document.dispatchEvent(
                    new CustomEvent('nf-sim-state', { detail: { running: true } })
                  );

                  const startedAt = Date.now();

                  const tick = () => {
                    if (!__e2e_sim_ctrl) return;
                    if (__e2e_sim_ctrl.cancelled) {
                      document.dispatchEvent(new CustomEvent('nf-sim-stop'));
                      resolve(JSON.stringify({ status: 'stopped', scopes: [] }));
                      return;
                    }
                    if (__e2e_sim_ctrl.paused) {
                      setTimeout(tick, 50);
                      return;
                    }
                    // Long enough for tests to observe the running state.
                    if (Date.now() - startedAt > 1500) {
                      document.dispatchEvent(new CustomEvent('nf-sim-stop'));
                      resolve(JSON.stringify({ status: 'done', scopes: [] }));
                      return;
                    }
                    setTimeout(tick, 50);
                  };

                  tick();
                });
              };

              (window as any).__e2ePauseSimulationBackend = async (wantPause: boolean) => {
                if (!__e2e_sim_ctrl) return;
                __e2e_sim_ctrl.paused = !!wantPause;
                document.dispatchEvent(
                  new CustomEvent('nf-sim-state', { detail: { running: !__e2e_sim_ctrl.paused } })
                );
              };

              (window as any).__e2eStopSimulationBackend = async () => {
                if (!__e2e_sim_ctrl) return;
                __e2e_sim_ctrl.cancelled = true;
              };
            }
          } catch (_e) {}

          document.addEventListener('nf-sim-play', () => {
            setTimeout(() => {
              running = true;
              document.dispatchEvent(new CustomEvent('nf-sim-start'));
              document.dispatchEvent(
                new CustomEvent('nf-sim-state', { detail: { running: true } })
              );
            }, 50);
          });
          document.addEventListener('nf-sim-pause', () => {
            setTimeout(() => {
              running = false;
              try {
                if (typeof (window as any).__e2ePauseSimulationBackend === 'function') {
                  (window as any).__e2ePauseSimulationBackend(true);
                }
              } catch (_e) {}
              document.dispatchEvent(
                new CustomEvent('nf-sim-state', { detail: { running: false } })
              );
              document.dispatchEvent(new CustomEvent('nf-sim-stop'));
            }, 50);
          });
        })();
      },
      {
        openPath: openDialogPath,
        openContent: SIMPLE_DIAGRAM_CONTENT,
        openFileName: path.basename(openDialogPath),
        openFilePath: openDialogPath,
        savePath: path.join(
          (typeof process !== 'undefined' && process.env['TEMP']) || '/tmp',
          'nflow-e2e-save.nflow'
        ),
        saveFileName: 'nflow-e2e-save.nflow',
      }
    );

    page.on('console', (msg) => {
      try {
        const text = msg.text();
        // Benign noise from hosts/static server, e.g. optional assets that 404.
        if (typeof text === 'string') {
          const lower = text.toLowerCase();
          if (
            lower.includes('failed to load resource') ||
            lower.includes('favicon') ||
            lower.includes('webui.js') ||
            lower.includes('err_file_not_found')
          ) {
            return;
          }
        }
      } catch (e) {
        // ignore
      }
    });

    page.on('pageerror', (err) => {
      try {
      } catch (e) {
        // ignore
      }
    });

    page.on('requestfailed', (request) => {
      try {
        const failure = request.failure();
        // eslint-disable-next-line no-console
      } catch (e) {
        // ignore
      }
    });

    await use(page);
  },
});
//=============================================================================
export { expect } from '@playwright/test';
//=============================================================================
