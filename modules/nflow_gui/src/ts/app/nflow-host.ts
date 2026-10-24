//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export interface NFlowHost {
  readonly kind: 'native-webui' | 'wasm-worker' | 'test';
  isConnected(): boolean;
  canCall(method: string): boolean;
  call(envelope: string, signal: AbortSignal): Promise<string>;
}
//=============================================================================
interface WebUiGlobal {
  call(name: string, payload: string): Promise<string>;
  isConnected?: () => boolean;
}
//=============================================================================
export interface NativeWebUiGlobal {
  webui?: WebUiGlobal;
  nflow_rpc?: (payload: string) => Promise<string> | string;
  rpc?: (payload: string) => Promise<string> | string;
}
//=============================================================================
export class NativeWebUiHost implements NFlowHost {
  readonly kind = 'native-webui' as const;
  constructor(private readonly globalProvider: () => NativeWebUiGlobal) {}
  isConnected(): boolean {
    const target = this.globalProvider();
    if (typeof target.webui?.call !== 'function') {
      return typeof target.nflow_rpc === 'function' || typeof target.rpc === 'function';
    }
    if (typeof target.webui.isConnected === 'function') return target.webui.isConnected();
    return true;
  }
  //=============================================================================
  canCall(_method: string): boolean {
    return this.hasTransport();
  }
  //=============================================================================
  async call(envelope: string, signal: AbortSignal): Promise<string> {
    if (signal.aborted) throw signal.reason;
    const target = this.globalProvider();
    if (typeof target.webui?.call === 'function') {
      try {
        return await target.webui.call('nflow_rpc', envelope);
      } catch (error) {
        const message = (error as Error | null)?.message || String(error);
        if (!/nflow_rpc|No binding was found|not available/i.test(message)) throw error;
        return await target.webui.call('rpc', envelope);
      }
    }
    if (typeof target.nflow_rpc === 'function') {
      return String(await target.nflow_rpc(envelope));
    }
    if (typeof target.rpc === 'function') return String(await target.rpc(envelope));
    throw new Error('WebUI RPC transport is not connected');
  }
  //=============================================================================
  private hasTransport(): boolean {
    const target = this.globalProvider();
    return (
      typeof target.webui?.call === 'function' ||
      typeof target.nflow_rpc === 'function' ||
      typeof target.rpc === 'function'
    );
  }
}
//=============================================================================
export interface NFlowWasmUiAction {
  type?: string;
  path?: string;
}
//=============================================================================
export interface NFlowWasmWorkerClient {
  simulateNFlow(diagramJson: string): Promise<unknown>;
  /** Engine virtual filesystem (shared by every page of the engine). */
  readFile?(path: string): Promise<Uint8Array>;
  writeFile?(path: string, data: Uint8Array | string): Promise<unknown>;
  listFiles?(): Promise<string[]>;
  listDirectories?(): Promise<string[]>;
  evaluate?(
    code: string,
    onOutput?: (output: { stream?: string; text?: string }) => void
  ): Promise<{ stdout?: string; uiActions?: NFlowWasmUiAction[] }>;
  interrupt(): Promise<unknown>;
  cancelNFlow?(): Promise<unknown>;
  sendNFlowSimEvent?(name: string, data: string): unknown;
  sendNFlowSimCallback?(code: string, blockId: string): unknown;
  onNFlowPartial?(handler: (partial: unknown) => void): () => void;
}
//=============================================================================
export interface NFlowWasmExamplesProvider {
  listExamples(): Promise<unknown>;
  readText(path: string): Promise<string>;
}
//=============================================================================
export interface NFlowWasmFmuProvider {
  pickFile(): Promise<string | null>;
  inspect(path: string): Promise<string>;
  prepareDiagram(json: string): Promise<string>;
}
//=============================================================================
export type NFlowWasmHelpOpener = (url: string) => void;
export type NFlowWasmModelOpener = (path: string) => void;
// The shared example browser runs a script entry as `run('<file>')`; in the
// browser the file is a packaged example script that the host evaluates on
// the shared engine instead of a filesystem path.
const EXAMPLE_SCRIPT_RUN = /^run\('(nflow-example:\/[^']+\.m)'\)$/;
const ENGINE_EXAMPLE_PATH = /^\/modules\/[^/]+\/examples\/(.+\.nflow)$/i;
const WASM_METHODS = new Set([
  'nflow.ping',
  'nflow.host.mode',
  'nflow.sim.run',
  'nflow.sim.stop',
  'nflow.sim.event',
  'nflow.callback.invoke',
]);
//=============================================================================
const HELP_TOPIC_PATTERN = /^(?:block:)?[A-Za-z][A-Za-z0-9_.-]*$/;
const HELP_PAGE_PATTERN = /^\.[/][A-Za-z0-9_-]+(?:[/][A-Za-z0-9_.-]+)+[.]html$/;
const WASM_EXAMPLE_METHODS = new Set(['nflow.examples.list', 'nflow.file.readText']);
// Open / Save / directory browsing of the editor, served by the engine's
// virtual filesystem (`/workspace` is persistent and shared with the desktop).
const WASM_FILE_METHODS = new Set([
  'nflow.file.readText',
  'nflow.file.writeText',
  'nflow.fs.list',
  'nflow.fs.dialogStart',
  'nflow.fs.dialogAccept',
]);
//=============================================================================
// Code generation runs the engine's nflow_codegenerate builtin on the shared
// engine and writes below /workspace; building the result needs a native
// toolchain and stays unavailable in the browser.
const WASM_CODEGEN_METHODS = new Set(['nflow.codegen.generateToDir']);
const CODEGEN_MODEL_DIRECTORY = '/workspace/.nflow-codegen';
const CODEGEN_WARNING_MARKER = '__NFLOW_CODEGEN_WARNING__';
const ENGINE_ROOTS = ['/workspace', '/modules', '/user-modules'];
const ENGINE_WRITABLE_ROOTS = ['/workspace'];
const DEFAULT_ENGINE_DIRECTORY = '/workspace';
function normalizeEnginePath(value: string, roots: readonly string[]): string {
  let path = String(value ?? '')
    .replace(/\\/g, '/')
    .trim();
  if (!path || path === '.') path = DEFAULT_ENGINE_DIRECTORY;
  if (!path.startsWith('/')) path = `${DEFAULT_ENGINE_DIRECTORY}/${path}`;
  path = path.replace(/\/{2,}/g, '/');
  if (path.length > 1 && path.endsWith('/')) path = path.slice(0, -1);
  if (path.split('/').includes('..')) throw new Error(`Invalid engine path: ${value}`);
  if (!roots.some((root) => path === root || path.startsWith(`${root}/`))) {
    throw new Error(`Path is outside the engine filesystem: ${value}`);
  }
  return path;
}
//=============================================================================
function decodeBase64(text: string): Uint8Array {
  const binary = atob(text);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) bytes[i] = binary.charCodeAt(i);
  return bytes;
}
//=============================================================================
export class WasmWorkerHost implements NFlowHost {
  readonly kind = 'wasm-worker' as const;
  private connected = true;
  constructor(
    private readonly client: NFlowWasmWorkerClient,
    private readonly examples?: NFlowWasmExamplesProvider,
    private readonly openHelp?: NFlowWasmHelpOpener,
    private readonly fmus?: NFlowWasmFmuProvider,
    private readonly openModel?: NFlowWasmModelOpener
  ) {}
  //=============================================================================
  private readonly dialogDirectories = new Map<string, string>();
  // Same contract as the native nflow.codegen.generateToDir: {success, dir,
  // warnings?} or {success:false, error}. The builtin writes the sources below
  // <destination>/<model name>/ (model.c, main.c, CMakeLists.txt or a Rust crate).
  private async generateCodeToDir(
    diagramJson: string,
    optionsJson: string,
    destination: string
  ): Promise<Record<string, unknown>> {
    try {
      let options: { lang?: unknown; includeMain?: unknown } = {};
      try {
        options = JSON.parse(optionsJson || '{}') as typeof options;
      } catch {
        throw new Error('Invalid code generation options');
      }
      let diagramName = 'model';
      try {
        const diagram = JSON.parse(diagramJson) as { name?: unknown };
        if (typeof diagram.name === 'string' && diagram.name.trim()) diagramName = diagram.name;
      } catch {
        throw new Error('Invalid diagram JSON');
      }
      const language = String(options.lang ?? 'c').toLowerCase() === 'rust' ? 'rust' : 'C';
      const includeMain = options.includeMain !== false;
      const destinationDirectory = normalizeEnginePath(
        destination || DEFAULT_ENGINE_DIRECTORY,
        ENGINE_WRITABLE_ROOTS
      );
      const stem =
        diagramName
          .replace(/[^A-Za-z0-9_-]+/g, '_')
          .replace(/^_+|_+$/g, '')
          .slice(0, 64) || 'model';
      const modelPath = `${CODEGEN_MODEL_DIRECTORY}/${stem}.nflow`;
      await this.client.writeFile!(modelPath, new TextEncoder().encode(diagramJson));
      const quote = (value: string) => `'${value.replace(/'/g, "''")}'`;
      const code = [
        "nelsonWasmCodegenState = warning('off', 'all');",
        "lastwarn('');",
        'try',
        `  nflow_codegenerate(${quote(modelPath)}, ${quote(destinationDirectory)}, ${
          includeMain ? 'true' : 'false'
        }, ${quote(language)});`,
        '  nelsonWasmCodegenWarning = lastwarn();',
        '  warning(nelsonWasmCodegenState);',
        'catch nelsonWasmCodegenError',
        '  warning(nelsonWasmCodegenState);',
        '  clear nelsonWasmCodegenState;',
        '  rethrow(nelsonWasmCodegenError);',
        'end',
        `disp([${quote(CODEGEN_WARNING_MARKER)}, nelsonWasmCodegenWarning]);`,
        'clear nelsonWasmCodegenState nelsonWasmCodegenWarning;',
      ].join('\n');
      const chunks: string[] = [];
      const evaluated = await this.client.evaluate!(code, (output) => {
        if (output.stream === 'stdout' && typeof output.text === 'string') chunks.push(output.text);
      });
      const output = `${chunks.join('')}${evaluated?.stdout ?? ''}`;
      const match = new RegExp(`${CODEGEN_WARNING_MARKER}(.*)`).exec(output);
      const warning = match?.[1]?.trim() ?? '';
      const result: Record<string, unknown> = {
        success: true,
        dir: `${destinationDirectory}/${stem}`,
      };
      if (warning) result.warnings = [warning];
      // `options.build` is ignored: the browser page hides the option because
      // building needs a native toolchain; the sources are what this host delivers.
      return result;
    } catch (error) {
      return { success: false, error: error instanceof Error ? error.message : String(error) };
    }
  }
  //=============================================================================
  // Packaged examples are read from the gallery provider, everything else
  // from the engine filesystem. A script's modulepath()-based example path
  // (/modules/<module>/examples/...) is mapped to its packaged copy because
  // the example trees are not mounted in the browser engine by default.
  private async readTextFile(
    path: string
  ): Promise<{ success: boolean; content?: string; error?: string }> {
    try {
      if (path.startsWith('nflow-example:/')) {
        if (!this.examples) throw new Error('packaged examples are unavailable');
        return { success: true, content: await this.examples.readText(path) };
      }
      const packaged = ENGINE_EXAMPLE_PATH.test(path)
        ? await this.resolveExampleModelPath(path)
        : null;
      if (packaged && this.examples) {
        return { success: true, content: await this.examples.readText(packaged) };
      }
      if (!this.hasEngineFiles()) throw new Error('engine filesystem is unavailable');
      const bytes = await this.client.readFile!(normalizeEnginePath(path, ENGINE_ROOTS));
      return { success: true, content: new TextDecoder().decode(bytes) };
    } catch (error) {
      return { success: false, error: error instanceof Error ? error.message : String(error) };
    }
  }
  //=============================================================================
  private async writeTextFile(
    path: string,
    content: string
  ): Promise<{ success: boolean; error?: string }> {
    try {
      if (!this.hasEngineFiles()) throw new Error('engine filesystem is unavailable');
      const target = normalizeEnginePath(path, ENGINE_WRITABLE_ROOTS);
      // A "base64:" prefix carries binary payloads (e.g. rasterized exports),
      // as in the native bridge.
      const data = content.startsWith('base64:')
        ? decodeBase64(content.slice('base64:'.length))
        : new TextEncoder().encode(content);
      await this.client.writeFile!(target, data);
      return { success: true };
    } catch (error) {
      return { success: false, error: error instanceof Error ? error.message : String(error) };
    }
  }
  //=============================================================================
  private async listEngineDirectory(path: string): Promise<{
    cwd: string;
    entries: Array<{ name: string; path: string; isDir: boolean; size: number }>;
  }> {
    let cwd = DEFAULT_ENGINE_DIRECTORY;
    try {
      cwd = normalizeEnginePath(path, ENGINE_ROOTS);
    } catch {
      cwd = DEFAULT_ENGINE_DIRECTORY;
    }
    const [files, directories] = await Promise.all([
      this.client.listFiles!(),
      this.client.listDirectories!(),
    ]);
    const prefix = `${cwd}/`;
    const childOf = (full: string): string | null => {
      if (!full.startsWith(prefix)) return null;
      const rest = full.slice(prefix.length);
      return rest && !rest.includes('/') ? rest : null;
    };
    const entries: Array<{ name: string; path: string; isDir: boolean; size: number }> = [];
    const seen = new Set<string>();
    for (const directory of directories) {
      const name = childOf(directory);
      if (name && !seen.has(name)) {
        seen.add(name);
        entries.push({ name, path: directory, isDir: true, size: 0 });
      }
    }
    for (const file of files) {
      const name = childOf(file);
      if (name && !seen.has(name)) {
        seen.add(name);
        entries.push({ name, path: file, isDir: false, size: 0 });
      }
    }
    entries.sort((a, b) => Number(b.isDir) - Number(a.isDir) || a.name.localeCompare(b.name));
    return { cwd, entries };
  }
  //=============================================================================
  // Runs a packaged companion script in the base workspace of the shared engine,
  // then honours the UI actions it emits (open_system() emits `open-nflow`).
  private async runExampleScript(path: string): Promise<void> {
    const source = await this.examples!.readText(path);
    const result = await this.client.evaluate!(source);
    for (const action of result?.uiActions ?? []) {
      if (action?.type !== 'open-nflow' || typeof action.path !== 'string') continue;
      const resolved = await this.resolveExampleModelPath(action.path);
      if (resolved && this.openModel) this.openModel(resolved);
    }
  }
  //=============================================================================
  // Scripts address models through modulepath(), i.e. /modules/<module>/examples/...;
  // the packaged gallery knows them as nflow-example:/<namespace><relative>.
  private async resolveExampleModelPath(enginePath: string): Promise<string | null> {
    if (enginePath.startsWith('nflow-example:/')) return enginePath;
    const match = ENGINE_EXAMPLE_PATH.exec(enginePath);
    if (!match) return null;
    const suffix = `/${match[1]}`;
    const files = await this.exampleFiles();
    return files.find((file) => file.endsWith(suffix)) ?? null;
  }
  //=============================================================================
  private async exampleFiles(): Promise<string[]> {
    if (!this.examples) return [];
    const catalog = (await this.examples.listExamples()) as {
      groups?: Array<{ examples?: Array<{ file?: unknown; path?: unknown }> }>;
      examples?: Array<{ file?: unknown; path?: unknown }>;
    };
    const files: string[] = [];
    const push = (entry: { file?: unknown; path?: unknown } | undefined) => {
      const value = typeof entry?.file === 'string' ? entry.file : entry?.path;
      if (typeof value === 'string') files.push(value);
    };
    for (const group of catalog?.groups ?? [])
      for (const entry of group.examples ?? []) push(entry);
    for (const entry of catalog?.examples ?? []) push(entry);
    return files;
  }
  //=============================================================================
  isConnected(): boolean {
    return this.connected;
  }
  //=============================================================================
  private hasEngineFiles(): boolean {
    return (
      typeof this.client.readFile === 'function' &&
      typeof this.client.writeFile === 'function' &&
      typeof this.client.listFiles === 'function' &&
      typeof this.client.listDirectories === 'function'
    );
  }
  //=============================================================================
  canCall(method: string): boolean {
    return (
      this.connected &&
      (WASM_METHODS.has(method) ||
        (this.hasEngineFiles() && WASM_FILE_METHODS.has(method)) ||
        (this.hasEngineFiles() &&
          typeof this.client.evaluate === 'function' &&
          WASM_CODEGEN_METHODS.has(method)) ||
        (this.fmus !== undefined && ['nflow.fmi3.inspect', 'nflow.fmu.pick'].includes(method)) ||
        (method === 'nflow.help.open' &&
          typeof this.client.evaluate === 'function' &&
          this.openHelp !== undefined) ||
        (this.examples !== undefined && WASM_EXAMPLE_METHODS.has(method)))
    );
  }
  //=============================================================================
  disconnect(): void {
    this.connected = false;
  }
  //=============================================================================
  async call(envelopeText: string, signal: AbortSignal): Promise<string> {
    if (signal.aborted) throw signal.reason;
    if (!this.connected) throw new Error('WebAssembly Worker host is disconnected');
    const envelope = JSON.parse(envelopeText) as {
      id: number;
      method: string;
      params?: Record<string, unknown>;
    };
    if (!this.canCall(envelope.method)) {
      throw new Error(`WebAssembly Worker method is not available: ${envelope.method}`);
    }
    if (envelope.method === 'nflow.ping') {
      return JSON.stringify({ id: envelope.id, result: { pong: true } });
    }
    if (envelope.method === 'nflow.fmu.pick') {
      return JSON.stringify({ id: envelope.id, result: await this.fmus!.pickFile() });
    }
    if (envelope.method === 'nflow.fmi3.inspect') {
      return JSON.stringify({
        id: envelope.id,
        result: await this.fmus!.inspect(String(envelope.params?.path || '')),
      });
    }
    if (envelope.method === 'nflow.host.mode') {
      return JSON.stringify({ id: envelope.id, result: { mode: 'wasm' } });
    }
    if (envelope.method === 'nflow.sim.stop') {
      if (this.client.cancelNFlow) await this.client.cancelNFlow();
      else await this.client.interrupt();
      return JSON.stringify({ id: envelope.id, result: { stopped: true } });
    }
    if (envelope.method === 'nflow.sim.event') {
      const name = String(envelope.params?.name ?? '');
      const data = String(envelope.params?.data ?? '');
      const delivered = this.client.sendNFlowSimEvent
        ? this.client.sendNFlowSimEvent(name, data)
        : { delivered: false };
      return JSON.stringify({ id: envelope.id, result: delivered ?? { delivered: true } });
    }
    if (envelope.method === 'nflow.callback.invoke') {
      const code = String(envelope.params?.code ?? '');
      const blockId = String(envelope.params?.blockId ?? '');
      const script = EXAMPLE_SCRIPT_RUN.exec(code.trim());
      if (script && this.examples && typeof this.client.evaluate === 'function') {
        await this.runExampleScript(script[1]);
        return JSON.stringify({ id: envelope.id, result: { delivered: true, script: script[1] } });
      }
      // dashboardCallbackButton: run a Nelson snippet during the running sim.
      const delivered = this.client.sendNFlowSimCallback
        ? this.client.sendNFlowSimCallback(code, blockId)
        : { delivered: false };
      return JSON.stringify({ id: envelope.id, result: delivered ?? { delivered: true } });
    }
    if (envelope.method === 'nflow.help.open') {
      const topic = String(envelope.params?.topic ?? '').trim();
      if (!HELP_TOPIC_PATTERN.test(topic)) throw new Error('invalid documentation topic');
      const page = String(envelope.params?.page ?? '').trim();
      if (page && (!HELP_PAGE_PATTERN.test(page) || page.includes('..'))) {
        throw new Error('invalid documentation page');
      }
      if (!this.client.evaluate || !this.openHelp) {
        throw new Error('documentation is not available');
      }
      const escapedTopic = topic.replace(/'/g, "''");
      const escapedPage = page.replace(/'/g, "''");
      const code = [
        "nflowWasmPreviousDocDryRun = getenv('NELSON_DOC_DRY_RUN');",
        "nflowWasmPreviousDocPage = getenv('NELSON_DOC_BLOCK_PAGE');",
        "setenv('NELSON_DOC_DRY_RUN', 'TRUE');",
        ...(page ? [`setenv('NELSON_DOC_BLOCK_PAGE', '${escapedPage}');`] : []),
        'try',
        `  doc('${escapedTopic}');`,
        'catch exception',
        "  setenv('NELSON_DOC_DRY_RUN', nflowWasmPreviousDocDryRun);",
        "  setenv('NELSON_DOC_BLOCK_PAGE', nflowWasmPreviousDocPage);",
        '  clear nflowWasmPreviousDocDryRun;',
        '  clear nflowWasmPreviousDocPage;',
        '  rethrow(exception);',
        'end',
        "setenv('NELSON_DOC_DRY_RUN', nflowWasmPreviousDocDryRun);",
        "setenv('NELSON_DOC_BLOCK_PAGE', nflowWasmPreviousDocPage);",
        'clear nflowWasmPreviousDocDryRun;',
        'clear nflowWasmPreviousDocPage;',
      ].join('\n');
      const outputChunks: string[] = [];
      let result: { stdout?: string };
      try {
        result = await this.client.evaluate(code, (output) => {
          if (output.stream === 'stdout' && typeof output.text === 'string') {
            outputChunks.push(output.text);
          }
        });
      } finally {
        // Do not leave the dry-run switch enabled if evaluation itself fails.
        await this.client
          .evaluate(
            "setenv('NELSON_DOC_DRY_RUN', ''); setenv('NELSON_DOC_BLOCK_PAGE', ''); clear nflowWasmPreviousDocDryRun; clear nflowWasmPreviousDocPage;"
          )
          .catch(() => undefined);
      }
      const output = `${outputChunks.join('')}\n${
        typeof result.stdout === 'string' ? result.stdout : ''
      }`;
      const url = output
        .split(/\r?\n/)
        .map((line) => line.trim())
        .find((line) => /^(?:https?:\/\/|file:\/\/)/i.test(line));
      if (!url) throw new Error('documentation URL was not returned');
      this.openHelp(url);
      return JSON.stringify({ id: envelope.id, result: { success: true, topic, url } });
    }
    if (envelope.method === 'nflow.examples.list') {
      const result = await this.examples!.listExamples();
      return JSON.stringify({ id: envelope.id, result });
    }
    if (envelope.method === 'nflow.file.readText') {
      const path = String(envelope.params?.path ?? envelope.params?.filePath ?? '');
      const result = JSON.stringify(await this.readTextFile(path));
      return JSON.stringify({ id: envelope.id, result });
    }
    if (envelope.method === 'nflow.file.writeText') {
      const path = String(envelope.params?.path ?? envelope.params?.filePath ?? '');
      const content = String(envelope.params?.content ?? '');
      const result = JSON.stringify(await this.writeTextFile(path, content));
      return JSON.stringify({ id: envelope.id, result });
    }
    if (envelope.method === 'nflow.codegen.generateToDir') {
      const result = await this.generateCodeToDir(
        String(envelope.params?.diagramJson ?? ''),
        String(envelope.params?.optionsJson ?? '{}'),
        String(envelope.params?.destinationDirectory ?? envelope.params?.destDir ?? '')
      );
      return JSON.stringify({ id: envelope.id, result: JSON.stringify(result) });
    }
    if (envelope.method === 'nflow.fs.list') {
      const path = String(envelope.params?.path ?? envelope.params?.dir ?? '');
      const result = JSON.stringify(await this.listEngineDirectory(path));
      return JSON.stringify({ id: envelope.id, result });
    }
    if (envelope.method === 'nflow.fs.dialogStart') {
      const mode = String(envelope.params?.mode ?? 'open');
      const requested = String(envelope.params?.defaultPath ?? '');
      let directory = this.dialogDirectories.get(mode) ?? DEFAULT_ENGINE_DIRECTORY;
      try {
        if (requested && requested !== '.')
          directory = normalizeEnginePath(requested, ENGINE_ROOTS);
      } catch {
        // keep the remembered directory
      }
      return JSON.stringify({ id: envelope.id, result: JSON.stringify({ directory }) });
    }
    if (envelope.method === 'nflow.fs.dialogAccept') {
      const mode = String(envelope.params?.mode ?? 'open');
      let ok = false;
      try {
        const selected = normalizeEnginePath(String(envelope.params?.path ?? ''), ENGINE_ROOTS);
        const directory =
          mode === 'dir' ? selected : selected.slice(0, selected.lastIndexOf('/')) || '/';
        this.dialogDirectories.set(mode, directory);
        ok = true;
      } catch {
        ok = false;
      }
      return JSON.stringify({ id: envelope.id, result: JSON.stringify({ ok }) });
    }
    let diagramJson = String(envelope.params?.diagramJson ?? '{}');
    if (this.fmus) diagramJson = await this.fmus.prepareDiagram(diagramJson);
    const result = await this.client.simulateNFlow(diagramJson);
    return JSON.stringify({ id: envelope.id, result });
  }
}
//=============================================================================
