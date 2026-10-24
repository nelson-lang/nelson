//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type {
  NelsonEvaluationResult,
  NelsonFigureFrame,
  NelsonWorkerClientApi,
} from '@nelson/wasm-client';
import { emitLocalPush, emitLocalPushValue, type LocalRpcBackend } from '../rpc/client';
import { isMatrixOnlyFrame, mergeMatrixOnlyFrame } from './figureFrameCache';
import { setWasmFigureView } from './figureView';
import type {
  DrawPrimitive,
  FigureDisplayListResult,
  FigureImageResult,
  FsEntry,
} from '../rpc/types';
import type {
  BrowserManifest,
  RuntimeCapabilityManifest,
  StartupDownloadPlan,
} from './runtimeManifest';
import { resolveVersionedAssetUrl } from './runtimeUrls';
import {
  clearWasmHistoryForSession,
  defaultHistoryStorage,
  loadWasmHistoryForSession,
  saveWasmHistory,
  type WasmHistoryStorage,
} from './historyStore';
import {
  WASM_PREFERENCES_PATH,
  WasmPreferencesService,
  WasmProfilerService,
} from './wasmDesktopServices';
import { WasmNewsfeedService } from './wasmNewsfeed';
import { WasmPackageManagerServices } from './wasmPackageManagerServices';
import { handleWasmAudioAction, type WasmAudioAction } from './webAudio';
import { detectBrowserRuntimeDiagnostics } from './browserCapabilities';
import {
  recordWasmFigureFlush,
  recordWasmFigureImagePayload,
  recordWasmFigureReceived,
} from './wasmFigureProfile';
import { resolveSelectionType } from '../panels/figureMouseEvents';
//=============================================================================
const WORKSPACE = '/workspace';
const DEFAULT_NMM_REGISTRY =
  'https://raw.githubusercontent.com/nelson-lang/nelson-modules-registry/master/registry.json';
const NMM_REGISTRY_FILE = `${WORKSPACE}/.nmm-registry/registry.json`;
function isWasmAudioAction(action: unknown): action is WasmAudioAction {
  const type = (action as { type?: unknown })?.type;
  return (
    type === 'play-audio' ||
    type === 'audio-buffer-begin' ||
    type === 'audio-buffer-chunk' ||
    type === 'audio-buffer-play' ||
    type === 'audio-control'
  );
}
//=============================================================================
function parseDocCommandTopic(line: string): string | null | undefined {
  const trimmed = line.trim().replace(/;+\s*$/, '');
  if (!/^doc\b/i.test(trimmed)) return undefined;
  const call = /^doc\s*\(\s*(['"])(.*?)\1\s*\)$/i.exec(trimmed);
  if (call) return call[2].trim();
  const command = /^doc\b(.*)$/i.exec(trimmed);
  if (!command) return undefined;
  const topic = command[1].trim();
  if (!topic) return '';
  // Package-qualified builtins (for example tessera.dla) are valid topics too.
  // The grammar stays narrow so an arbitrary expression is still evaluated by the
  // interpreter.
  if (/^[A-Za-z][A-Za-z0-9_]*(?:\.[A-Za-z][A-Za-z0-9_]*)*$/.test(topic)) return topic;
  return undefined;
}
//=============================================================================
// The worker file API exposes the persistent virtual workspace and module trees, not
// the host temporary directory, so the generated help cache lives in the workspace.
const WASM_HELP_CACHE_ROOT = '/workspace/.nelson-wasm-help';
function normalizeVirtualPath(path: string): string {
  const parts: string[] = [];
  for (const part of path.split('/')) {
    if (!part || part === '.') continue;
    if (part === '..') {
      parts.pop();
      continue;
    }
    parts.push(part);
  }
  return `/${parts.join('/')}`;
}
//=============================================================================
function resolveVirtualPath(baseFile: string, relative: string): string {
  if (relative.startsWith('/')) return normalizeVirtualPath(relative);
  const base = baseFile.slice(0, baseFile.lastIndexOf('/'));
  return normalizeVirtualPath(`${base}/${relative}`);
}
//=============================================================================
function dataUrl(path: string, bytes: Uint8Array): string {
  let binary = '';
  const chunkSize = 0x8000;
  for (let offset = 0; offset < bytes.length; offset += chunkSize) {
    binary += String.fromCharCode(...bytes.subarray(offset, offset + chunkSize));
  }
  const mime = /\.css$/i.test(path)
    ? 'text/css'
    : /\.js$/i.test(path)
    ? 'text/javascript'
    : /\.svg$/i.test(path)
    ? 'image/svg+xml'
    : /\.png$/i.test(path)
    ? 'image/png'
    : /\.jpe?g$/i.test(path)
    ? 'image/jpeg'
    : 'application/octet-stream';
  return `data:${mime};base64,${btoa(binary)}`;
}
//=============================================================================
function emitPromptAfterCurrentRpc(): void {
  window.setTimeout(() => emitLocalPush('term.prompt', { prompt: '>> ' }), 0);
}
//=============================================================================
export interface WasmDesktopOptions {
  nflowUrl?: string;
  packageManagerUrl?: string;
  examplesUrl?: string;
  runtimeResourcesUrl?: string;
  runtimeCapabilities?: RuntimeCapabilityManifest;
  deploymentManifest?: BrowserManifest;
  downloadPlan?: StartupDownloadPlan;
  openExternal?: (url: string) => void;
  historyStorage?: WasmHistoryStorage | null;
  preferencesStorage?: WasmHistoryStorage | null;
}
//=============================================================================
interface ExamplesCatalogEntry {
  title?: string;
  description?: string;
  kind?: string;
  file?: string;
  command?: string;
  tags?: string[];
  requirements?: string[];
}
//=============================================================================
interface ExamplesCatalogGroup {
  name: string;
  title: string;
  source: 'nelson' | 'nflow' | 'external';
  examples: ExamplesCatalogEntry[];
}
//=============================================================================
interface ExamplesCatalog {
  groups: ExamplesCatalogGroup[];
  diagnostics: unknown[];
  locale: string;
}
//=============================================================================
interface RuntimeResourceSelection {
  bundles: string[];
  bundleFileCounts: Map<string, number>;
  files: string[];
}
//=============================================================================
type ImagePrimitive = Extract<DrawPrimitive, { kind: 'image' }>;
interface RetainedRgbaImage {
  rgbaBytes: ArrayBuffer;
  imageWidth?: number;
  imageHeight?: number;
}
//=============================================================================
function displayList(frame: NelsonFigureFrame): FigureDisplayListResult {
  return { ...(frame.displayList as FigureDisplayListResult), handle: frame.handle };
}
//=============================================================================
function ensureTrailingNewline(text: string): string {
  return !text || text.endsWith('\n') ? text : `${text}\n`;
}
//=============================================================================
function base64FromBytes(bytes: Uint8Array): string {
  let binary = '';
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary);
}
//=============================================================================
function bytesFromBase64(value: string): Uint8Array {
  return Uint8Array.from(atob(value), (character) => character.charCodeAt(0));
}
//=============================================================================
function localizedText(value: unknown, locale: string): string {
  if (typeof value === 'string') return value;
  if (!value || typeof value !== 'object' || Array.isArray(value)) return '';
  const table = value as Record<string, unknown>;
  const preferred = table[locale] ?? table.en_US ?? table.fr_FR;
  return typeof preferred === 'string' ? preferred : '';
}
//=============================================================================
const NFLOW_MODEL_CHANNEL = 'nelson-wasm-nflow';
const NFLOW_MODEL_ACK_TIMEOUT_MS = 400;
function browserChannelName(prefix: string): string {
  const uuid = globalThis.crypto?.randomUUID?.();
  const suffix = uuid ?? `${Date.now().toString(36)}-${Math.random().toString(36).slice(2)}`;
  return `${prefix}-${suffix}`;
}
//=============================================================================
export class WasmDesktopBackend implements LocalRpcBackend {
  private static nextEvaluationPromptToken = 0;
  private static nextRuntimeResourceProgressId = 0;
  private readonly figures = new Map<number, FigureDisplayListResult>();
  private readonly figureTitles = new Map<number, string>();
  private removeFigureListListener: (() => void) | null = null;
  private readonly pendingFigureFrames = new Map<number, NelsonFigureFrame>();
  private readonly pendingFigureFrameReceivedAt = new Map<number, number>();
  // Notebook owns its figures without opening regular desktop figure panels.
  // Keep this state in the shared Wasm backend so the browser path follows the
  // same lifecycle as the native WebGuiRpc implementation.
  private readonly notebookFigures = new Set<number>();
  private readonly notebookCapturedHandles = new Set<number>();
  private readonly notebookBaseline = new Set<number>();
  private notebookCapture = false;
  private notebookInteractive = false;
  // Worker messages for a display list can be delivered just after the
  // evaluate response. Keep a short hand-off window so those frames stay in
  // the Notebook output instead of opening a regular Figure panel.
  private notebookGraceUntil = 0;
  private readonly notebookGraceBaseline = new Set<number>();
  // A response and its figure frame are separate Worker messages. After a
  // Notebook cell returns, a late frame must not create a desktop Figure
  // tab; the explicit "open in figure window" action remains the only way to
  // release a Notebook figure into the desktop lifecycle.
  private notebookSuppressExternalFrames = false;
  private readonly figureImages = new Map<string, string>();
  private readonly figureRgbaImages = new Map<string, RetainedRgbaImage>();
  private readonly history: string[];
  private readonly historyStorage: WasmHistoryStorage | null;
  private readonly preferencesStorage: WasmHistoryStorage | null;
  private readonly preferences: WasmPreferencesService;
  private readonly profiler: WasmProfilerService;
  private readonly newsfeed: WasmNewsfeedService;
  private readonly packageManagerServices: WasmPackageManagerServices | null;
  private cwd = WORKSPACE;
  private removeFigureListener: (() => void) | null;
  private removeAudioListener: (() => void) | null;
  private removeDebuggerListener: (() => void) | null;
  private removeInputListener: (() => void) | null;
  private removeFilesChangedListener: (() => void) | null = null;
  private examplesChannel: BroadcastChannel | null = null;
  private packageManagerChannel: BroadcastChannel | null = null;
  private examplesCatalog: ExamplesCatalog | null = null;
  private runtimeResourcePromises = new Map<string, Promise<void>>();
  private runtimeResourcesIndexPromise: Promise<unknown> | null = null;
  private runtimeResourceAbortController: AbortController | null = null;
  private mountedRuntimeResourceBundles = new Set<string>();
  private mountedRuntimeResourceFiles = new Set<string>();
  private browserRegistryUrl = '';
  private browserRegistryReady = false;
  private evaluationInProgress = false;
  private evaluationInWorker = false;
  private evaluationGeneration = 0;
  private externalEvaluationPreparationToken: number | null = null;
  private exampleRunAcknowledgement: (() => void) | null = null;
  private figureFlushHandle: number | null = null;
  private figureFlushUsesAnimationFrame = false;
  private readonly examplesChannelName = browserChannelName('nelson-examples');
  private readonly packageManagerChannelName = browserChannelName('nelson-nmm');
  private isInterpreterBusy(): boolean {
    return (
      this.evaluationInProgress ||
      this.externalEvaluationPreparationToken !== null ||
      this.exampleRunAcknowledgement !== null
    );
  }
  //=============================================================================
  constructor(
    private readonly client: NelsonWorkerClientApi,
    private readonly options: WasmDesktopOptions = {}
  ) {
    this.historyStorage =
      options.historyStorage === undefined ? defaultHistoryStorage() : options.historyStorage;
    this.preferencesStorage =
      options.preferencesStorage === undefined
        ? defaultHistoryStorage()
        : options.preferencesStorage;
    this.history = loadWasmHistoryForSession(this.historyStorage);
    this.preferences = new WasmPreferencesService(this.preferencesStorage);
    this.profiler = new WasmProfilerService(client);
    this.newsfeed = new WasmNewsfeedService();
    this.packageManagerServices = options.packageManagerUrl
      ? new WasmPackageManagerServices(options.packageManagerUrl, options.openExternal)
      : null;
    this.removeFigureListener = client.onFigure((frame) => this.receiveFigure(frame));
    this.removeFigureListListener =
      client.onFigureList?.((figures) => this.reconcileFigureList(figures)) ?? null;
    this.removeAudioListener = client.onAudioAction((action) => {
      if (!isWasmAudioAction(action)) return;
      void handleWasmAudioAction(action).catch((error) => {
        const message = error instanceof Error ? error.message : String(error);
        emitLocalPush('term.output', {
          text: `\u001b[31mWeb Audio playback failed: ${ensureTrailingNewline(message)}\u001b[0m`,
        });
      });
    });
    this.removeDebuggerListener = client.onDebuggerState((state) =>
      emitLocalPush('debugger.state', state)
    );
    this.removeInputListener = client.onInputRequest((request) => {
      emitLocalPush('term.inputRequest', request);
    });
    // Files written by another page of the shared engine (NFlow save, code
    // generation, ...) are not seen by this page's own file operations.
    this.removeFilesChangedListener =
      client.onFilesChanged?.(() => emitLocalPush('fs.changed', { path: WORKSPACE })) ?? null;
    if (this.options.examplesUrl && typeof BroadcastChannel === 'function') {
      this.examplesChannel = new BroadcastChannel(this.examplesChannelName);
      this.examplesChannel.onmessage = (event) => void this.handleExamplesMessage(event.data);
    }
    if (this.options.packageManagerUrl && typeof BroadcastChannel === 'function') {
      this.packageManagerChannel = new BroadcastChannel(this.packageManagerChannelName);
      this.packageManagerChannel.onmessage = (event) =>
        void this.handlePackageManagerMessage(event.data);
    }
  }
  //=============================================================================
  dispose(): void {
    this.removeFilesChangedListener?.();
    this.removeFilesChangedListener = null;
    this.removeFigureListener?.();
    this.removeFigureListener = null;
    this.removeFigureListListener?.();
    this.removeAudioListener?.();
    this.removeAudioListener = null;
    this.removeDebuggerListener?.();
    this.removeDebuggerListener = null;
    this.removeInputListener?.();
    this.removeInputListener = null;
    this.cancelPendingFigureFlush();
    this.examplesChannel?.close();
    this.examplesChannel = null;
    this.packageManagerChannel?.close();
    this.packageManagerChannel = null;
    this.client.terminate();
  }
  //=============================================================================
  async handle(
    method: string,
    params: Record<string, unknown>,
    _signal: AbortSignal
  ): Promise<unknown> {
    switch (method) {
      case 'ping': {
        const buildId = this.options.deploymentManifest?.buildId;
        return {
          pong: true,
          backend: 'WebAssembly Worker',
          version:
            typeof buildId === 'string' && buildId ? `portable (build ${buildId})` : 'portable',
        };
      }
      case 'app.heartbeat':
        return { ok: true };
      case 'app.capabilities':
        return {
          nflow:
            Boolean(this.options.nflowUrl) &&
            (this.options.runtimeCapabilities?.features.nflow ?? true),
          packageManager:
            Boolean(this.options.packageManagerUrl) &&
            (this.options.runtimeCapabilities?.capabilities.includes('package-manager-core') ??
              true),
          launchMode: 'browser',
        };
      case 'diagnostics.info':
        return this.diagnosticsInfo();
      case 'client.role':
        return { role: 'desktop', launchMode: 'browser' };
      case 'client.ready':
        return { ok: true };
      case 'app.openPackageManager':
        if (!this.options.packageManagerUrl) throw new Error('Package Manager is unavailable');
        emitLocalPush('tool.show', { tool: 'nmm', args: {} });
        return { opened: true };
      case 'tool.dispatch':
        return this.dispatchWebTool(params);
      case 'prefs.load':
        return this.preferences.load();
      case 'prefs.save':
        return this.preferences.save(params.values);
      case 'term.input':
        return this.evaluate(String(params.line ?? ''));
      case 'term.inputResponse':
        return this.client.respondInput(Number(params.id), String(params.value ?? ''));
      case 'term.interrupt':
        return this.interrupt(params.busy === true);
      case 'term.resize':
        return { ok: true };
      case 'term.sync':
        if (this.isInterpreterBusy()) return { ok: true };
        return { prompt: '>> ' };
      case 'notebook.run':
        return this.runNotebook(params);
      case 'notebook.closeFigures':
        return this.closeNotebookFigures(params);
      case 'notebook.releaseFigure':
        return this.releaseNotebookFigure(params);
      case 'completion.request':
        return this.complete(String(params.line ?? ''));
      case 'history.list':
        return { entries: this.history.slice() };
      case 'history.clear':
        this.history.splice(
          0,
          this.history.length,
          ...clearWasmHistoryForSession(this.historyStorage)
        );
        emitLocalPush('history.changed', {});
        return { ok: true };
      case 'history.remove':
        this.history.splice(Number(params.index), 1);
        saveWasmHistory(this.historyStorage, this.history);
        emitLocalPush('history.changed', {});
        return { ok: true };
      case 'workspace.list':
        return this.listWorkspace();
      case 'var.open':
        return this.client.openVariable(String(params.name ?? ''));
      case 'var.getBlock':
        return this.client.getVariableBlock(
          String(params.name ?? ''),
          Number(params.row0),
          Number(params.col0),
          Number(params.rows),
          Number(params.cols)
        );
      case 'var.replace':
        return this.replaceVariable(String(params.name ?? ''), params.values as unknown[][]);
      case 'var.setNested':
        return this.setNestedVariable(params);
      case 'fs.cwd':
        return this.cwdResult();
      case 'fs.chdir':
        return this.changeDirectory(String(params.path ?? ''));
      case 'fs.list':
        return this.listDirectory(String(params.path || this.cwd));
      case 'fs.upload':
        return this.upload(String(params.path ?? ''), String(params.data ?? ''));
      case 'fs.download':
        return this.download(String(params.path ?? ''));
      case 'fs.create':
        return this.createFile(params);
      case 'fs.rename':
        return this.renamePath(String(params.from ?? ''), String(params.to ?? ''));
      case 'fs.delete':
        return this.deletePath(String(params.path ?? ''));
      case 'editor.open':
        return this.openEditor(String(params.path ?? ''));
      case 'editor.save':
        return this.saveEditor(String(params.path ?? ''), String(params.content ?? ''));
      case 'code.analyze':
        return this.client.analyzeCode(String(params.path ?? ''), String(params.source ?? ''));
      case 'profile.info':
        return this.profiler.info();
      case 'profile.control':
        return this.profiler.control(String(params.action ?? ''));
      case 'profile.export':
        return this.profileExport(String(params.path ?? ''));
      case 'help.text':
        return this.helpText(String(params.name ?? ''));
      case 'help.document':
        return this.helpDocument(String(params.name ?? ''), String(params.path ?? ''));
      case 'newsfeed.list':
        return this.newsfeed.list(params.url);
      case 'examples.list':
        return this.loadExamplesCatalog();
      case 'examples.refresh':
        this.examplesCatalog = null;
        return this.loadExamplesCatalog();
      case 'examples.open':
        return this.dispatchExamplesRequest('open', params);
      case 'examples.run':
        return this.dispatchExamplesRequest('run', params);
      case 'debugger.state':
        return this.client.debuggerState();
      case 'debugger.getBreakpoints':
        return this.client.debuggerGetBreakpoints(String(params.file ?? ''));
      case 'debugger.toggleBreakpoint':
        return this.client.debuggerToggleBreakpoint(
          String(params.file ?? ''),
          Number(params.line) || 0
        );
      case 'fs.dialog.native':
        return { available: false, reason: 'browser' };
      case 'fs.dialog.start':
        return { directory: String(params.defaultPath || this.cwd) };
      case 'fs.dialog.accept':
        return { ok: true };
      case 'figure.list':
        return this.figureList();
      case 'figure.action': {
        const handle = this.figureHandle(Number(params.handle));
        const request = params.request as Record<string, unknown>;
        if (!request || typeof request.action !== 'string')
          throw new Error('Invalid figure action');
        if (this.isInterpreterBusy()) {
          if (request.action === 'export')
            throw new Error('Export is available when execution has finished.');
          if (request.action === 'state') return { queued: true };
          this.client.queueCommand(
            `__web_figure_deferred_action__(${handle}, ${this.nelsonText(
              JSON.stringify(request)
            )});`
          );
          return { queued: true };
        }
        const result = (await this.client.figureAction(handle, request)) as {
          displayList?: FigureDisplayListResult;
        };
        if (result.displayList) {
          result.displayList.handle = handle;
          this.cacheFigure(handle, this.hydrateFigureImages(result.displayList));
          // Notebook owns its figures in an inline panel.  The action result
          // is applied directly by that panel, so publishing it as a regular
          // figure frame is both redundant and dangerous: the shared desktop
          // lifecycle interprets an unseen frame as a request to open a new
          // Figure tab.  It also schedules an extra state refresh for every
          // rotation gesture, making the interaction noticeably slower.
          if (!this.notebookFigures.has(handle)) {
            emitLocalPushValue('figure.frame', { handle, displayList: result.displayList });
          }
        }
        return result;
      }
      case 'figure.getDisplayList':
        return this.getFigure(Number(params.handle));
      case 'figure.getImage':
        return this.getFigureImage(Number(params.handle));
      case 'figure.saveAs':
        return this.saveFigure(Number(params.handle), String(params.path ?? ''));
      case 'figure.setView':
        return this.setFigureView(params);
      case 'figure.setSize':
        return this.setFigureSize(params);
      case 'figure.pan':
        return this.panFigure(params);
      case 'figure.keyEvent':
        return this.sendFigureKeyEvent(params);
      case 'figure.mouseEvent':
        return this.sendFigureMouseEvent(params);
      case 'figure.setPosition':
      case 'figure.setWindowSize':
        return { ok: false };
      case 'uicontrol.action':
        return this.sendUIControlAction(
          Number(params.handle),
          Number(params.value),
          String(params.text ?? '')
        );
      case 'figure.close':
        return this.closeFigure(Number(params.handle));
      case 'uihtml.pull':
        return { handle: Number(params.handle), dataJson: '[]', revision: 0, events: [] };
      default:
        throw new Error(`RPC method "${method}" is unavailable in the WebAssembly desktop`);
    }
  }
  //=============================================================================
  private diagnosticsInfo(): Record<string, unknown> {
    const capabilities = this.options.runtimeCapabilities;
    const manifest = this.options.deploymentManifest;
    const files = Array.isArray(manifest?.files) ? manifest.files.length : undefined;
    const totalBytes = Number.isSafeInteger(manifest?.totalBytes)
      ? Number(manifest?.totalBytes)
      : undefined;
    return {
      backend: { name: 'WebAssembly Worker', version: 'portable', launchMode: 'browser' },
      runtime: {
        name: 'webassembly',
        profile: capabilities?.profile,
        buildId: typeof manifest?.buildId === 'string' ? manifest.buildId : undefined,
      },
      delivery: {
        files,
        totalBytes,
        startupBytes: this.options.downloadPlan?.startupBytes,
        onDemandBytes: this.options.downloadPlan?.onDemandBytes,
      },
      capabilities: capabilities
        ? {
            modules: capabilities.modules,
            capabilities: capabilities.capabilities,
            features: capabilities.features,
            limits: capabilities.limits,
            unavailable: capabilities.unavailable,
          }
        : undefined,
      persistence: {
        commandHistory: this.historyStorage !== null,
        preferencesPath: this.preferencesStorage !== null ? WASM_PREFERENCES_PATH : '',
        workspacePath: WORKSPACE,
      },
      browser: detectBrowserRuntimeDiagnostics(),
    };
  }
  //=============================================================================
  private async evaluate(line: string): Promise<{ accepted: boolean }> {
    if (!line.trim()) {
      emitLocalPush('term.prompt', { prompt: '>> ' });
      return { accepted: true };
    }
    this.history.push(line);
    saveWasmHistory(this.historyStorage, this.history);
    emitLocalPush('history.changed', {});
    const docTopic = parseDocCommandTopic(line);
    if (docTopic !== undefined) {
      emitLocalPush('help.show', { topic: docTopic });
      emitPromptAfterCurrentRpc();
      return { accepted: true };
    }
    if (this.evaluationInProgress) {
      this.client.queueCommand(line);
      return { accepted: true };
    }
    // A regular command starts a new desktop figure lifecycle. Frames from a
    // preceding Notebook run have already had their short hand-off window;
    // any new figure created by this command must be visible normally.
    this.notebookSuppressExternalFrames = false;
    const generation = ++this.evaluationGeneration;
    const promptToken = ++WasmDesktopBackend.nextEvaluationPromptToken;
    this.evaluationInProgress = true;
    emitLocalPush('term.busy', { token: promptToken });
    try {
      await this.materializeBrowserExamplesReferencedByLine(line);
      if (this.commandNeedsRuntimeResources(line))
        await this.ensureRuntimeResourcesMountedForLine(line);
      if (this.evaluationGeneration !== generation) return { accepted: true };
      this.evaluationInWorker = true;
      const result = await this.client.evaluate(line, (output) => {
        if (output.clear) {
          emitLocalPush('term.clear', {});
          return;
        }
        if (output.html !== undefined) {
          emitLocalPush('html.show', { html: output.html });
          return;
        }
        if (output.expandable) {
          emitLocalPush('term.expandable', output.expandable);
          return;
        }
        if (!output.text) return;
        emitLocalPush('term.output', {
          text: output.stream === 'stderr' ? `\u001b[31m${output.text}\u001b[0m` : output.text,
        });
      });
      this.evaluationInWorker = false;
      this.writeEvaluationOutput(result);
      emitLocalPush('workspace.changed', {});
    } catch (error) {
      this.evaluationInWorker = false;
      if (this.evaluationGeneration !== generation) return { accepted: true };
      const message = error instanceof Error ? error.message : String(error);
      emitLocalPush('term.output', { text: `\u001b[31m${message}\u001b[0m\n` });
    } finally {
      this.evaluationInWorker = false;
      if (this.evaluationGeneration === generation) {
        this.evaluationInProgress = false;
        this.acknowledgeExampleRun();
        emitLocalPush('term.prompt', { prompt: '>> ', token: promptToken });
      }
    }
    return { accepted: true };
  }
  //=============================================================================
  private async runNotebook(params: Record<string, unknown>): Promise<{
    ok: boolean;
    output: string;
    errorId: string;
    errorMessage: string;
    figures: FigureImageResult[];
  }> {
    if (this.isInterpreterBusy()) {
      return {
        ok: false,
        output: '',
        errorId: 'Nelson:Notebook:busy',
        errorMessage: 'Nelson is busy. Stop the current execution before starting another one.',
        figures: [],
      };
    }

    const reuseHandles = Array.isArray(params.reuseHandles)
      ? params.reuseHandles
          .map((value) => Number(value))
          .filter((value) => Number.isSafeInteger(value) && value >= 0)
      : [];
    const code = String(params.code ?? '');
    const interactive = params.interactiveFigures === true;
    // A second Notebook document shares this worker with the first one.
    // Reset only the capture state here: figures already owned by another
    // Notebook panel must remain alive and continue to render in that
    // panel. Closing the global set would leave the first panel with invalid
    // handles as soon as the second document starts running.
    if (params.resetLiveFigures === true) {
      this.notebookSuppressExternalFrames = true;
      this.notebookCapturedHandles.clear();
      this.notebookGraceBaseline.clear();
      this.notebookGraceUntil = 0;
    }
    const baseline = new Set<number>();
    const output: string[] = [];
    this.notebookCapturedHandles.clear();
    this.notebookBaseline.clear();
    this.notebookGraceUntil = 0;
    this.notebookGraceBaseline.clear();
    this.notebookInteractive = interactive;
    if (interactive) this.notebookSuppressExternalFrames = true;
    try {
      // A shared worker can have a frame queued from a previous page. It is
      // not part of this cell and must not be flushed as a new desktop figure
      // while the Notebook capture is being established.
      this.cancelPendingFigureFlush();
      this.pendingFigureFrames.clear();
      this.pendingFigureFrameReceivedAt.clear();
      // Close the previous cell figures before taking the baseline. This keeps
      // handles stable across slider-driven re-runs, matching WebGuiRpc.
      for (const handle of reuseHandles) {
        try {
          await this.client.closeFigure(handle);
        } catch {
          // A stale handle is harmless; the next cell run will create a fresh one.
        }
        this.notebookFigures.delete(handle);
        this.figures.delete(handle);
        this.pendingFigureFrames.delete(handle);
        this.pendingFigureFrameReceivedAt.delete(handle);
      }
      for (const handle of this.figures.keys()) baseline.add(handle);
      for (const handle of this.pendingFigureFrames.keys()) baseline.add(handle);
      for (const handle of baseline) this.notebookBaseline.add(handle);
      this.notebookCapture = true;

      this.evaluationInProgress = true;
      this.evaluationInWorker = true;
      const result = await this.client.evaluate(code, (chunk) => {
        if (chunk.stream === 'stdout' && chunk.text) output.push(chunk.text);
      });
      this.evaluationInWorker = false;
      this.evaluationInProgress = false;

      const frames = (result.figures ?? []).filter((frame) => !baseline.has(frame.handle));
      const figures: FigureImageResult[] = [];
      // Mark all figures before releasing capture. Worker figure notifications
      // can arrive just after evaluate() resolves; marking first prevents those
      // late frames from opening a regular Figure panel beside the Notebook.
      for (const frame of frames) {
        this.cacheFigure(frame.handle, this.hydrateFigureImages(displayList(frame)));
        if (interactive) this.notebookFigures.add(frame.handle);
      }
      // A shared Wasm worker can already own figures created by a previous
      // Notebook page. The new page has no local frame cache for those
      // handles, so the worker's authoritative figure list is also adopted
      // before capture ends. This prevents stale inline figures from surfacing
      // as desktop Figure tabs after Run all.
      if (interactive && Array.isArray(result.figureList)) {
        for (const entry of result.figureList) {
          const handle = Number(entry?.handle);
          if (Number.isSafeInteger(handle) && handle >= 0 && !baseline.has(handle)) {
            this.notebookFigures.add(handle);
          }
        }
      }
      this.notebookCapture = false;
      this.notebookInteractive = false;
      if (interactive) {
        this.notebookGraceBaseline.clear();
        for (const handle of baseline) this.notebookGraceBaseline.add(handle);
        this.notebookGraceUntil = performance.now() + 500;
      }
      this.notebookBaseline.clear();
      for (const frame of frames) {
        try {
          const image = await this.client.getFigureImage(frame.handle);
          const cached = this.figures.get(frame.handle);
          figures.push({
            handle: frame.handle,
            image: image.image,
            width: cached?.width || 0,
            height: cached?.height || 0,
            interactive,
          });
        } catch {
          figures.push({
            handle: frame.handle,
            image: '',
            width: this.figures.get(frame.handle)?.width ?? 0,
            height: this.figures.get(frame.handle)?.height ?? 0,
            interactive,
          });
        }
        if (!interactive) {
          try {
            await this.client.closeFigure(frame.handle);
          } catch {
            // A cell may have closed its figure itself; the static result is
            // still valid and the next figure-list reconcile will recover.
          }
          this.figures.delete(frame.handle);
          this.pendingFigureFrames.delete(frame.handle);
          this.pendingFigureFrameReceivedAt.delete(frame.handle);
        }
      }
      // Keep the same cell isolation as WebGuiRpc: the figures remain owned by
      // the notebook, but the next cell starts without a current figure and
      // therefore cannot draw into the previous cell's live panel.
      if (interactive && figures.length > 0) {
        await this.client.clearCurrentFigure?.();
      }
      this.notebookCapturedHandles.clear();
      const text = `${output.join('')}${result.stdout ?? ''}`
        .replace(/\u001b\[[0-?]*[ -/]*[@-~]/g, '')
        .replace(/\r\n/g, '\n')
        .replace(/\s+$/, '');
      return { ok: true, output: text, errorId: '', errorMessage: '', figures };
    } catch (error) {
      this.evaluationInWorker = false;
      this.evaluationInProgress = false;
      this.notebookCapture = false;
      this.notebookInteractive = false;
      this.notebookGraceUntil = 0;
      this.notebookGraceBaseline.clear();
      this.notebookBaseline.clear();
      const message = error instanceof Error ? error.message : String(error);
      const leaked = [...this.notebookCapturedHandles].filter((handle) => !baseline.has(handle));
      this.notebookCapturedHandles.clear();
      for (const handle of leaked) {
        this.notebookFigures.delete(handle);
        try {
          await this.client.closeFigure(handle);
        } catch {
          // Best effort cleanup after a failed cell.
        }
        this.figures.delete(handle);
        this.pendingFigureFrames.delete(handle);
        this.pendingFigureFrameReceivedAt.delete(handle);
      }
      return {
        ok: false,
        output: '',
        errorId: 'Nelson:Notebook:error',
        errorMessage: message,
        figures: [],
      };
    }
  }
  //=============================================================================
  private async closeNotebookFigures(
    params: Record<string, unknown>
  ): Promise<{ closed: number[] }> {
    const handles = Array.isArray(params.handles)
      ? params.handles
          .map((value) => Number(value))
          .filter((value) => Number.isSafeInteger(value) && value >= 0)
      : [];
    for (const handle of handles) {
      this.notebookFigures.delete(handle);
      try {
        if (this.isInterpreterBusy()) this.client.queueCommand(`__web_figure_close__(${handle});`);
        else await this.client.closeFigure(handle);
      } catch {
        // Closing an already closed figure is idempotent for Notebook cleanup.
      }
      this.figures.delete(handle);
      this.pendingFigureFrames.delete(handle);
      this.pendingFigureFrameReceivedAt.delete(handle);
    }
    return { closed: handles };
  }
  //=============================================================================
  private releaseNotebookFigure(params: Record<string, unknown>): { ok: true; handle: number } {
    const handle = this.figureHandle(Number(params.handle));
    if (!this.notebookFigures.delete(handle)) return { ok: true, handle };
    emitLocalPush('figure.created', {
      handle,
      title: this.figureTitles.get(handle) ?? `Figure ${handle + 1}`,
      windowStyle: 'docked',
    });
    return { ok: true, handle };
  }
  //=============================================================================
  private acknowledgeExampleRun(): void {
    const acknowledge = this.exampleRunAcknowledgement;
    this.exampleRunAcknowledgement = null;
    acknowledge?.();
  }
  //=============================================================================
  private beginExternalEvaluationPreparation(): number {
    const token = ++WasmDesktopBackend.nextEvaluationPromptToken;
    this.externalEvaluationPreparationToken = token;
    emitLocalPush('term.busy', { token });
    return token;
  }
  //=============================================================================
  private endExternalEvaluationPreparation(token: number, prompt: boolean): void {
    if (this.externalEvaluationPreparationToken !== token) return;
    this.externalEvaluationPreparationToken = null;
    if (prompt) emitLocalPush('term.prompt', { prompt: '>> ', token });
  }
  //=============================================================================
  private async runExample(
    command: string,
    preparationToken: number | null = null
  ): Promise<{ accepted: true }> {
    const busyFromAnotherSource =
      this.evaluationInProgress ||
      this.exampleRunAcknowledgement !== null ||
      (this.externalEvaluationPreparationToken !== null &&
        this.externalEvaluationPreparationToken !== preparationToken);
    if (busyFromAnotherSource) {
      throw new Error('Nelson is busy. Stop the current execution before starting another demo.');
    }
    return new Promise((resolve, reject) => {
      this.exampleRunAcknowledgement = () => resolve({ accepted: true });
      const evaluation = this.evaluate(command);
      if (preparationToken !== null) this.endExternalEvaluationPreparation(preparationToken, false);
      void evaluation.catch((error) => {
        if (preparationToken !== null)
          this.endExternalEvaluationPreparation(preparationToken, true);
        this.exampleRunAcknowledgement = null;
        reject(error);
      });
    });
  }
  //=============================================================================
  private async loadExamplesCatalog(): Promise<ExamplesCatalog> {
    if (this.examplesCatalog) return this.examplesCatalog;
    if (!this.options.examplesUrl) throw new Error('Examples are unavailable');
    const response = await fetch(
      resolveVersionedAssetUrl('./catalog.json', this.options.examplesUrl)
    );
    if (!response.ok) throw new Error(`Cannot load examples catalog (${response.status})`);
    const catalog = (await response.json()) as ExamplesCatalog;
    this.examplesCatalog = await this.withInstalledPackageExamples(catalog);
    return this.examplesCatalog;
  }
  //=============================================================================
  private async withInstalledPackageExamples(catalog: ExamplesCatalog): Promise<ExamplesCatalog> {
    const groups = catalog.groups.slice();
    const diagnostics = Array.isArray(catalog.diagnostics) ? catalog.diagnostics.slice() : [];
    const installedGroups = await this.installedPackageExampleGroups(catalog.locale, diagnostics);
    if (!installedGroups.length) return { ...catalog, groups, diagnostics };
    return { ...catalog, groups: [...groups, ...installedGroups], diagnostics };
  }
  //=============================================================================
  private async installedPackageExampleGroups(
    locale: string,
    diagnostics: unknown[]
  ): Promise<ExamplesCatalogGroup[]> {
    if (!this.packageManagerServices) return [];
    let listResult: unknown;
    try {
      listResult = await this.packageManagerFacade('list', {});
    } catch {
      return [];
    }
    const installed = (listResult as { installed?: unknown[] })?.installed;
    if (!Array.isArray(installed)) return [];
    const groups: ExamplesCatalogGroup[] = [];
    for (const item of installed) {
      const group = await this.installedPackageExampleGroup(item, locale, diagnostics);
      if (group) groups.push(group);
    }
    return groups.sort((left, right) => left.title.localeCompare(right.title));
  }
  //=============================================================================
  private async installedPackageExampleGroup(
    item: unknown,
    locale: string,
    diagnostics: unknown[]
  ): Promise<ExamplesCatalogGroup | null> {
    if (!item || typeof item !== 'object' || Array.isArray(item)) return null;
    const packageInfo = item as Record<string, unknown>;
    const name = String(packageInfo.name ?? '').trim();
    const root = String(packageInfo.path ?? '')
      .replaceAll('\\', '/')
      .replace(/\/+$/, '');
    if (!/^[A-Za-z][A-Za-z0-9_]*$/.test(name) || !root) return null;
    const indexPath = `${root}/examples/index.json`;
    let manifest: unknown;
    try {
      manifest = JSON.parse(new TextDecoder().decode(await this.client.readFile(indexPath)));
    } catch {
      return null;
    }
    const examples = this.installedPackageExamplesFromManifest(name, root, manifest, locale);
    if (!examples.length) return null;
    if ((manifest as { schemaVersion?: unknown })?.schemaVersion !== 1) {
      diagnostics.push({ module: name, message: 'unsupported installed examples catalog schema' });
    }
    return {
      name: `external:${name}`,
      title: name.replaceAll('_', ' '),
      source: 'external',
      examples,
    };
  }
  //=============================================================================
  private installedPackageExamplesFromManifest(
    name: string,
    root: string,
    manifest: unknown,
    locale: string
  ): ExamplesCatalogEntry[] {
    if (
      !manifest ||
      typeof manifest !== 'object' ||
      Array.isArray(manifest) ||
      (manifest as { schemaVersion?: unknown }).schemaVersion !== 1 ||
      !Array.isArray((manifest as { examples?: unknown }).examples)
    ) {
      return [];
    }
    const entries: ExamplesCatalogEntry[] = [];
    for (const item of (manifest as { examples: unknown[] }).examples) {
      if (!item || typeof item !== 'object' || Array.isArray(item)) continue;
      const raw = item as Record<string, unknown>;
      const entry: ExamplesCatalogEntry = {
        title: localizedText(raw.title, locale),
        description: localizedText(raw.description, locale),
        kind: raw.command ? 'command' : 'script',
        tags: Array.isArray(raw.tags) ? raw.tags.map(String) : [],
        requirements: Array.isArray(raw.requirements) ? raw.requirements.map(String) : [],
      };
      if (raw.command) {
        entry.command = String(raw.command);
      } else {
        const file = String(raw.file ?? '').replaceAll('\\', '/');
        if (
          !file.endsWith('.m') ||
          file.startsWith('/') ||
          file.split('/').some((part) => part === '..')
        ) {
          continue;
        }
        entry.file = `${root}/examples/${file}`;
      }
      if (!entry.title) entry.title = entry.file?.split('/').pop() ?? name;
      entries.push(entry);
    }
    return entries;
  }
  //=============================================================================
  private async dispatchExamplesRequest(
    method: string,
    params: Record<string, unknown>
  ): Promise<unknown> {
    const preparationToken =
      method === 'run' && !this.isInterpreterBusy()
        ? this.beginExternalEvaluationPreparation()
        : null;
    try {
      const catalog = await this.loadExamplesCatalog();
      if (method === 'list' || method === 'refresh') return catalog;
      const entries = catalog.groups.flatMap((group) => group.examples);
      if ((method === 'open' || method === 'run') && typeof params.path === 'string') {
        const path = params.path;
        if (!entries.some((entry) => entry.file === path)) throw new Error('Unknown example path');
        await this.materializeBrowserExample(path);
        if (method === 'open') {
          emitLocalPush('editor.show', { path });
          return { accepted: true };
        }
        return this.runExample(`run('${path.replaceAll("'", "''")}')`, preparationToken);
      }
      if (method === 'run' && typeof params.command === 'string') {
        const command = params.command;
        if (!entries.some((entry) => entry.command === command)) {
          throw new Error('Unknown example command');
        }
        return this.runExample(command, preparationToken);
      }
      throw new Error('Unsupported examples action');
    } catch (error) {
      if (preparationToken !== null) this.endExternalEvaluationPreparation(preparationToken, true);
      throw error;
    }
  }
  //=============================================================================
  private async handleExamplesMessage(message: unknown): Promise<void> {
    const envelope = message as {
      type?: string;
      payload?: { id?: number; method?: string; params?: Record<string, unknown> };
    };
    if (envelope?.type !== 'request' || !envelope.payload) return;
    const request = envelope.payload;
    const id = Number(request.id ?? 0);
    try {
      const result = await this.dispatchExamplesRequest(
        String(request.method ?? ''),
        request.params ?? {}
      );
      this.examplesChannel?.postMessage({ type: 'response', id, payload: { id, result } });
    } catch (error) {
      const messageText = error instanceof Error ? error.message : String(error);
      this.examplesChannel?.postMessage({
        type: 'response',
        id,
        payload: { id, error: { message: messageText } },
      });
    }
  }
  //=============================================================================
  private nelsonText(value: string): string {
    return `'${value.replaceAll("'", "''")}'`;
  }
  //=============================================================================
  private async profileExport(
    pathValue: string
  ): Promise<{ ok: true; path: string; index: string; url: string; on: false }> {
    const directory = this.workspacePath(pathValue || `${WORKSPACE}/profile_result`);
    const result = await this.profiler.exportReport(directory);
    emitLocalPush('fs.changed', { path: result.index });
    return result;
  }
  //=============================================================================
  private async packageManagerFacade(
    method: string,
    params: Record<string, unknown>
  ): Promise<unknown> {
    const begin = '__NELSON_NMM_RESULT_BEGIN__';
    const end = '__NELSON_NMM_RESULT_END__';
    const code = [
      `disp('${begin}');`,
      `disp(nmm_gui_rpc(${this.nelsonText(method)}, ${this.nelsonText(JSON.stringify(params))}));`,
      `disp('${end}');`,
    ].join(' ');
    let streamedStdout = '';
    const result = await this.client.evaluate(code, (output) => {
      if (output.stream === 'stdout') streamedStdout += output.text;
    });
    if (result.exitCode !== 0) {
      throw new Error(result.errorMessage || result.stderr || 'Package Manager request failed');
    }
    const stdout = result.stdout || streamedStdout;
    const start = stdout.indexOf(begin);
    const finish = stdout.lastIndexOf(end);
    if (start < 0 || finish < start) throw new Error('Package Manager response is incomplete');
    const json = stdout.slice(start + begin.length, finish).trim();
    return JSON.parse(json);
  }
  //=============================================================================
  private async runPackageManagerCommand(code: string): Promise<{ accepted: true }> {
    const result = await this.client.evaluate(code);
    if (result.exitCode !== 0) {
      throw new Error(result.errorMessage || result.stderr || 'Package operation failed');
    }
    emitLocalPush('workspace.changed', {});
    return { accepted: true };
  }
  //=============================================================================
  private isPackageEnvironmentMutation(method: string): boolean {
    return [
      'install',
      'installSource',
      'load',
      'pin',
      'remove',
      'setRegistry',
      'unload',
      'unpin',
      'update',
    ].includes(method);
  }
  //=============================================================================
  private notifyPackageEnvironmentChanged(): void {
    this.examplesCatalog = null;
    emitLocalPush('examples.changed', { reason: 'packages' });
    this.examplesChannel?.postMessage({
      type: 'event',
      event: 'changed',
      payload: { reason: 'packages' },
    });
    emitLocalPush('workspace.changed', {});
  }
  //=============================================================================
  private async helpText(name: string): Promise<{ text: string }> {
    if (this.isInterpreterBusy()) {
      throw new Error('Documentation is unavailable while Nelson is busy.');
    }
    const topic = name.trim();
    if (!topic) return { text: '' };
    await this.ensureRuntimeResourcesMountedForModules({
      modules: ['help_tools'],
      kinds: ['help'],
    });
    const begin = '__NELSON_HELP_TEXT_BEGIN__';
    const end = '__NELSON_HELP_TEXT_END__';
    const code = [`disp('${begin}');`, `help(${this.nelsonText(topic)});`, `disp('${end}');`].join(
      ' '
    );
    let streamedStdout = '';
    const result = await this.client.evaluate(code, (output) => {
      if (output.stream === 'stdout' && output.text) streamedStdout += output.text;
    });
    if (result.exitCode !== 0) {
      throw new Error(
        result.errorMessage || result.stderr || `No documentation found for "${topic}".`
      );
    }
    const stdout = result.stdout || streamedStdout;
    const start = stdout.indexOf(begin);
    const finish = stdout.lastIndexOf(end);
    if (start < 0 || finish < start) {
      throw new Error(`No documentation found for "${topic}".`);
    }
    const text = stdout.slice(start + begin.length, finish).trim();
    if (!text) throw new Error(`No documentation found for "${topic}".`);
    return { text };
  }
  //=============================================================================
  private async helpDocument(
    name: string,
    requestedPath: string
  ): Promise<{ html?: string; path: string; title: string; url?: string }> {
    if (this.isInterpreterBusy()) {
      throw new Error('Documentation is unavailable while Nelson is busy.');
    }

    let pagePath = requestedPath.trim();
    if (!pagePath) {
      const rootBegin = '__NELSON_DOCROOT_BEGIN__';
      const rootEnd = '__NELSON_DOCROOT_END__';
      let rootStdout = '';
      const rootResult = await this.client.evaluate(
        [`disp('${rootBegin}');`, 'disp(docroot());', `disp('${rootEnd}');`].join(' '),
        (output) => {
          if (output.stream === 'stdout' && output.text) rootStdout += output.text;
        }
      );
      if (rootResult.exitCode !== 0) {
        throw new Error(
          rootResult.errorMessage || rootResult.stderr || 'Unable to read documentation root.'
        );
      }
      const rootOutput = rootResult.stdout || rootStdout;
      const rootStart = rootOutput.indexOf(rootBegin);
      const rootFinish = rootOutput.lastIndexOf(rootEnd);
      const configuredRoot =
        rootStart >= 0 && rootFinish > rootStart
          ? rootOutput.slice(rootStart + rootBegin.length, rootFinish).trim()
          : '';
      const localHelp = configuredRoot === '.';
      if (localHelp) {
        // A package-qualified topic needs only the main viewer archive and that
        // package's archive. Mounting every built-in archive would fetch tens of
        // megabytes on the first documentation request.
        const qualifiedModule = /^([A-Za-z][A-Za-z0-9_]*)\./.exec(name.trim())?.[1];
        await this.ensureRuntimeResourcesMountedForModules({
          modules: null,
          kinds: ['archives'],
          archiveModules: qualifiedModule ? ['main', qualifiedModule] : null,
        });
      }
      let streamedStdout = '';
      const result = await this.client.evaluate(
        [
          "setenv('NELSON_DOC_DRY_RUN', 'TRUE');",
          ...(localHelp
            ? [
                `setenv('NELSON_HELP_CACHE_DIR', '${WASM_HELP_CACHE_ROOT}');`,
                // Refresh also rebuilds index.json, which the help archives of an
                // installed external package need; install alone only extracts them.
                "deployhelp('refresh');",
              ]
            : []),
          `doc(${this.nelsonText(name.trim())});`,
          "setenv('NELSON_DOC_DRY_RUN', '');",
        ].join(' '),
        (output) => {
          if (output.stream === 'stdout' && output.text) streamedStdout += output.text;
        }
      );
      if (result.exitCode !== 0) {
        throw new Error(result.errorMessage || result.stderr || 'Unable to prepare documentation.');
      }
      const output = result.stdout || streamedStdout;
      const targetMatch = /(https?:\/\/[^\s"'<>]+|file:\/\/[^\s"'<>]+)/i.exec(output);
      if (!targetMatch) throw new Error(`No documentation found for "${name}".`);
      const target = targetMatch[0];
      if (/^https?:\/\//i.test(target)) {
        return { path: target, title: name.trim(), url: target };
      }
      const targetPath = decodeURIComponent(target.replace(/^file:\/\//i, '').split('#')[0]);
      const languageRoot = targetPath.slice(0, targetPath.lastIndexOf('/'));
      const openMatch = /#open=([^&"']+)/i.exec(target);
      pagePath = openMatch
        ? normalizeVirtualPath(`${languageRoot}/${decodeURIComponent(openMatch[1])}`)
        : targetPath;
    }

    const normalizedPath = normalizeVirtualPath(pagePath);
    if (!normalizedPath.startsWith(`${WASM_HELP_CACHE_ROOT}/`)) {
      throw new Error('Invalid local documentation path.');
    }
    const source = new TextDecoder().decode(await this.client.readFile(normalizedPath));
    if (!source) throw new Error(`No documentation found for "${name}".`);
    const html = await this.inlineHelpAssets(normalizedPath, source);
    const title = /<title[^>]*>([^<]+)<\/title>/i.exec(source)?.[1]?.trim() || name.trim();
    return { html, path: normalizedPath, title };
  }
  //=============================================================================
  private async inlineHelpAssets(pagePath: string, source: string): Promise<string> {
    const references = new Set<string>();
    for (const match of source.matchAll(/(?:href|src)=["']([^"']+)["']/gi)) {
      const reference = match[1].trim();
      if (
        !reference ||
        reference.startsWith('#') ||
        reference.startsWith('data:') ||
        /^[a-z][a-z0-9+.-]*:/i.test(reference) ||
        reference.endsWith('.html')
      ) {
        continue;
      }
      references.add(reference);
    }

    // Generated viewer pages hold JavaScript string literals containing `</script>`.
    // Keep real closing tags, but escape literals followed by JavaScript punctuation
    // so the browser does not show the rest of the script as page text.
    let html = source.replace(/<\/script>(?=[^<\s])/gi, '<\\/script>');
    for (const reference of references) {
      // Keep viewer JavaScript a relative resource: a global string replacement can
      // also match the same tag inside an inline JavaScript string and corrupt the page.
      if (/\.js$/i.test(reference)) continue;
      const assetPath = resolveVirtualPath(pagePath, reference);
      let bytes: Uint8Array;
      try {
        bytes = await this.client.readFile(assetPath);
      } catch {
        continue;
      }
      if (!bytes.byteLength) continue;
      const escaped = reference.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
      if (/\.css$/i.test(reference)) {
        const css = new TextDecoder().decode(bytes);
        html = html.replace(
          new RegExp(`<link([^>]+href=["']${escaped}["'][^>]*)>`, 'i'),
          `<style>${css}</style>`
        );
      } else {
        html = html.replace(
          new RegExp(`(["'])${escaped}(["'])`, 'g'),
          `$1${dataUrl(assetPath, bytes)}$2`
        );
      }
    }

    const bridge =
      '<script>(function(){document.addEventListener("click",function(e){var a=e.target.closest&&e.target.closest("a[href]");if(!a)return;var h=a.getAttribute("href")||"";if(/\\.html(?:#.*)?$/i.test(h)&&!/^https?:/i.test(h)){e.preventDefault();window.parent.postMessage({source:"nelson-wasm-help",href:h},"*");}});})();</script>';
    return html.replace(/<\/body>/i, `${bridge}</body>`);
  }
  //=============================================================================
  private async installPackageSource(source: string): Promise<{ accepted: true }> {
    if (/^\/workspace\/[A-Za-z0-9_./-]+\.nmz$/i.test(source) && !source.includes('..')) {
      return this.runPackageManagerCommand(`nmm('install', ${this.nelsonText(source)});`);
    }
    if (!/^https?:\/\//i.test(source) || !source.toLowerCase().endsWith('.nmz')) {
      throw new Error('WebAssembly package sources must be HTTP(S) or /workspace .nmz archives');
    }
    const limit = this.options.runtimeCapabilities?.limits.maxVirtualFileBytes ?? 32 * 1024 * 1024;
    const [bytes, checksum] = await Promise.all([
      this.fetchPackageResource(source, 'Package archive', limit),
      this.fetchPackageResource(`${source}.sha256`, 'Package checksum'),
    ]);
    const sourceName = decodeURIComponent(
      new URL(source).pathname.split('/').at(-1) || 'package.nmz'
    );
    const safeName = sourceName.replace(/[^A-Za-z0-9_.-]/g, '_');
    const path = `/workspace/${safeName}`;
    await this.client.writeFile(path, bytes);
    await this.client.writeFile(`${path}.sha256`, checksum);
    return this.runPackageManagerCommand(`nmm('install', ${this.nelsonText(path)});`);
  }
  //=============================================================================
  private async fetchRequired(
    url: string,
    description: string,
    limit = this.options.runtimeCapabilities?.limits.maxSourceBytes ?? 4 * 1024 * 1024,
    signal?: AbortSignal
  ): Promise<Uint8Array> {
    let response: Response;
    try {
      response = signal ? await fetch(url, { signal }) : await fetch(url);
    } catch {
      throw new Error(
        `${description} download failed: the host must allow cross-origin browser downloads`
      );
    }
    if (!response.ok) throw new Error(`${description} download failed: HTTP ${response.status}`);
    const bytes = new Uint8Array(await response.arrayBuffer());
    if (bytes.byteLength > limit) throw new Error(`${description} exceeds the ${limit}-byte limit`);
    return bytes;
  }
  //=============================================================================
  private packageMirrorUrl(source: string): string | null {
    if (!this.options.packageManagerUrl) return null;
    const filename = new URL(source).pathname.split('/').at(-1);
    if (!filename) return null;
    return resolveVersionedAssetUrl(
      `../nmm-packages/${encodeURIComponent(filename)}`,
      this.options.packageManagerUrl
    );
  }
  //=============================================================================
  private async fetchPackageResource(
    source: string,
    description: string,
    limit?: number
  ): Promise<Uint8Array> {
    try {
      return await this.fetchRequired(source, description, limit);
    } catch (directError) {
      const mirror = this.packageMirrorUrl(source);
      if (!mirror || mirror === source) throw directError;
      try {
        return await this.fetchRequired(mirror, description, limit);
      } catch {
        const detail = directError instanceof Error ? directError.message : String(directError);
        throw new Error(`${detail}; same-origin package mirror unavailable at ${mirror}`);
      }
    }
  }
  //=============================================================================
  private async configureBrowserRegistry(url: string, fresh: boolean): Promise<void> {
    const source = url.trim() || DEFAULT_NMM_REGISTRY;
    if (!/^https?:\/\//i.test(source)) {
      throw new Error('The browser registry must use an HTTP(S) URL');
    }
    if (!fresh && this.browserRegistryReady && this.browserRegistryUrl === source) return;
    const [registry, checksum, signature] = await Promise.all([
      this.fetchRequired(source, 'Registry'),
      this.fetchRequired(`${source}.sha256`, 'Registry checksum'),
      this.fetchRequired(`${source}.sig`, 'Registry signature'),
    ]);
    await this.client.writeFile(NMM_REGISTRY_FILE, registry);
    await this.client.writeFile(`${NMM_REGISTRY_FILE}.sha256`, checksum);
    await this.client.writeFile(`${NMM_REGISTRY_FILE}.sig`, signature);
    await this.runPackageManagerCommand(
      `setenv('NELSON_NMM_REGISTRY', ${this.nelsonText(NMM_REGISTRY_FILE)});`
    );
    const status = (await this.packageManagerFacade('registry', {})) as {
      ok?: boolean;
      error?: string;
    };
    if (!status.ok) throw new Error(status.error || 'Registry validation failed');
    this.browserRegistryUrl = source;
    this.browserRegistryReady = true;
  }
  //=============================================================================
  private async installRegistryPackage(
    name: string,
    version: string,
    registryUrl: string
  ): Promise<{ accepted: true }> {
    await this.configureBrowserRegistry(registryUrl, false);
    const preview = (await this.packageManagerFacade('preview', { name, version })) as {
      ok?: boolean;
      error?: string;
      plan?: { install_plan?: unknown };
    };
    if (!preview.ok) throw new Error(preview.error || 'Package installation plan failed');
    const value = preview.plan?.install_plan;
    const items = (Array.isArray(value) ? value : value ? [value] : []) as {
      action?: string;
      package?: string;
      version?: string;
      source?: string;
      checksum?: string;
    }[];
    for (const item of items.reverse()) {
      if (item.action !== 'install') continue;
      const source = String(item.source ?? '');
      const checksum = String(item.checksum ?? '');
      if (!/^https?:\/\//i.test(source) || !source.toLowerCase().endsWith('.nmz')) {
        throw new Error(`Package ${item.package || name} has no browser-compatible .nmz archive`);
      }
      if (!/^[a-f0-9]{64}$/i.test(checksum)) {
        throw new Error(`Package ${item.package || name} has no valid registry checksum`);
      }
      const bytes = await this.fetchPackageResource(
        source,
        `Package ${item.package || name}`,
        this.options.runtimeCapabilities?.limits.maxVirtualFileBytes ?? 32 * 1024 * 1024
      );
      // Keep the registry's archive name (it carries the platform tag, e.g.
      // tessera-wasm32-1.0.0.nmz) so the workspace copy and its checksum sidecar match
      // the registry and the same-origin mirror.
      const sourceName = (new URL(source).pathname.split('/').at(-1) || '').replace(
        /[^A-Za-z0-9_.-]/g,
        '_'
      );
      const safePackage = String(item.package || name).replace(/[^A-Za-z0-9_.-]/g, '_');
      const safeVersion = String(item.version || version || 'latest').replace(
        /[^A-Za-z0-9_.-]/g,
        '_'
      );
      const fileName = /\.nmz$/i.test(sourceName)
        ? sourceName
        : `${safePackage}-${safeVersion}.nmz`;
      const path = `${WORKSPACE}/${fileName}`;
      await this.client.writeFile(path, bytes);
      await this.client.writeFile(`${path}.sha256`, new TextEncoder().encode(`${checksum}\n`));
      await this.runPackageManagerCommand(`nmm('install', ${this.nelsonText(path)});`);
    }
    return { accepted: true };
  }
  //=============================================================================
  private async dispatchPackageManagerRequest(
    method: string,
    params: Record<string, unknown>
  ): Promise<unknown> {
    if (method === 'interrupt') return this.interrupt();
    if (method === 'updates') {
      const count = Math.max(0, Math.trunc(Number(params.count) || 0));
      emitLocalPush('packages.updates', { count });
      return { count };
    }
    const readMethods = new Set([
      'cache',
      'cacheClear',
      'cacheRemove',
      'cacheVerify',
      'doc',
      'freeze',
      'list',
      'preview',
      'registry',
      'search',
      'snapshot',
      'tree',
      'verifyInstalled',
      'versions',
    ]);
    const registryUrl = String(params.browserRegistryUrl ?? '');
    const registryReadMethods = new Set([
      'doc',
      'list',
      'preview',
      'registry',
      'search',
      'snapshot',
      'tree',
      'versions',
    ]);
    if (registryReadMethods.has(method)) {
      await this.configureBrowserRegistry(
        registryUrl,
        method === 'snapshot' && Boolean(params.fresh)
      );
    }
    if (readMethods.has(method)) return this.packageManagerFacade(method, params);
    if (method === 'installSource') return this.installPackageSource(String(params.source ?? ''));
    if (method === 'setRegistry') {
      const source = String(params.url ?? '');
      await this.configureBrowserRegistry(source, true);
      return { accepted: true };
    }
    const name = String(params.name ?? '');
    const version = String(params.version ?? '');
    if (!/^[A-Za-z][A-Za-z0-9_]*$/.test(name)) throw new Error('Invalid package name');
    if (method === 'remove') return this.runPackageManagerCommand(`nmm('remove', '${name}');`);
    if (method === 'load') {
      const suffix = version ? `, ${this.nelsonText(version)}` : '';
      return this.runPackageManagerCommand(`nmm('load', '${name}'${suffix});`);
    }
    if (method === 'unload') return this.runPackageManagerCommand(`removemodule('${name}');`);
    if (method === 'pin' || method === 'unpin') {
      const suffix = version ? `, ${this.nelsonText(version)}` : '';
      return this.runPackageManagerCommand(`nmm('${method}', '${name}'${suffix});`);
    }
    if (method === 'install' || method === 'update') {
      return this.installRegistryPackage(name, version, registryUrl);
    }
    throw new Error(`Unsupported Package Manager operation: ${method}`);
  }
  //=============================================================================
  private async dispatchWebTool(params: Record<string, unknown>): Promise<unknown> {
    if (params.tool !== 'nmm' || !this.packageManagerServices) {
      throw new Error(`Unknown web tool: ${String(params.tool ?? '')}`);
    }
    const method = String(params.method ?? '');
    const requestParams =
      params.params && typeof params.params === 'object' && !Array.isArray(params.params)
        ? (params.params as Record<string, unknown>)
        : {};
    const prepared = await this.packageManagerServices.prepare(method, requestParams);
    if (prepared.handled) return prepared.result;
    const result = await this.dispatchPackageManagerRequest(method, prepared.params);
    this.packageManagerServices.complete(method, requestParams);
    if (this.isPackageEnvironmentMutation(method)) {
      this.notifyPackageEnvironmentChanged();
      emitLocalPush('tool.event', { tool: 'nmm', channel: 'changed', payload: '' });
    }
    return result;
  }
  //=============================================================================
  private async handlePackageManagerMessage(message: unknown): Promise<void> {
    const envelope = message as {
      type?: string;
      token?: number;
      payload?: { id?: number; method?: string; params?: Record<string, unknown> };
    };
    if (envelope?.type !== 'request' || !Number.isSafeInteger(envelope.token) || !envelope.payload)
      return;
    const id = Number(envelope.payload.id ?? 0);
    try {
      const method = String(envelope.payload.method ?? '');
      const result = await this.dispatchPackageManagerRequest(
        method,
        envelope.payload.params ?? {}
      );
      if (this.isPackageEnvironmentMutation(method)) this.notifyPackageEnvironmentChanged();
      this.packageManagerChannel?.postMessage({
        type: 'response',
        token: envelope.token,
        payload: { id, result },
      });
    } catch (error) {
      this.packageManagerChannel?.postMessage({
        type: 'response',
        token: envelope.token,
        payload: { id, error: { message: error instanceof Error ? error.message : String(error) } },
      });
    }
  }
  //=============================================================================
  // open_system(path): an already open NFlow page of this origin takes the
  // model over the shared channel; otherwise a new page opens on it.
  private async openNFlow(modelPath?: string): Promise<void> {
    const url = this.options.nflowUrl;
    if (!url) return;
    if (!modelPath) {
      this.options.openExternal?.(url);
      return;
    }
    const delivered = await this.requestNFlowModel(modelPath);
    if (delivered) return;
    const target = new URL(url, window.location.href);
    target.searchParams.set('model', modelPath);
    this.options.openExternal?.(target.href);
  }
  //=============================================================================
  private requestNFlowModel(modelPath: string): Promise<boolean> {
    if (typeof BroadcastChannel !== 'function') return Promise.resolve(false);
    return new Promise((resolve) => {
      const channel = new BroadcastChannel(NFLOW_MODEL_CHANNEL);
      const token = `${Date.now().toString(36)}-${Math.random().toString(36).slice(2)}`;
      const finish = (value: boolean) => {
        clearTimeout(timer);
        channel.close();
        resolve(value);
      };
      const timer = setTimeout(() => finish(false), NFLOW_MODEL_ACK_TIMEOUT_MS);
      channel.onmessage = (event: MessageEvent) => {
        const data = event.data as { type?: string; token?: string } | null;
        if (data?.type === 'open-model-ack' && data.token === token) finish(true);
      };
      channel.postMessage({ type: 'open-model', path: modelPath, token });
    });
  }
  //=============================================================================
  private writeEvaluationOutput(result: NelsonEvaluationResult): void {
    if (result.figureList) this.reconcileFigureList(result.figureList);
    for (const action of result.uiActions ?? []) {
      if (action.type === 'open-examples' && this.options.examplesUrl) {
        emitLocalPush('tool.show', { tool: 'examples', args: {} });
      }
      if (action.type === 'open-nflow') {
        if (this.options.nflowUrl) {
          void this.openNFlow(typeof action.path === 'string' ? action.path : undefined);
        } else {
          emitLocalPush('term.output', {
            text: '\u001b[31mNFlow graphical editor is unavailable in this build.\u001b[0m\n',
          });
        }
      }
      if (action.type === 'open-editor') {
        emitLocalPush('editor.show', { path: action.path, line: action.line });
      }
      if (action.type === 'open-notebook') {
        emitLocalPush('notebook.open', { path: action.path });
      }
      if (
        action.type === 'play-audio' ||
        action.type === 'audio-buffer-begin' ||
        action.type === 'audio-buffer-chunk' ||
        action.type === 'audio-buffer-play' ||
        action.type === 'audio-control'
      ) {
        void handleWasmAudioAction(action).catch((error) => {
          const message = error instanceof Error ? error.message : String(error);
          emitLocalPush('term.output', {
            text: `\u001b[31mWeb Audio playback failed: ${ensureTrailingNewline(message)}\u001b[0m`,
          });
        });
      }
    }
    if (result.stdout) {
      emitLocalPush('term.output', { text: ensureTrailingNewline(result.stdout) });
    }
    if (result.stderr) {
      emitLocalPush('term.output', {
        text: `\u001b[31m${ensureTrailingNewline(result.stderr)}\u001b[0m`,
      });
    }
  }
  //=============================================================================
  private receiveFigure(frame: NelsonFigureFrame): void {
    const inNotebookGrace =
      this.notebookGraceUntil > performance.now() && !this.notebookGraceBaseline.has(frame.handle);
    const isNotebookFigure = this.notebookFigures.has(frame.handle) || inNotebookGrace;
    if (inNotebookGrace) this.notebookFigures.add(frame.handle);
    if (this.notebookCapture || isNotebookFigure) {
      if (this.notebookCapture) {
        this.notebookCapturedHandles.add(frame.handle);
        // Register a newly created figure while capture is still active. The
        // worker may flush a final frame after evaluate() resolves; registering
        // it now closes the same race that WebGuiServer avoids by registering
        // before ending its inline capture guard.
        if (this.notebookInteractive && !this.notebookBaseline.has(frame.handle)) {
          this.notebookFigures.add(frame.handle);
        }
      }
      this.retainFigureImagePayloads(frame);
      const figure = this.cacheFigure(frame.handle, this.hydrateFigureImages(displayList(frame)));
      // A late frame belonging to a Notebook still updates its inline
      // output, but must never create a separate desktop Figure panel.
      if (isNotebookFigure && !this.notebookCapture && !this.notebookSuppressExternalFrames) {
        emitLocalPushValue('figure.frame', { handle: frame.handle, displayList: figure });
      }
      return;
    }
    const pending = this.pendingFigureFrames.get(frame.handle);
    const wasPending = pending !== undefined;
    this.retainFigureImagePayloads(frame);
    // Frames coalesce per figure until the flush: a compact matrix-only frame
    // must not discard a complete frame still waiting there.
    const next =
      pending && isMatrixOnlyFrame(displayList(frame)) && !isMatrixOnlyFrame(displayList(pending))
        ? { ...frame, displayList: mergeMatrixOnlyFrame(displayList(pending), displayList(frame)) }
        : frame;
    this.pendingFigureFrames.set(frame.handle, next);
    this.pendingFigureFrameReceivedAt.set(frame.handle, performance.now());
    recordWasmFigureReceived(wasPending, this.pendingFigureFrames.size);
    this.schedulePendingFigureFlush();
  }
  //=============================================================================
  private reconcileFigureList(entries: Array<{ handle: number; title: string }>): void {
    if (this.notebookCapture) {
      for (const entry of entries) this.figureTitles.set(entry.handle, entry.title);
      return;
    }
    const current = new Set(entries.map((entry) => entry.handle));
    for (const handle of new Set([...this.figures.keys(), ...this.pendingFigureFrames.keys()])) {
      if (current.has(handle)) continue;
      this.figures.delete(handle);
      this.pendingFigureFrames.delete(handle);
      this.pendingFigureFrameReceivedAt.delete(handle);
      this.figureTitles.delete(handle);
      emitLocalPush('figure.closed', { handle });
    }
    for (const entry of entries) {
      if (this.figureTitles.get(entry.handle) !== entry.title) {
        this.figureTitles.set(entry.handle, entry.title);
        emitLocalPush('figure.title', entry);
      }
    }
  }
  //=============================================================================
  private retainFigureImagePayloads(frame: NelsonFigureFrame): void {
    const figure = displayList(frame);
    for (const primitive of figure.primitives) {
      if (primitive.kind !== 'image') continue;
      const transport = primitive as ImagePrimitive;
      if (transport.imageId && transport.image) {
        this.figureImages.set(transport.imageId, transport.image);
        recordWasmFigureImagePayload();
      }
      if (transport.imageId && transport.rgbaBytes instanceof ArrayBuffer) {
        this.figureRgbaImages.set(transport.imageId, {
          rgbaBytes: transport.rgbaBytes,
          imageWidth: transport.imageWidth,
          imageHeight: transport.imageHeight,
        });
        recordWasmFigureImagePayload();
      }
    }
  }
  //=============================================================================
  private schedulePendingFigureFlush(): void {
    if (this.figureFlushHandle !== null) return;
    // The Worker client and FigurePanel already coalesce frames. Another rAF here
    // costs a full display tick per frame in wasm animations, so only same-turn
    // bursts are coalesced and the latest frame is forwarded immediately.
    this.figureFlushUsesAnimationFrame = false;
    this.figureFlushHandle = window.setTimeout(() => this.flushPendingFigureFrames(), 0);
  }
  //=============================================================================
  private cancelPendingFigureFlush(): void {
    if (this.figureFlushHandle === null) return;
    if (this.figureFlushUsesAnimationFrame && typeof window.cancelAnimationFrame === 'function') {
      window.cancelAnimationFrame(this.figureFlushHandle);
    } else {
      window.clearTimeout(this.figureFlushHandle);
    }
    this.figureFlushHandle = null;
    this.figureFlushUsesAnimationFrame = false;
  }
  //=============================================================================
  private flushPendingFigureFrames(): void {
    this.figureFlushHandle = null;
    this.figureFlushUsesAnimationFrame = false;
    const frames = [...this.pendingFigureFrames.values()];
    this.pendingFigureFrames.clear();
    for (const frame of frames) this.flushFigureFrame(frame);
  }
  //=============================================================================
  private flushPendingFigureFrame(handle: number): void {
    const frame = this.pendingFigureFrames.get(handle);
    if (!frame) return;
    this.pendingFigureFrames.delete(handle);
    const receivedAt = this.pendingFigureFrameReceivedAt.get(handle);
    this.pendingFigureFrameReceivedAt.delete(handle);
    if (this.pendingFigureFrames.size === 0) this.cancelPendingFigureFlush();
    if (receivedAt !== undefined) recordWasmFigureFlush(performance.now() - receivedAt);
    this.flushFigureFrame(frame);
  }
  //=============================================================================
  private flushFigureFrame(frame: NelsonFigureFrame): void {
    const receivedAt = this.pendingFigureFrameReceivedAt.get(frame.handle);
    this.pendingFigureFrameReceivedAt.delete(frame.handle);
    if (receivedAt !== undefined) recordWasmFigureFlush(performance.now() - receivedAt);
    const fresh = !this.figures.has(frame.handle);
    const figure = this.hydrateFigureImages(displayList(frame));
    this.cacheFigure(frame.handle, figure);
    if (fresh && !this.notebookFigures.has(frame.handle) && !this.notebookSuppressExternalFrames) {
      emitLocalPush('figure.created', {
        handle: frame.handle,
        title: this.figureTitles.get(frame.handle) ?? `Figure ${frame.handle + 1}`,
        windowStyle: 'docked',
      });
    }
    // drawnow already supplied the rendered frame: forward it without a second fetch
    // or JSON round-trip.
    if (!this.notebookSuppressExternalFrames) {
      emitLocalPushValue('figure.frame', { handle: frame.handle, displayList: figure });
    }
    this.acknowledgeExampleRun();
  }
  //=============================================================================
  // The cached list answers figure.getDisplayList and the replies synthesized
  // while the interpreter is busy, so it must stay a complete display list: a
  // compact matrix-only frame only updates the matrices of the previous one.
  private cacheFigure(handle: number, figure: FigureDisplayListResult): FigureDisplayListResult {
    const previous = this.figures.get(handle);
    const cached =
      previous && isMatrixOnlyFrame(figure) ? mergeMatrixOnlyFrame(previous, figure) : figure;
    this.figures.set(handle, cached);
    return cached;
  }
  //=============================================================================
  private hydrateFigureImages(figure: FigureDisplayListResult): FigureDisplayListResult {
    const primitives = figure.primitives.map((primitive) => {
      if (primitive.kind !== 'image') return primitive;
      const transport = primitive as ImagePrimitive;
      if (transport.imageId && transport.image) {
        this.figureImages.set(transport.imageId, transport.image);
        return primitive;
      }
      if (transport.imageId && transport.rgbaBytes instanceof ArrayBuffer) {
        this.figureRgbaImages.set(transport.imageId, {
          rgbaBytes: transport.rgbaBytes,
          imageWidth: transport.imageWidth,
          imageHeight: transport.imageHeight,
        });
        return primitive;
      }
      const retained = transport.imageId ? this.figureImages.get(transport.imageId) : undefined;
      if (retained) return { ...transport, image: retained };
      const retainedRgba = transport.imageId
        ? this.figureRgbaImages.get(transport.imageId)
        : undefined;
      return retainedRgba
        ? {
            ...transport,
            rgbaBytes: retainedRgba.rgbaBytes,
            imageWidth: transport.imageWidth ?? retainedRgba.imageWidth,
            imageHeight: transport.imageHeight ?? retainedRgba.imageHeight,
          }
        : primitive;
    });
    return { ...figure, primitives };
  }
  //=============================================================================
  private figureList(): { figures: { handle: number; title: string; windowStyle: 'docked' }[] } {
    // The shared desktop lifecycle polls this list while a cell is running.
    // Hide the transient Notebook population during capture (and while
    // late frames are being handed to the inline panels), otherwise the poll
    // opens regular Figure tabs before the cell response is attached.
    if (this.notebookCapture || this.notebookSuppressExternalFrames) {
      return { figures: [] };
    }
    const handles = new Set([...this.figures.keys(), ...this.pendingFigureFrames.keys()]);
    return {
      figures: [...handles]
        .filter((handle) => !this.notebookFigures.has(handle))
        .map((handle) => ({
          handle,
          title: this.figureTitles.get(handle) ?? `Figure ${handle + 1}`,
          windowStyle: 'docked',
        })),
    };
  }
  //=============================================================================
  private getFigure(handle: number): FigureDisplayListResult {
    this.flushPendingFigureFrame(handle);
    const figure = this.figures.get(handle);
    if (!figure) throw new Error(`figure ${handle} is not available`);
    return figure;
  }
  //=============================================================================
  private async getFigureImage(
    handleValue: number
  ): Promise<{ handle: number; image: string; width: number; height: number }> {
    const handle = this.figureHandle(handleValue);
    const figure = this.getFigure(handle);
    const result = await this.client.getFigureImage(handle);
    return { ...result, width: figure.width, height: figure.height };
  }
  //=============================================================================
  private async saveFigure(
    handleValue: number,
    pathValue: string
  ): Promise<{ ok: true; path: string }> {
    const handle = this.figureHandle(handleValue);
    this.getFigure(handle);
    const path = this.workspacePath(pathValue);
    const result = await this.client.saveFigure(handle, path);
    emitLocalPush('fs.changed', { path });
    return result;
  }
  //=============================================================================
  private async closeFigure(handle: number): Promise<{ ok: true; handle: number }> {
    this.flushPendingFigureFrame(handle);
    if (!this.figures.has(handle)) throw new Error(`figure ${handle} is not available`);
    if (this.isInterpreterBusy()) {
      this.client.queueCommand(`__web_figure_close__(${this.figureHandle(handle)});`);
      this.figures.delete(handle);
      emitLocalPush('figure.closed', { handle });
      return { ok: true, handle };
    }
    const result = await this.client.closeFigure(handle);
    if (!result.ok) throw new Error(`figure ${handle} could not be closed`);
    this.figures.delete(handle);
    emitLocalPush('figure.closed', { handle });
    return { ok: true, handle };
  }
  //=============================================================================
  private async setFigureView(params: Record<string, unknown>): Promise<FigureDisplayListResult> {
    const handle = this.figureHandle(Number(params.handle));
    return setWasmFigureView(params, {
      client: this.client,
      handle,
      busy: this.isInterpreterBusy(),
      current: () => this.getFigure(handle),
      accept: (frame) => {
        const updated = { ...frame, handle };
        this.figures.set(handle, updated);
        return updated;
      },
      text: (value) => this.nelsonText(value),
      finite: (value, name) => this.finiteNumber(value, name),
    });
  }
  //=============================================================================
  private async setFigureSize(params: Record<string, unknown>): Promise<FigureDisplayListResult> {
    const handle = this.figureHandle(Number(params.handle));
    const width = this.finiteNumber(params.width, 'width');
    const height = this.finiteNumber(params.height, 'height');
    if (width <= 0 || height <= 0) throw new Error('Invalid WebAssembly figure size');
    if (this.isInterpreterBusy()) {
      this.client.queueCommand(`__web_figure_size__(${handle}, ${width}, ${height});`);
      const updated = { ...this.getFigure(handle), width, height };
      this.figures.set(handle, updated);
      return updated;
    }
    const result = await this.client.setFigureSize(handle, width, height);
    if (!result.ok) throw new Error(`figure ${handle} could not be resized`);
    return this.figureFromActionResult(result, handle) ?? this.getFigure(handle);
  }
  //=============================================================================
  private async panFigure(params: Record<string, unknown>): Promise<FigureDisplayListResult> {
    const handle = this.figureHandle(Number(params.handle));
    const dxFrac = this.finiteNumber(params.dxFrac, 'horizontal pan');
    const dyFrac = this.finiteNumber(params.dyFrac, 'vertical pan');
    if (this.isInterpreterBusy()) {
      this.client.queueCommand(`__web_figure_pan__(${handle}, ${dxFrac}, ${dyFrac});`);
      return this.getFigure(handle);
    }
    const result = await this.client.panFigure(handle, dxFrac, dyFrac);
    if (!result.ok) throw new Error(`figure ${handle} has no axes to pan`);
    return this.figureFromActionResult(result, handle) ?? this.getFigure(handle);
  }
  //=============================================================================
  private figureFromActionResult(
    result: { figures?: NelsonFigureFrame[] },
    handle: number
  ): FigureDisplayListResult | null {
    const frame = result.figures?.find((candidate) => candidate.handle === handle);
    if (!frame) return null;
    this.retainFigureImagePayloads(frame);
    return this.cacheFigure(handle, this.hydrateFigureImages(displayList(frame)));
  }
  //=============================================================================
  private async sendFigureKeyEvent(
    params: Record<string, unknown>
  ): Promise<{ ok: boolean; figures: NelsonFigureFrame[] }> {
    const payload = {
      handle: this.figureHandle(Number(params.handle)),
      pressed: Boolean(params.pressed),
      character: String(params.character ?? ''),
      key: String(params.key ?? ''),
      shift: Boolean(params.shift),
      control: Boolean(params.control),
      alt: Boolean(params.alt),
      meta: Boolean(params.meta),
    };
    if (this.isInterpreterBusy()) {
      const logical = (value: boolean) => (value ? 'true' : 'false');
      this.client.queueCommand(
        `__web_figure_key_event__(${payload.handle}, ${logical(payload.pressed)}, ` +
          `${this.nelsonText(payload.character)}, ${this.nelsonText(payload.key)}, ` +
          `${logical(payload.shift)}, ${logical(payload.control)}, ${logical(payload.alt)}, ` +
          `${logical(payload.meta)});`
      );
      return { ok: true, figures: [] };
    }
    return this.client.sendFigureKeyEvent(payload);
  }
  //=============================================================================
  private async sendFigureMouseEvent(
    params: Record<string, unknown>
  ): Promise<{ ok: boolean; motion: boolean; figures: NelsonFigureFrame[] }> {
    const action = String(params.action ?? '') as 'press' | 'motion' | 'release';
    if (!['press', 'motion', 'release'].includes(action)) {
      throw new Error('Invalid WebAssembly figure mouse action');
    }
    const button = Number(params.button ?? 0);
    const shiftKey = Boolean(params.shiftKey ?? false);
    const ctrlKey = Boolean(params.ctrlKey ?? false);
    const doubleClick = Boolean(params.doubleClick ?? false);
    const payload = {
      handle: this.figureHandle(Number(params.handle)),
      action,
      x: this.finiteNumber(params.x, 'mouse x'),
      y: this.finiteNumber(params.y, 'mouse y'),
      button,
      shiftKey,
      ctrlKey,
      doubleClick,
    };
    if (this.isInterpreterBusy()) {
      const selectionType = resolveSelectionType(button, shiftKey, ctrlKey, doubleClick);
      this.client.queueCommand(
        `__figure_mouse_event__(${payload.handle}, ${this.nelsonText(action)}, ` +
          `[${payload.x} ${payload.y}], 'web', ${this.nelsonText(selectionType)});`
      );
      return { ok: true, motion: true, figures: [] };
    }
    return this.client.sendFigureMouseEvent(payload);
  }
  //=============================================================================
  private figureHandle(handle: number): number {
    if (!Number.isSafeInteger(handle) || handle < 0) {
      throw new Error('Invalid WebAssembly figure handle');
    }
    return handle;
  }
  //=============================================================================
  private finiteNumber(value: unknown, label: string): number {
    const number = Number(value);
    if (!Number.isFinite(number)) throw new Error(`Invalid WebAssembly ${label}`);
    return number;
  }
  //=============================================================================
  private async replaceVariable(name: string, values: unknown[][]): Promise<{ ok: true }> {
    const result = await this.client.replaceVariable(name, values);
    emitLocalPush('workspace.changed', {});
    emitLocalPush('var.changed', { name });
    return result;
  }
  //=============================================================================
  private async setNestedVariable(params: Record<string, unknown>): Promise<{ ok: true }> {
    const kind = String(params.kind ?? '') as 'struct' | 'cell';
    if (kind !== 'struct' && kind !== 'cell') {
      throw new Error('Invalid nested WebAssembly variable kind');
    }
    const name = String(params.name ?? '');
    const result = await this.client.setNestedVariable(
      name,
      kind,
      Number(params.row),
      Number(params.col),
      String(params.value ?? '')
    );
    emitLocalPush('workspace.changed', {});
    emitLocalPush('var.changed', { name });
    return result;
  }
  //=============================================================================
  private async sendUIControlAction(
    handle: number,
    value: number,
    text: string
  ): Promise<{ ok: boolean; figures: NelsonFigureFrame[] }> {
    if (this.isInterpreterBusy()) {
      if (!Number.isSafeInteger(handle) || handle < 0 || !Number.isFinite(value)) {
        throw new Error('Invalid WebAssembly UI control action');
      }
      this.client.queueCommand(
        `__web_uicontrol_action__(${handle}, ${value}, ${this.nelsonText(text)});`
      );
      return { ok: true, figures: [] };
    }
    return this.client.sendUIControlAction(handle, value, text);
  }
  //=============================================================================
  private async interrupt(requestedWhileBusy = false): Promise<{ interrupted: true }> {
    const wasEvaluating = this.isInterpreterBusy();
    const wasRunningInWorker = this.evaluationInWorker;
    if (wasEvaluating) {
      this.evaluationGeneration += 1;
      this.evaluationInProgress = false;
      this.evaluationInWorker = false;
      if (!wasRunningInWorker) this.runtimeResourceAbortController?.abort();
      this.externalEvaluationPreparationToken = null;
      this.acknowledgeExampleRun();
    }
    if (wasRunningInWorker) {
      await this.client.interrupt();
      this.invalidateMountedRuntimeResources();
    }
    this.cancelPendingFigureFlush();
    this.pendingFigureFrames.clear();
    this.pendingFigureFrameReceivedAt.clear();
    for (const handle of this.figures.keys()) emitLocalPush('figure.closed', { handle });
    this.figures.clear();
    this.figureTitles.clear();
    this.figureImages.clear();
    this.figureRgbaImages.clear();
    if (wasEvaluating || requestedWhileBusy) {
      emitLocalPush('term.output', { text: '\u001b[31mNelson evaluation interrupted\u001b[0m\n' });
    }
    emitLocalPush('term.prompt', {
      prompt: '>> ',
      token: ++WasmDesktopBackend.nextEvaluationPromptToken,
    });
    return { interrupted: true };
  }
  //=============================================================================
  private invalidateMountedRuntimeResources(): void {
    this.runtimeResourcePromises.clear();
    this.mountedRuntimeResourceBundles.clear();
    this.mountedRuntimeResourceFiles.clear();
  }
  //=============================================================================
  private cwdResult(): { cwd: string; separator: '/'; recentDirectories: string[] } {
    return { cwd: this.cwd, separator: '/', recentDirectories: [WORKSPACE] };
  }
  //=============================================================================
  private async changeDirectory(
    path: string
  ): Promise<ReturnType<WasmDesktopBackend['cwdResult']>> {
    const requested = path.replace(/\/$/, '') || WORKSPACE;
    const normalized = requested === '/' ? WORKSPACE : this.workspacePath(requested);
    const directories = await this.client.listDirectories();
    if (normalized !== WORKSPACE && !directories.includes(normalized)) {
      throw new Error(`browser directory does not exist: ${normalized}`);
    }
    this.cwd = normalized;
    return this.cwdResult();
  }
  //=============================================================================
  private async listDirectory(path: string): Promise<{ cwd: string; entries: FsEntry[] }> {
    const directory = path === '/' ? WORKSPACE : this.workspacePath(path);
    const [files, directories] = await Promise.all([
      this.client.listFiles(),
      this.client.listDirectories(),
    ]);
    const directChild = (entry: string) =>
      entry.startsWith(`${directory}/`) && !entry.slice(directory.length + 1).includes('/');
    const fileEntries = await Promise.all(
      files.filter(directChild).map(
        async (file): Promise<FsEntry> => ({
          name: file.slice(directory.length + 1),
          path: file,
          isDir: false,
          size: (await this.client.readFile(file)).byteLength,
        })
      )
    );
    const directoryEntries: FsEntry[] = directories.filter(directChild).map((entry) => ({
      name: entry.slice(directory.length + 1),
      path: entry,
      isDir: true,
      size: 0,
    }));
    return {
      cwd: directory,
      entries: [...directoryEntries, ...fileEntries].sort((left, right) =>
        left.name.localeCompare(right.name)
      ),
    };
  }
  //=============================================================================
  private async listWorkspace(): Promise<{
    scope: 'base';
    variables: {
      name: string;
      className: string;
      size: [number, number];
      bytes: number;
      value: string;
    }[];
  }> {
    const entries = (await this.client.listWorkspace()) as {
      name?: unknown;
      class?: unknown;
      size?: unknown;
      bytes?: unknown;
      sparse?: unknown;
    }[];
    return {
      scope: 'base',
      variables: entries.map((entry) => {
        const dimensions = Array.isArray(entry.size) ? entry.size.map(Number) : [];
        const baseClassName = String(entry.class ?? '');
        const className = entry.sparse ? `sparse ${baseClassName}` : baseClassName;
        return {
          name: String(entry.name ?? ''),
          className,
          size: [dimensions[0] ?? 0, dimensions[1] ?? 0],
          bytes: entry.sparse ? 0 : Number(entry.bytes ?? 0),
          value: `${dimensions.join('x')} ${className}`,
        };
      }),
    };
  }
  //=============================================================================
  private async complete(line: string): Promise<{
    prefix: string;
    builtin: string[];
    macros: string[];
    variables: string[];
    files: string[];
  }> {
    const result = (await this.client.complete(line)) as Record<string, unknown>;
    const list = (name: string): string[] => {
      const value = result[name];
      if (Array.isArray(value)) return value.map(String);
      return typeof value === 'string' && value ? [value] : [];
    };
    return {
      prefix: String(result.prefix ?? ''),
      builtin: [...list('builtin'), ...list('fields'), ...list('properties'), ...list('methods')],
      macros: list('macros'),
      variables: list('variables'),
      files: list('files'),
    };
  }
  //=============================================================================
  private async upload(
    path: string,
    data: string
  ): Promise<{ ok: true; path: string; size: number }> {
    const encodedLimit = this.options.runtimeCapabilities?.limits.maxVirtualFileBytes;
    if (encodedLimit && data.length > Math.ceil(encodedLimit / 3) * 4 + 4) {
      throw new Error(`browser import exceeds the ${encodedLimit}-byte limit`);
    }
    const bytes = bytesFromBase64(data);
    if (encodedLimit && bytes.byteLength > encodedLimit) {
      throw new Error(`browser import exceeds the ${encodedLimit}-byte limit`);
    }
    await this.client.writeFile(path, bytes);
    return { ok: true, path, size: bytes.byteLength };
  }
  //=============================================================================
  private async download(path: string): Promise<{ ok: true; name: string; data: string }> {
    // Example files are delivered on demand in the browser bundle. Materialize
    // them before reading so WebAssembly follows the same file contract as the
    // desktop/WebView backends.
    await this.materializeBrowserExample(this.browserFilePath(path));
    const bytes = await this.client.readFile(path);
    return { ok: true, name: path.split('/').pop() || 'nelson-file', data: base64FromBytes(bytes) };
  }
  //=============================================================================
  private async createFile(params: Record<string, unknown>): Promise<{ ok: true; path: string }> {
    const path = this.workspacePath(String(params.path ?? ''));
    if (params.kind === 'dir') {
      const result = await this.client.createDirectory(path);
      emitLocalPush('fs.changed', { path });
      return result;
    }
    await this.client.writeFile(path, new TextEncoder().encode(String(params.content ?? '')));
    emitLocalPush('fs.changed', { path });
    return { ok: true, path };
  }
  //=============================================================================
  private async renamePath(from: string, to: string): Promise<{ ok: true; path: string }> {
    const source = this.workspacePath(from);
    const target = this.workspacePath(to);
    const result = await this.client.renamePath(source, target);
    emitLocalPush('fs.changed', { path: target });
    return result;
  }
  //=============================================================================
  private async deletePath(path: string): Promise<{ ok: true }> {
    const target = this.workspacePath(path);
    const result = await this.client.deletePath(target);
    emitLocalPush('fs.changed', { path: target });
    return result;
  }
  //=============================================================================
  private workspacePath(path: string): string {
    const normalized = path.replaceAll('\\', '/');
    const absolute = normalized.startsWith('/') ? normalized : `${WORKSPACE}/${normalized}`;
    if (
      (absolute !== WORKSPACE && !absolute.startsWith(`${WORKSPACE}/`)) ||
      absolute.split('/').includes('..')
    ) {
      throw new Error('browser files must be below /workspace, /modules or /user-modules');
    }
    return absolute;
  }
  //=============================================================================
  private browserFilePath(path: string): string {
    const normalized = path.replaceAll('\\', '/');
    if (normalized.startsWith('/modules/') || normalized.startsWith('/user-modules/')) {
      if (normalized.split('/').includes('..')) {
        throw new Error('browser editor module paths cannot contain parent traversal');
      }
      return normalized;
    }
    return this.workspacePath(normalized);
  }
  //=============================================================================
  private browserExampleAssetUrl(path: string): string | null {
    if (!this.options.examplesUrl) return null;
    const match = /^\/modules\/([^/]+)\/examples\/(.+)$/i.exec(path.replaceAll('\\', '/'));
    if (!match || match[2].split('/').includes('..')) return null;
    return resolveVersionedAssetUrl(
      `./files/${encodeURIComponent(match[1])}/examples/${match[2]
        .split('/')
        .map((part) => encodeURIComponent(part))
        .join('/')}`,
      this.options.examplesUrl
    );
  }
  //=============================================================================
  private commandNeedsRuntimeResources(line: string): boolean {
    if (/^\s*help\b/i.test(line)) return this.helpCommandHasTopic(line);
    return /^\s*(?:test_run|bench_run|deployhelp|buildhelpweb)\b/i.test(line);
  }
  //=============================================================================
  private runtimeResourceModulesForLine(line: string): string[] | null {
    if (/^\s*help\b/i.test(line)) return ['help_tools'];
    if (/^\s*(?:deployhelp|buildhelpweb)\b/i.test(line)) return null;
    const match = /^\s*(?:test_run|bench_run)\s*\(\s*(['"])([^'"]+)\1/i.exec(line);
    if (!match) return null;
    const module = match[2].trim();
    if (!/^[A-Za-z][A-Za-z0-9_]*$/.test(module)) return null;
    return [...new Set([module, 'tests_manager'])].sort();
  }
  //=============================================================================
  private testRunNeedsHelpIndex(line: string, modules: string[] | null): boolean {
    if (!/^\s*test_run\b/i.test(line)) return false;
    return modules === null || modules.includes('help_tools');
  }
  //=============================================================================
  private helpCommandHasTopic(line: string): boolean {
    const trimmed = line.trim().replace(/;+\s*$/, '');
    const call = /^help\s*\((.*)\)$/i.exec(trimmed);
    if (call) return call[1].trim().length > 0;
    const command = /^help\b(.*)$/i.exec(trimmed);
    return Boolean(command && command[1].trim().length > 0);
  }
  //=============================================================================
  private runtimeResourceGroupAvailable(): boolean {
    const groups = this.options.deploymentManifest?.delivery?.onDemand;
    return (
      Array.isArray(groups) &&
      groups.some((group) => {
        if (!group || typeof group !== 'object') return false;
        const candidate = group as { id?: unknown; trigger?: unknown };
        return candidate.id === 'runtime-resources' || candidate.trigger === 'runtime-resources';
      })
    );
  }
  //=============================================================================
  private async packagedRuntimeResourceSelection(options: {
    modules: string[] | null;
    kinds: string[];
    archiveModules?: string[] | null;
  }): Promise<RuntimeResourceSelection> {
    const modules = options.modules ? new Set(options.modules) : null;
    const kinds = new Set(options.kinds);
    const archiveModules = options.archiveModules
      ? new Set(options.archiveModules.map((module) => module.toLowerCase()))
      : null;
    const bundles: string[] = [];
    const bundleFileCounts = new Map<string, number>();
    const index = await this.runtimeResourcesIndex();
    const indexModules = (index as { modules?: unknown })?.modules;
    const hasBundleIndex =
      Array.isArray(indexModules) &&
      indexModules.some((entry) => {
        const candidate = entry as { bundles?: unknown };
        return candidate && typeof candidate === 'object' && typeof candidate.bundles === 'object';
      });
    if (Array.isArray(indexModules)) {
      for (const entry of indexModules) {
        if (!entry || typeof entry !== 'object' || Array.isArray(entry)) continue;
        const module = String((entry as { module?: unknown }).module ?? '');
        if (!module || (modules && !modules.has(module))) continue;
        const moduleBundles = (entry as { bundles?: Record<string, unknown> }).bundles;
        if (!moduleBundles || typeof moduleBundles !== 'object') continue;
        for (const kind of ['tests', 'help', 'archives']) {
          if (!kinds.has(kind)) continue;
          const bundleValue = moduleBundles[kind];
          const candidates = Array.isArray(bundleValue) ? bundleValue : [bundleValue];
          for (const bundle of candidates) {
            if (kind === 'archives' && archiveModules) {
              const files =
                bundle && typeof bundle === 'object'
                  ? (bundle as { files?: unknown }).files
                  : undefined;
              const archiveName = Array.isArray(files)
                ? files.find((file) => typeof file === 'string')
                : undefined;
              const matchesRequestedModule =
                typeof archiveName === 'string' &&
                [...archiveModules].some((module) =>
                  archiveName.toLowerCase().includes(`.modules.${module}.help.`)
                );
              if (!matchesRequestedModule) continue;
            }
            const file =
              bundle && typeof bundle === 'object'
                ? String((bundle as { file?: unknown }).file ?? '')
                : '';
            if (file && !file.split('/').includes('..')) bundles.push(file);
            if (file && !file.split('/').includes('..')) {
              const files = (bundle as { files?: unknown }).files;
              bundleFileCounts.set(file, Array.isArray(files) ? files.length : 1);
            }
          }
        }
      }
    }
    const manifestFiles = this.options.deploymentManifest?.files;
    if (!Array.isArray(manifestFiles)) return { bundles, bundleFileCounts, files: [] };
    const files = manifestFiles
      .map((entry) =>
        entry && typeof entry === 'object' ? String((entry as { file?: unknown }).file ?? '') : ''
      )
      .filter((file) => {
        const runtimeMatch = /^runtime-resources\/modules\/([^/]+)\/(tests|help)\//.exec(file);
        const examplesMatch = /^examples\/files\/([^/]+)\/(examples)\//.exec(file);
        const match = runtimeMatch ?? examplesMatch;
        if (!match) return false;
        if (file.split('/').includes('..')) return false;
        if (runtimeMatch && hasBundleIndex) return false;
        if (modules && !modules.has(match[1])) return false;
        if (!kinds.has(match[2])) return false;
        return true;
      });
    return { bundles, bundleFileCounts, files };
  }
  //=============================================================================
  private runtimeResourceAssetUrl(packagedFile: string): string | null {
    if (packagedFile.split('/').includes('..')) return null;
    const runtimePrefix = 'runtime-resources/';
    if (packagedFile.startsWith(runtimePrefix)) {
      if (!this.options.runtimeResourcesUrl) return null;
      return resolveVersionedAssetUrl(
        `./${packagedFile
          .slice(runtimePrefix.length)
          .split('/')
          .map((part) => encodeURIComponent(part))
          .join('/')}`,
        this.options.runtimeResourcesUrl
      );
    }
    const examplesPrefix = 'examples/';
    if (!packagedFile.startsWith(examplesPrefix)) return null;
    if (!this.options.examplesUrl) return null;
    return resolveVersionedAssetUrl(
      `./${packagedFile
        .slice(examplesPrefix.length)
        .split('/')
        .map((part) => encodeURIComponent(part))
        .join('/')}`,
      this.options.examplesUrl
    );
  }
  //=============================================================================
  private runtimeResourceVirtualPath(packagedFile: string): string | null {
    if (packagedFile.split('/').includes('..')) return null;
    const runtimePrefix = 'runtime-resources/modules/';
    if (packagedFile.startsWith(runtimePrefix)) {
      const relative = packagedFile.slice(runtimePrefix.length);
      // Archives are delivered through help_tools but must be visible in the
      // owning module's help directory, which is the path used by
      // deployhelp/buildhelpweb on every other Nelson target.
      const archive =
        /^help_tools\/help\/(io\.github\.nelson_lang\.Nelson\.modules\.([A-Za-z0-9_]+)\.help\.[A-Za-z0-9_]+\.nhz)$/i.exec(
          relative
        );
      if (archive) return `/modules/${archive[2]}/help/${archive[1]}`;
      return `/modules/${relative}`;
    }
    const match = /^examples\/files\/([^/]+)\/examples\/(.+)$/.exec(packagedFile);
    if (!match || match[2].split('/').includes('..')) return null;
    return `/modules/${match[1]}/examples/${match[2]}`;
  }
  //=============================================================================
  private async ensureRuntimeResourcesMountedForLine(line: string): Promise<void> {
    const command = /^\s*(test_run|bench_run|deployhelp|buildhelpweb|help)\b/i
      .exec(line)?.[1]
      ?.toLowerCase();
    const kinds =
      command === 'help'
        ? ['help']
        : command === 'deployhelp' || command === 'buildhelpweb'
        ? ['archives']
        : ['tests', 'examples'];
    const modules = this.runtimeResourceModulesForLine(line);
    await this.ensureRuntimeResourcesMountedForModules({
      modules,
      kinds,
    });
    if (this.testRunNeedsHelpIndex(line, modules)) {
      await this.ensureRuntimeResourcesMountedForModules({
        modules: ['help_tools'],
        kinds: ['help'],
      });
    }
  }
  //=============================================================================
  private async ensureRuntimeResourcesMountedForModules(options: {
    modules: string[] | null;
    kinds: string[];
    archiveModules?: string[] | null;
  }): Promise<void> {
    if (!this.options.runtimeResourcesUrl || !this.runtimeResourceGroupAvailable()) return;
    const key = JSON.stringify({
      modules: options.modules ?? '*',
      kinds: [...options.kinds].sort(),
      archiveModules: options.archiveModules ?? '*',
    });
    const selection = await this.packagedRuntimeResourceSelection(options);
    if (!selection.files.length && !selection.bundles.length) return;
    let promise = this.runtimeResourcePromises.get(key);
    if (!promise) {
      const controller = new AbortController();
      this.runtimeResourceAbortController = controller;
      promise = this.mountRuntimeResources(selection, controller.signal)
        .catch((error) => {
          this.runtimeResourcePromises.delete(key);
          throw error;
        })
        .finally(() => {
          if (this.runtimeResourceAbortController === controller) {
            this.runtimeResourceAbortController = null;
          }
        });
      this.runtimeResourcePromises.set(key, promise);
    }
    await promise;
  }
  //=============================================================================
  private async runtimeResourcesIndex(): Promise<unknown> {
    if (!this.runtimeResourcesIndexPromise) {
      this.runtimeResourcesIndexPromise = this.fetchRequired(
        this.options.runtimeResourcesUrl ?? '',
        'Runtime resources index'
      ).then((bytes) => JSON.parse(new TextDecoder().decode(bytes)) as unknown);
    }
    return this.runtimeResourcesIndexPromise;
  }
  //=============================================================================
  private async mountRuntimeResources(
    selection: RuntimeResourceSelection,
    signal: AbortSignal
  ): Promise<void> {
    const limit = this.options.runtimeCapabilities?.limits.maxVirtualFileBytes ?? 32 * 1024 * 1024;
    const batchSize = 16;
    // Help archives are small independent requests. A wide batch keeps deployhelp
    // responsive on localhost and GitHub Pages while bounding memory (at most 24
    // archives are fetched before they are written to the worker).
    const bundleBatchSize = 24;
    const pendingBundles = selection.bundles.filter(
      (file) => !this.mountedRuntimeResourceBundles.has(file)
    );
    const pendingFiles = selection.files.filter(
      (file) => !this.mountedRuntimeResourceFiles.has(file)
    );
    const pendingLogicalFiles =
      pendingFiles.length +
      pendingBundles.reduce((sum, file) => sum + (selection.bundleFileCounts.get(file) ?? 1), 0);
    const totalBytes = this.runtimeResourceBytes([...pendingBundles, ...pendingFiles]);
    const reportProgress = pendingLogicalFiles >= 128;
    const progressId = reportProgress ? ++WasmDesktopBackend.nextRuntimeResourceProgressId : 0;
    if (reportProgress) {
      this.emitRuntimeResourceProgress(
        progressId,
        `Preparing WebAssembly runtime resources (${this.formatBytes(totalBytes)})`,
        0,
        pendingLogicalFiles,
        `${pendingLogicalFiles} files to mount`
      );
      emitLocalPush('term.output', {
        text: `Preparing WebAssembly runtime resources (${pendingLogicalFiles} files, ${this.formatBytes(
          totalBytes
        )}). Please wait...\n`,
      });
    }
    let completed = 0;
    try {
      for (let index = 0; index < pendingBundles.length; index += bundleBatchSize) {
        const batch = pendingBundles.slice(index, index + bundleBatchSize);
        const downloaded = await Promise.all(
          batch.map(async (packagedBundle) => {
            const url = this.runtimeResourceAssetUrl(packagedBundle);
            if (!url) return null;
            const bytes = await this.fetchRequired(url, 'Runtime resource bundle', limit, signal);
            return { packagedBundle, resources: this.unpackRuntimeResourceBundle(bytes) };
          })
        );
        for (const bundle of downloaded) {
          if (!bundle) continue;
          await this.writePackagedRuntimeResources(bundle.resources);
          for (const resource of bundle.resources)
            this.mountedRuntimeResourceFiles.add(resource.file);
          completed += bundle.resources.length;
          this.mountedRuntimeResourceBundles.add(bundle.packagedBundle);
          if (reportProgress) {
            this.emitRuntimeResourceProgress(
              progressId,
              `Preparing WebAssembly runtime resources (${this.formatBytes(totalBytes)})`,
              completed,
              pendingLogicalFiles,
              `Mounted ${completed}/${pendingLogicalFiles} files`
            );
          }
          if (reportProgress && (completed === pendingLogicalFiles || completed % 512 === 0)) {
            emitLocalPush('term.output', {
              text: `  mounted ${completed}/${pendingLogicalFiles} runtime resource files\n`,
            });
          }
        }
      }
      for (let index = 0; index < pendingFiles.length; index += batchSize) {
        const batch = pendingFiles.slice(index, index + batchSize);
        await Promise.all(
          batch.map(async (packagedFile) => {
            const url = this.runtimeResourceAssetUrl(packagedFile);
            if (!url) return;
            const bytes = await this.fetchRequired(url, 'Runtime resource', limit, signal);
            await this.writePackagedRuntimeResource(packagedFile, bytes);
            this.mountedRuntimeResourceFiles.add(packagedFile);
          })
        );
        completed += batch.length;
        if (reportProgress) {
          this.emitRuntimeResourceProgress(
            progressId,
            `Preparing WebAssembly runtime resources (${this.formatBytes(totalBytes)})`,
            completed,
            pendingLogicalFiles,
            `Mounted ${completed}/${pendingLogicalFiles} files`
          );
        }
        if (reportProgress && (completed === pendingLogicalFiles || completed % 512 === 0)) {
          emitLocalPush('term.output', {
            text: `  mounted ${completed}/${pendingLogicalFiles} runtime resource files\n`,
          });
        }
      }
    } finally {
      if (reportProgress) {
        emitLocalPush('dialog.progress', {
          id: progressId,
          visible: false,
          title: 'Preparing WebAssembly runtime resources',
          message: '',
          value: completed,
          minimum: 0,
          maximum: pendingLogicalFiles,
          indeterminate: false,
        });
      }
    }
  }
  //=============================================================================
  private emitRuntimeResourceProgress(
    id: number,
    title: string,
    value: number,
    maximum: number,
    message: string
  ): void {
    emitLocalPush('dialog.progress', {
      id,
      visible: true,
      title,
      message,
      value,
      minimum: 0,
      maximum,
      indeterminate: maximum <= 0,
    });
  }
  //=============================================================================
  private async writePackagedRuntimeResource(
    packagedFile: string,
    bytes: Uint8Array
  ): Promise<void> {
    const path = this.runtimeResourceVirtualPath(packagedFile);
    if (!path) return;
    await this.client.writeFile(path, bytes);
  }
  //=============================================================================
  private async writePackagedRuntimeResources(
    resources: { file: string; bytes: Uint8Array }[]
  ): Promise<void> {
    const files = resources
      .map((resource) => {
        const path = this.runtimeResourceVirtualPath(resource.file);
        return path ? { path, data: resource.bytes } : null;
      })
      .filter((file): file is { path: string; data: Uint8Array } => file !== null);
    if (!files.length) return;
    if (this.client.writeFiles) {
      await this.client.writeFiles(files);
      return;
    }
    for (const file of files) await this.client.writeFile(file.path, file.data);
  }
  //=============================================================================
  private unpackRuntimeResourceBundle(bytes: Uint8Array): { file: string; bytes: Uint8Array }[] {
    const magic = new TextDecoder().decode(bytes.slice(0, 8));
    if (magic !== 'NLRPACK1' || bytes.byteLength < 12) {
      throw new Error('Runtime resource bundle has an unsupported format');
    }
    const headerLength = new DataView(bytes.buffer, bytes.byteOffset + 8, 4).getUint32(0, true);
    const payloadOffset = 12 + headerLength;
    const header = JSON.parse(new TextDecoder().decode(bytes.slice(12, payloadOffset))) as {
      formatVersion?: unknown;
      files?: unknown[];
    };
    if (header.formatVersion !== 1 || !Array.isArray(header.files)) {
      throw new Error('Runtime resource bundle manifest is invalid');
    }
    return header.files.map((entry) => {
      const record = entry as { file?: unknown; offset?: unknown; bytes?: unknown };
      const file = String(record.file ?? '');
      const offset = Number(record.offset);
      const length = Number(record.bytes);
      if (
        !file ||
        file.split('/').includes('..') ||
        !Number.isSafeInteger(offset) ||
        !Number.isSafeInteger(length) ||
        offset < 0 ||
        length < 0 ||
        payloadOffset + offset + length > bytes.byteLength
      ) {
        throw new Error('Runtime resource bundle entry is invalid');
      }
      return { file, bytes: bytes.slice(payloadOffset + offset, payloadOffset + offset + length) };
    });
  }
  //=============================================================================
  private runtimeResourceBytes(files: string[]): number {
    const manifestFiles = this.options.deploymentManifest?.files;
    if (!Array.isArray(manifestFiles)) return 0;
    const requested = new Set(files);
    let total = 0;
    for (const entry of manifestFiles) {
      if (!entry || typeof entry !== 'object') continue;
      const file = String((entry as { file?: unknown }).file ?? '');
      if (!requested.has(file)) continue;
      const bytes = Number((entry as { bytes?: unknown }).bytes);
      if (Number.isFinite(bytes) && bytes > 0) total += bytes;
    }
    return total;
  }
  //=============================================================================
  private formatBytes(bytes: number): string {
    if (!Number.isFinite(bytes) || bytes <= 0) return 'unknown size';
    if (bytes < 1024 * 1024) return `${Math.round(bytes / 1024)} KiB`;
    return `${(bytes / 1024 / 1024).toFixed(1)} MiB`;
  }
  //=============================================================================
  private browserExamplePackagedFile(path: string): string | null {
    const match = /^\/modules\/([^/]+)\/examples\/(.+)$/i.exec(path.replaceAll('\\', '/'));
    if (!match || match[2].split('/').includes('..')) return null;
    return `examples/files/${match[1]}/examples/${match[2]}`;
  }
  //=============================================================================
  private browserExampleVirtualPath(packagedFile: string): string | null {
    const match = /^examples\/files\/([^/]+)\/examples\/(.+)$/.exec(packagedFile);
    if (!match || match[2].split('/').includes('..')) return null;
    return `/modules/${match[1]}/examples/${match[2]}`;
  }
  //=============================================================================
  private packagedExampleSiblings(path: string): string[] {
    const packagedFile = this.browserExamplePackagedFile(path);
    const manifestFiles = this.options.deploymentManifest?.files;
    if (!packagedFile || !Array.isArray(manifestFiles)) return [];
    const directory = packagedFile.slice(0, packagedFile.lastIndexOf('/') + 1);
    const fileName = packagedFile.slice(packagedFile.lastIndexOf('/') + 1);
    const assetDirectory = `${directory}${fileName.replace(/\.m$/i, '')}_assets/`;
    return manifestFiles
      .map((entry) =>
        entry && typeof entry === 'object' ? String((entry as { file?: unknown }).file ?? '') : ''
      )
      .filter((file) => {
        if (file.startsWith(assetDirectory)) return true;
        return file.startsWith(directory) && !file.slice(directory.length).includes('/');
      })
      .map((file) => this.browserExampleVirtualPath(file))
      .filter((file): file is string => file !== null);
  }
  //=============================================================================
  private async materializeBrowserExample(path: string): Promise<void> {
    const workspacePath = this.browserFilePath(path);
    const siblings = new Set([workspacePath, ...this.packagedExampleSiblings(workspacePath)]);
    await Promise.all([...siblings].map((sibling) => this.materializeBrowserExampleFile(sibling)));
  }
  //=============================================================================
  private async materializeBrowserExampleFile(path: string): Promise<void> {
    const url = this.browserExampleAssetUrl(path);
    if (!url) return;
    try {
      const existing = await this.client.readFile(path);
      if (existing.byteLength > 0) return;
    } catch {
      // Missing files are populated from the packaged examples below.
    }
    const bytes = await this.fetchRequired(url, 'Example source');
    await this.client.writeFile(path, bytes);
  }
  //=============================================================================
  private async materializeBrowserExamplesReferencedByLine(line: string): Promise<void> {
    if (!this.options.examplesUrl || !line.includes('/modules/')) return;
    const paths = new Set<string>();
    const pattern = /(['"])(\/modules\/[^'"]+\/examples\/[^'"]+\.m)\1/g;
    for (const match of line.matchAll(pattern)) {
      if (typeof match[2] === 'string') paths.add(match[2]);
    }
    await Promise.all([...paths].map((path) => this.materializeBrowserExample(path)));
  }
  //=============================================================================
  private async openEditor(
    path: string
  ): Promise<{ path: string; content: string; language: string }> {
    const workspacePath = this.browserFilePath(path || 'untitled.m');
    await this.materializeBrowserExample(workspacePath);
    let content = '';
    try {
      content = new TextDecoder().decode(await this.client.readFile(workspacePath));
    } catch {
      // A new buffer is materialized in the virtual filesystem on its first save.
    }
    return {
      path,
      content,
      language: /\.m$/i.test(workspacePath) ? 'nelson' : 'plaintext',
    };
  }
  //=============================================================================
  private async saveEditor(path: string, content: string): Promise<{ ok: true; path: string }> {
    const workspacePath = this.browserFilePath(path || 'untitled.m');
    await this.client.writeFile(workspacePath, new TextEncoder().encode(content));
    emitLocalPush('fs.changed', { path: workspacePath });
    return { ok: true, path: workspacePath };
  }
}
//=============================================================================
