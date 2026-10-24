//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
declare module '@nelson/wasm-client' {
  export interface RuntimeCapabilityManifest {
    formatVersion: 1;
    runtime: 'webassembly';
    profile: string;
    modules: string[];
    capabilities: string[];
    features: {
      blas: boolean;
      controlSystem: boolean;
      lapack: boolean;
      nflow: boolean;
      odeSolvers: boolean;
      packageManager: boolean;
      plots: boolean;
      slicot: boolean;
      virtualFilesystem: boolean;
      worker: boolean;
    };
    limits: {
      maxNflowJsonBytes: number;
      maxSourceBytes: number;
      maxVariableBlockCells: number;
      maxVirtualFileBytes: number;
      maxWasmBytes: number;
    };
    unavailable: string[];
  }

  export interface NelsonFigureFrame {
    handle: number;
    displayList: unknown;
  }

  export interface NelsonEvaluationResult {
    figureList?: Array<{ handle: number; title: string; windowStyle?: string }>;
    exitCode: number;
    stdout: string;
    stderr: string;
    errorIdentifier?: string;
    errorMessage?: string;
    figures: NelsonFigureFrame[];
    uiActions?: (
      | { type: 'open-examples' }
      | { type: 'open-nflow'; path?: string }
      | { type: 'open-editor'; path: string; line?: number }
      | { type: 'open-notebook'; path: string }
      | {
          type: 'play-audio';
          sampleRate: number;
          channels: number;
          frames: number;
          samples: number[];
        }
      | {
          type: 'audio-buffer-begin';
          id: number;
          sampleRate: number;
          channels: number;
          frames: number;
        }
      | {
          type: 'audio-buffer-chunk';
          id: number;
          offset: number;
          frames: number;
          samples?: number[];
          samplesF32Base64?: string;
          samplesF32Buffer?: ArrayBuffer;
        }
      | { type: 'audio-buffer-play'; id: number }
      | { type: 'audio-control'; id: number; command: 'pause' | 'resume' | 'stop' }
    )[];
  }

  export interface NelsonOutputChunk {
    stream: 'stdout' | 'stderr' | 'control';
    text?: string;
    clear?: boolean;
    html?: string;
    expandable?: { id: number; text: string };
  }

  export interface NelsonWorkerClientApi {
    figureAction(handle: number, request: Record<string, unknown>): Promise<unknown>;
    onFigureList?(handler: (figures: Array<{ handle: number; title: string }>) => void): () => void;
    evaluate(
      code: string,
      onOutput?: (output: NelsonOutputChunk) => void
    ): Promise<NelsonEvaluationResult>;
    clearCurrentFigure?(): Promise<{ ok: true }>;
    queueCommand(code: string): { queued: true };
    simulateNFlow(diagramJson: string): Promise<unknown>;
    writeFile(path: string, data: Uint8Array): Promise<{ path: string; bytes: number }>;
    writeFiles?(
      files: Array<{ path: string; data: Uint8Array }>
    ): Promise<{ files: number; bytes: number }>;
    readFile(path: string): Promise<Uint8Array>;
    listFiles(): Promise<string[]>;
    listDirectories(): Promise<string[]>;
    createDirectory(path: string): Promise<{ ok: true; path: string }>;
    renamePath(from: string, to: string): Promise<{ ok: true; path: string }>;
    deletePath(path: string): Promise<{ ok: true }>;
    listWorkspace(): Promise<unknown[]>;
    openVariable(name: string): Promise<unknown>;
    getVariableBlock(
      name: string,
      row0: number,
      col0: number,
      rows: number,
      cols: number
    ): Promise<unknown>;
    replaceVariable(name: string, values: unknown[][]): Promise<{ ok: true }>;
    setNestedVariable(
      name: string,
      kind: 'struct' | 'cell',
      row: number,
      col: number,
      value: string
    ): Promise<{ ok: true }>;
    complete(line: string): Promise<unknown>;
    analyzeCode(
      path: string,
      source: string
    ): Promise<{
      diagnostics: Array<{
        severity: string;
        message: string;
        ruleName: string;
        checkId: string;
        category: string;
        helpUri: string;
        lineStart: number;
        lineEnd: number;
        columnStart: number;
        columnEnd: number;
        fixes: Array<{
          title: string;
          safe: boolean;
          edits: Array<{
            lineStart: number;
            lineEnd: number;
            columnStart: number;
            columnEnd: number;
            replacementText: string;
          }>;
        }>;
      }>;
    }>;
    debuggerState(): Promise<{
      running: boolean;
      currentFile: string | null;
      currentLine: number | null;
      stack: Array<{ file: string; line: number; function: string }>;
    }>;
    debuggerGetBreakpoints(file: string): Promise<{ file: string; lines: number[] }>;
    debuggerToggleBreakpoint(
      file: string,
      line: number
    ): Promise<{ file: string; lines: number[] }>;
    setFigureView(
      handle: number,
      azimuth: number,
      elevation: number,
      compactView?: boolean,
      axesHandle?: number
    ): Promise<unknown>;
    setFigureSize(
      handle: number,
      width: number,
      height: number
    ): Promise<{ ok: boolean; figures: NelsonFigureFrame[] }>;
    panFigure(
      handle: number,
      dxFrac: number,
      dyFrac: number
    ): Promise<{ ok: boolean; figures: NelsonFigureFrame[] }>;
    closeFigure(handle: number): Promise<{ ok: boolean; figures: NelsonFigureFrame[] }>;
    getFigureImage(handle: number): Promise<{ handle: number; image: string }>;
    saveFigure(handle: number, path: string): Promise<{ ok: true; path: string }>;
    sendFigureKeyEvent(payload: {
      handle: number;
      pressed: boolean;
      character: string;
      key: string;
      shift: boolean;
      control: boolean;
      alt: boolean;
      meta: boolean;
    }): Promise<{ ok: boolean; figures: NelsonFigureFrame[] }>;
    sendFigureMouseEvent(payload: {
      handle: number;
      action: 'press' | 'motion' | 'release';
      x: number;
      y: number;
      button?: number;
      shiftKey?: boolean;
      ctrlKey?: boolean;
      doubleClick?: boolean;
    }): Promise<{ ok: boolean; motion: boolean; figures: NelsonFigureFrame[] }>;
    sendUIControlAction(
      handle: number,
      value: number,
      text: string
    ): Promise<{ ok: boolean; figures: NelsonFigureFrame[] }>;
    reset(): Promise<{ reset: true }>;
    interrupt(): Promise<{ capabilities: string[] }>;
    respondInput(inputId: number, value: string): Promise<{ ok: true }>;
    onFigure(handler: (frame: NelsonFigureFrame) => void): () => void;
    onAudioAction(
      handler: (action: NonNullable<NelsonEvaluationResult['uiActions']>[number]) => void
    ): () => void;
    onNFlowPartial(handler: (partial: unknown) => void): () => void;
    onDebuggerState(
      handler: (state: {
        running: boolean;
        currentFile: string | null;
        currentLine: number | null;
        stack: Array<{ file: string; line: number; function: string }>;
      }) => void
    ): () => void;
    onProgress(handler: (progress: NelsonLoadingProgress) => void): () => void;
    onInputRequest(handler: (request: { id: number; prompt: string }) => void): () => void;
    /** dedicated: own Worker; owner: own Worker shared with other pages; client: relayed. */
    readonly role: 'dedicated' | 'owner' | 'client';
    /** Other pages currently served by this page's engine (owner only). */
    readonly peers: number;
    onPeersChange(handler: (peers: number) => void): () => void;
    /** Another page of the shared engine changed the virtual filesystem. */
    onFilesChanged?(handler: (event: { origin: string }) => void): () => void;
    onEngineRestart(handler: (event: { reason?: string }) => void): () => void;
    terminate(): void;
  }

  export interface NelsonLoadingProgress {
    phase: 'wasm' | 'resources' | 'initializing' | 'ready';
    label: string;
    loaded?: number;
    total?: number;
  }

  export function createNelsonWorkerClient(options: {
    workerUrl: string;
    /** SharedWorker broker URL; omitted or unsupported means a dedicated Worker. */
    sharedWorkerUrl?: string;
    moduleUrl: string;
    wasmUrl?: string;
    wasmBytes?: number;
    runtimeCapabilities: RuntimeCapabilityManifest;
    onProgress?: (progress: NelsonLoadingProgress) => void;
  }): Promise<NelsonWorkerClientApi>;
}
