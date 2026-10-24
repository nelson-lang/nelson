// ============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================
declare module '@nelson/wasm-client' {
  export interface NFlowWasmWorkerClient {
    simulateNFlow(diagramJson: string): Promise<unknown>;
    readFile(path: string): Promise<Uint8Array>;
    writeFile(path: string, data: Uint8Array | string): Promise<unknown>;
    listFiles(): Promise<string[]>;
    listDirectories(): Promise<string[]>;
    registerNFlowFmu(model: import('./fmu.js').WasmFmuModel): Promise<unknown>;
    evaluate(
      code: string,
      onOutput?: (output: { stream?: string; text?: string }) => void
    ): Promise<{ stdout?: string; uiActions?: Array<{ type?: string; path?: string }> }>;
    interrupt(): Promise<unknown>;
    onNFlowPartial(handler: (partial: unknown) => void): () => void;
    /** dedicated: own Worker; owner: own Worker shared with other pages; client: relayed. */
    readonly role: 'dedicated' | 'owner' | 'client';
    /** Other pages currently served by this page's engine (owner only). */
    readonly peers: number;
    onPeersChange(handler: (peers: number) => void): () => void;
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
    runtimeCapabilities: unknown;
    onProgress?: (progress: NelsonLoadingProgress) => void;
  }): Promise<NFlowWasmWorkerClient>;
}
