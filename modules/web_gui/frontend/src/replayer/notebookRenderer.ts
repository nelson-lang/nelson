//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Notebook renderer entry point. A host that shows notebook outputs (such as a
// VS Code NotebookRenderer) loads this module, hands each output item to
// renderOutputItem, and the scene is drawn.
//
// Every output is drawn static first, so it renders with nothing running behind
// it. When the host gave a two-way messaging channel and the scene carries a
// figure handle, the output then asks the extension whether a live kernel backs
// it. If one does, the static drawing is replaced by the interactive figure
// panel, driven over that channel; if not (no kernel, or a notebook reloaded
// from disk), the static drawing stays.
//=============================================================================
import {
  renderNelsonFigureScene,
  isNelsonFigureScene,
  sceneFigureHandle,
  type NelsonFigureScene,
  type SceneReplayHandle,
} from './sceneReplayer';
import { NotebookFigureBridge, type RendererMessaging } from './notebookBridge';
import { mountLiveFigure, type LiveFigureHandle } from './notebookFigureLive';
//=============================================================================
interface OutputItem {
  readonly id: string;
  readonly mime: string;
  json(): unknown;
  text(): string;
}
//=============================================================================
export interface NotebookRendererApi {
  renderOutputItem(outputItem: OutputItem, element: HTMLElement): Promise<void>;
  disposeOutputItem(id?: string): void;
}
//=============================================================================
interface OutputRecord {
  element: HTMLElement;
  scene: NelsonFigureScene;
  handle: number;
  disposed: boolean;
  staticFigure: SceneReplayHandle | null;
  liveFigure: LiveFigureHandle | null;
}
//=============================================================================
export function activate(context?: RendererMessaging): NotebookRendererApi {
  // The renderer is one self-contained module, so the 2D canvas paints on the
  // main thread rather than pulling in a separate worker file. The WebGL path
  // (3D surfaces, rotation) is unaffected.
  try {
    window.localStorage.setItem('nelson.figureCanvas2DRenderer', 'main');
  } catch {
    // A sandboxed host without storage still paints, it only cannot pin the mode.
  }
  const records = new Map<string, OutputRecord>();
  const bridge =
    context?.postMessage || context?.onDidReceiveMessage ? new NotebookFigureBridge(context) : null;

  const drawStatic = async (record: OutputRecord): Promise<void> => {
    record.element.replaceChildren();
    record.staticFigure = await renderNelsonFigureScene(record.element, record.scene);
  };

  const clearRecord = (record: OutputRecord): void => {
    record.liveFigure?.destroy();
    record.liveFigure = null;
    record.staticFigure?.destroy();
    record.staticFigure = null;
  };

  // Replace the static drawing with the interactive panel once a live kernel is
  // confirmed. A figure.closed later drops the output back to static.
  const goLive = (record: OutputRecord): void => {
    if (record.disposed || record.liveFigure) return;
    record.staticFigure?.destroy();
    record.staticFigure = null;
    record.element.replaceChildren();
    record.liveFigure = mountLiveFigure(
      record.element,
      record.handle,
      `Figure ${record.handle + 1}`
    );
  };

  const goStatic = (record: OutputRecord): void => {
    if (record.disposed || !record.liveFigure) return;
    record.liveFigure.destroy();
    record.liveFigure = null;
    void drawStatic(record);
  };

  const dispose = (id: string): void => {
    const record = records.get(id);
    if (!record) return;
    record.disposed = true;
    records.delete(id);
    clearRecord(record);
    bridge?.bye(id);
  };

  return {
    async renderOutputItem(outputItem: OutputItem, element: HTMLElement): Promise<void> {
      dispose(outputItem.id);
      element.replaceChildren();
      let data: unknown;
      try {
        data = outputItem.json();
      } catch {
        try {
          data = JSON.parse(outputItem.text());
        } catch {
          data = null;
        }
      }
      if (!isNelsonFigureScene(data)) {
        const message = document.createElement('div');
        message.textContent = 'Nelson figure: unsupported or malformed scene.';
        element.appendChild(message);
        return;
      }
      const record: OutputRecord = {
        element,
        scene: data,
        handle: sceneFigureHandle(data),
        disposed: false,
        staticFigure: null,
        liveFigure: null,
      };
      records.set(outputItem.id, record);
      await drawStatic(record);
      if (record.disposed) return;

      // Only a scene that carries a figure handle can go live, and only when the
      // host opened a channel. Everything else is left as the static drawing.
      if (!bridge || !bridge.canGoLive() || record.handle < 0) return;
      try {
        const init = await bridge.requestLive(outputItem.id, record.handle, () => goStatic(record));
        if (record.disposed) {
          bridge.bye(outputItem.id);
          return;
        }
        if (init.live) goLive(record);
      } catch {
        // A channel that never answered leaves the static drawing in place.
      }
    },
    disposeOutputItem(id?: string): void {
      if (id === undefined) {
        for (const key of [...records.keys()]) dispose(key);
        bridge?.dispose();
        return;
      }
      dispose(id);
    },
  };
}
//=============================================================================
