//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { renderBlockPanel, type BlockPanelDeps } from './inspector-block.js';
import { renderInspectorReact, unmountInspectorReact } from './inspector-react-root.js';
import { state } from '../app/state.js';
import {
  portCountsChanged,
  actualPortCounts,
  survivingConnections,
  type SimpleConnection,
} from './port-rebuild.js';
//=============================================================================
export type Block = {
  type?: string;
  kind?: string;
  params: Record<string, unknown> & { _visible?: Record<string, unknown>; [k: string]: unknown };
  blocks?: number;
  connections?: number;
  fromId?: unknown;
  toId?: unknown;
  fromType?: string;
  toType?: string;
  toIndex?: number;
  label?: string;
  width?: number | string;
  height?: number | string;
  computedLimits?: Record<string, number>;
  paramLabels?: Record<string, string>;
  [k: string]: unknown;
};
//=============================================================================
export type Renderer = {
  current?: unknown;
  updateBlockLabel?: (b: Block) => void;
  resizeBlock?: (b: Block, w: number, h: number) => void;
  state?: { blocks?: Map<unknown, unknown> };
};
//=============================================================================
function currentCanvasBackground(): string {
  if (state.canvasBackground && String(state.canvasBackground).trim()) {
    return String(state.canvasBackground).trim();
  }
  if (typeof document === 'undefined') return '#ffffff';
  return (
    getComputedStyle(document.documentElement).getPropertyValue('--canvas-background') || '#ffffff'
  ).trim();
}
//=============================================================================
function CanvasInspectorPanel({
  color,
  signalDiagramChanged,
}: {
  color: string;
  signalDiagramChanged?: () => void;
}): JSX.Element {
  return (
    <div className="sidebar-section">
      <div className="sidebar-section-title">Canvas</div>
      <div className="sidebar-section-content">
        <div className="param">Background color</div>
        <div className="param">
          <input
            type="color"
            data-edit="canvasBackground"
            id="canvasBackgroundInput"
            defaultValue={color}
            onInput={(event) => {
              const val = event.currentTarget.value || '';
              try {
                state.canvasBackground = val;
                document.documentElement.style.setProperty('--canvas-background', val);
              } catch (e) {}
              try {
                signalDiagramChanged?.();
              } catch (e) {}
            }}
          />
        </div>
      </div>
    </div>
  );
}
//=============================================================================
function MultiSelectionInspectorPanel({ block }: { block: Block }): JSX.Element {
  return <>{`Selected ${block.blocks} blocks and ${block.connections} wires.`}</>;
}
//=============================================================================
function ConnectionInspectorPanel({
  fromLabel,
  toLabel,
  toIndex,
}: {
  fromLabel: string;
  toLabel: string;
  toIndex?: number;
}): JSX.Element {
  return (
    <>
      <div className="param">Wire</div>
      <div className="param">From: {fromLabel}</div>
      <div className="param">
        To: {toLabel} - input {typeof toIndex === 'number' ? toIndex + 1 : ''}
      </div>
    </>
  );
}
//=============================================================================
function renderCanvasPanel(
  inspectorBody: HTMLElement | null,
  signalDiagramChanged?: () => void
): void {
  const contextTitle = document.getElementById('inspectorContextTitle');
  if (contextTitle) contextTitle.textContent = 'Diagram';
  renderInspectorReact(
    inspectorBody,
    <CanvasInspectorPanel
      color={currentCanvasBackground()}
      signalDiagramChanged={signalDiagramChanged}
    />
  );
  try {
    const propsIcon = document.querySelector(
      '#panel-properties .sidebar-header .sidebar-properties-icon'
    ) as HTMLElement | null;
    if (propsIcon) {
      propsIcon.innerHTML = '';
      try {
        propsIcon.removeAttribute('data-type');
      } catch (_e) {}
    }
  } catch (e) {}
}
//=============================================================================
function renderMultiSelectionPanel(
  inspectorBody: HTMLElement | null,
  block: Block,
  rotateSelectionBtn?: HTMLButtonElement | null
): void {
  const contextTitle = document.getElementById('inspectorContextTitle');
  if (contextTitle) contextTitle.textContent = 'Selection';
  renderInspectorReact(inspectorBody, <MultiSelectionInspectorPanel block={block} />);
  if (rotateSelectionBtn) rotateSelectionBtn.disabled = true;
}
//=============================================================================
function renderConnectionPanel(
  inspectorBody: HTMLElement | null,
  block: Block,
  getRenderer: () => Renderer | null | undefined,
  rotateSelectionBtn?: HTMLButtonElement | null
): void {
  const contextTitle = document.getElementById('inspectorContextTitle');
  if (contextTitle) contextTitle.textContent = 'Wire';
  const r = getRenderer();
  const tryGetBlockLabel = (id: unknown, fallbackType: unknown) => {
    try {
      const b = (r as Renderer | undefined)?.state?.blocks?.get(id) || undefined;
      if (b && typeof b === 'object') {
        const bp = b as Record<string, unknown>;
        let candidate: unknown = fallbackType;
        if (bp.params && typeof bp.params === 'object') {
          const p = bp.params as Record<string, unknown>;
          candidate = p.name ?? candidate;
        }
        candidate = candidate ?? bp.label ?? bp.type ?? fallbackType;
        return `${String(candidate)} (${String(id)})`;
      }
    } catch (e) {
      // ignore
    }
    return `${fallbackType} (${id})`;
  };
  const fromLabel = tryGetBlockLabel(block.fromId, block.fromType);
  const toLabel = tryGetBlockLabel(block.toId, block.toType);
  renderInspectorReact(
    inspectorBody,
    <ConnectionInspectorPanel fromLabel={fromLabel} toLabel={toLabel} toIndex={block.toIndex} />
  );
  if (rotateSelectionBtn) rotateSelectionBtn.disabled = true;
}
//=============================================================================
export const createInspector = ({
  inspectorBody,
  rotateSelectionBtn,
  renderer,
  renderScope,
  signalDiagramChanged,
  onOpenSubsystem,
  onSaveSubsystem,
  onGenerateBlockCode,
  getRuntimeSeconds,
}: {
  inspectorBody: HTMLElement | null;
  rotateSelectionBtn?: HTMLButtonElement | null;
  renderer?: Renderer | null;
  renderScope?: (block: Block) => void;
  signalDiagramChanged?: () => void;
  onOpenSubsystem?: (block: Block) => void;
  onSaveSubsystem?: (block: Block) => void;
  onGenerateBlockCode?: (block: Block) => void;
  getRuntimeSeconds?: () => number;
}) => {
  const getRenderer = () => renderer?.current || renderer;
  // Blocks whose port count comes from a parameter (mux/demux/sum/...) must be
  // rebuilt when that count changes, so the ports follow the edit. The block is
  // recreated in place (same id/position/params) and the connections that still
  // address a valid port are restored.
  const rebuildPortsIfNeeded = (b: Block) => {
    const r = getRenderer() as any;
    if (!r?.createBlock || !r?.deleteBlock || !portCountsChanged(b)) return;
    const id = (b as any).id;
    const captured: SimpleConnection[] = ((state.connections as any[]) || [])
      .filter((c) => c.from === id || c.to === id)
      .map((c) => ({
        from: c.from,
        to: c.to,
        fromIndex: Number(c.fromIndex ?? 0),
        toIndex: Number(c.toIndex ?? 0),
      }));
    r.deleteBlock(id);
    r.createBlock((b as any).type, (b as any).x, (b as any).y, {
      id,
      rotation: (b as any).rotation ?? 0,
      params: (b as any).params,
    });
    const nb = (state.blocks as any).get(id);
    if (!nb) return;
    const counts = actualPortCounts(nb);
    for (const c of survivingConnections(captured, id, counts)) {
      r.createConnection?.(c.from, c.to, c.toIndex, c.fromIndex);
    }
    try {
      r.setMultiSelection?.(new Set([id]), new Set());
    } catch (_e) {}
    try {
      renderInspector(nb as Block);
    } catch (_e) {}
  };
  const callUpdate = (b: Block) => {
    const r = getRenderer() as Renderer | undefined;
    if (r?.updateBlockLabel) {
      r.updateBlockLabel(b);
    }
    rebuildPortsIfNeeded(b);
  };
  const callResize = (b: Block, w: number, h: number) => {
    const r = getRenderer() as Renderer | undefined;
    if (r?.resizeBlock) {
      r.resizeBlock(b, w, h);
    }
  };

  const renderInspector = (block: Block | null) => {
    if (!block) {
      renderCanvasPanel(inspectorBody, signalDiagramChanged);
      if (rotateSelectionBtn) rotateSelectionBtn.disabled = true;
      return;
    }

    if (!block.params) block.params = {};

    if (block.kind === 'multi') {
      renderMultiSelectionPanel(inspectorBody, block, rotateSelectionBtn);
      return;
    }

    if (block.kind === 'connection') {
      renderConnectionPanel(inspectorBody, block, getRenderer, rotateSelectionBtn);
      return;
    }

    unmountInspectorReact(inspectorBody);
    if (rotateSelectionBtn) rotateSelectionBtn.disabled = false;

    const deps: BlockPanelDeps = {
      inspectorBody,
      callUpdate,
      callResize,
      renderScope,
      signalDiagramChanged,
      getRuntimeSeconds,
      renderInspector,
      onOpenSubsystem,
      onSaveSubsystem,
      onGenerateBlockCode,
    };
    renderBlockPanel(block, deps);
  };

  return { renderInspector };
};
//=============================================================================
