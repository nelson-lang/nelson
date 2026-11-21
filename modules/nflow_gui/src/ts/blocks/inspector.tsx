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
import { state, signalDiagramChanged as markDiagramChanged } from '../app/state.js';
import { setSignalName } from '../app/wire-labels.js';
import {
  portCountsChanged,
  desiredPortCounts,
  actualPortCounts,
  survivingConnections,
  remapSelectorConnections,
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
      <label className="param">
        Signal name
        <input
          data-edit="signalName"
          defaultValue={String((state.selectedConnection as any)?.label ?? '')}
          onChange={(event) => {
            const conn = state.selectedConnection as any;
            if (!conn || typeof conn !== 'object') return;
            const label = event.currentTarget.value;
            setSignalName(conn, label);
            markDiagramChanged();
          }}
        />
      </label>
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
    <ConnectionInspectorPanel
      key={`${String(block.fromId)}:${String(block.toId)}:${String(block.toIndex)}`}
      fromLabel={fromLabel}
      toLabel={toLabel}
      toIndex={block.toIndex}
    />
  );
  if (rotateSelectionBtn) rotateSelectionBtn.disabled = true;
}
//=============================================================================
function captureInspectorFocus(body: HTMLElement | null): () => void {
  const active = document.activeElement;
  const field = active instanceof HTMLElement && body?.contains(active) ? active : null;
  const key = field?.getAttribute('data-edit');
  const editable =
    field instanceof HTMLInputElement || field instanceof HTMLTextAreaElement ? field : null;
  const start = editable?.selectionStart;
  const end = editable?.selectionEnd;
  const direction = editable?.selectionDirection;
  return () => {
    if (!key || !body) return;
    const replacement = Array.from(body.querySelectorAll<HTMLElement>('[data-edit]')).find(
      (element) => element.getAttribute('data-edit') === key
    );
    replacement?.focus({ preventScroll: true });
    if (
      (replacement instanceof HTMLInputElement || replacement instanceof HTMLTextAreaElement) &&
      start != null &&
      end != null
    )
      replacement.setSelectionRange(start, end, direction ?? undefined);
  };
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
  const acceptedParams = new WeakMap<Block, Record<string, unknown>>();
  const saveParams = (block: Block) => {
    acceptedParams.set(block, JSON.parse(JSON.stringify(block.params || {})));
  };
  // Blocks whose port count comes from a parameter (mux/demux/sum/...) must be
  // rebuilt when that count changes, so the ports follow the edit. The block is
  // recreated in place (same id/position/params) and the connections that still
  // address a valid port are restored.
  const rebuildPortsIfNeeded = (b: Block): boolean => {
    const r = getRenderer() as any;
    const previous = acceptedParams.get(b);
    const oldPaths = Array.isArray(previous?.SelectedSignals)
      ? previous.SelectedSignals.map(String)
      : [];
    const newPaths = Array.isArray(b.params?.SelectedSignals)
      ? b.params.SelectedSignals.map(String)
      : [];
    const wasBus = previous?.OutputAsBus === true;
    const isBus = b.params?.OutputAsBus === true;
    const selectorChanged =
      b.type === 'busSelector' &&
      previous &&
      (wasBus !== isBus || JSON.stringify(oldPaths) !== JSON.stringify(newPaths));
    if (!r?.createBlock || !r?.deleteBlock || (!portCountsChanged(b) && !selectorChanged))
      return false;
    const restoreFocus = captureInspectorFocus(inspectorBody);
    const id = (b as any).id;
    let captured: SimpleConnection[] = ((state.connections as any[]) || [])
      .filter((c) => c.from === id || c.to === id)
      .map((c) => ({
        from: c.from,
        to: c.to,
        fromIndex: Number(c.fromIndex ?? 0),
        toIndex: Number(c.toIndex ?? 0),
        label: c.label,
        labelOffset: c.labelOffset ? { ...c.labelOffset } : undefined,
        points: Array.isArray(c.points) ? c.points.map((point: any) => ({ ...point })) : [],
      }));
    if (selectorChanged) {
      captured = remapSelectorConnections(captured, id, oldPaths, newPaths, wasBus, isBus);
    }
    const wanted = desiredPortCounts(b);
    const removed = wanted
      ? captured.length - survivingConnections(captured, id, wanted).length
      : 0;
    if (removed > 0 && !window.confirm('This change removes connected ports. Continue?')) {
      const previous = acceptedParams.get(b);
      if (previous) b.params = previous;
      r.updateBlockLabel?.(b);
      renderInspector(b);
      restoreFocus();
      return true;
    }
    r.deleteBlock(id);
    r.createBlock((b as any).type, (b as any).x, (b as any).y, {
      id,
      rotation: (b as any).rotation ?? 0,
      params: (b as any).params,
    });
    const nb = (state.blocks as any).get(id);
    if (!nb) return true;
    const counts = actualPortCounts(nb);
    for (const c of survivingConnections(captured, id, counts)) {
      const restored = r.createConnection?.(c.from, c.to, c.toIndex, c.fromIndex);
      if (restored) {
        if (typeof c.label === 'string') restored.label = c.label;
        if (c.labelOffset) restored.labelOffset = { ...c.labelOffset };
        if (c.points?.length) restored.points = c.points.map((point) => ({ ...point }));
      }
    }
    r.updateConnections?.(true);
    if (typeof window !== 'undefined') window.dispatchEvent(new CustomEvent('diagramChanged'));
    try {
      r.setMultiSelection?.(new Set([id]), new Set());
    } catch (_e) {}
    try {
      renderInspector(nb as Block);
      restoreFocus();
    } catch (_e) {}
    return true;
  };
  const callUpdate = (b: Block) => {
    const current = state.blocks.get(b.id as number | string);
    if (current && current !== b) return;
    if (rebuildPortsIfNeeded(b)) return;
    const r = getRenderer() as Renderer | undefined;
    if (r?.updateBlockLabel) {
      r.updateBlockLabel(b);
    }
    if ((state.blocks as any).get((b as any).id) === b) saveParams(b);
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
    saveParams(block);

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
