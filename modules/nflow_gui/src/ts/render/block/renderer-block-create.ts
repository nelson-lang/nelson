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
import { alignBlockToGrid, blockPortAnchor } from '../geometry.js';
import { state } from '../../app/state.js';
import { createSvgElement } from '../svg-elements.js';
import {
  appendPhysicalPins,
  layoutPhysicalPinsWithFrames,
} from '../../libraries/acausal/physical-pins.js';
import { blockTemplates, refreshBlockTemplates } from './templates.js';
import { renderAcausalCaptionOverlay } from '../../libraries/generic-block-renderer.js';
import type { RendererCtx } from '../core/renderer-types.js';
import * as FieldNames from '../../FieldNames.js';
import {
  engineNativeBlockLabel,
  isEngineNativeBlockType,
} from '../../libraries/engine-native-blocks.js';
//=============================================================================
function engineNativeTemplate(type: string, options: any) {
  const inputCount = Math.max(0, Math.trunc(Number(options.inputs) || 0));
  const outputCount = Math.max(0, Math.trunc(Number(options.outputs) || 0));
  const portCount = Math.max(inputCount, outputCount, 1);
  const width = 180;
  // Ports sit on a 20 px pitch centred on the block, so every port of the
  // block lands on the grid together (block placement snaps by its anchor port).
  const height = Math.max(80, 20 * (portCount + 1));
  const ports = (count: number, side: 'left' | 'right') =>
    Array.from({ length: count }, (_, index) => {
      const y = height / 2 + (index - (count - 1) / 2) * 20;
      const x = side === 'left' ? 0 : width;
      return { x, y, side, wireX: x, wireY: y };
    });
  return {
    width,
    height,
    inputs: ports(inputCount, 'left'),
    outputs: ports(outputCount, 'right'),
    defaultParams: {},
    render: (block: any) => {
      block.group.appendChild(
        createSvgElement('rect', {
          x: 0,
          y: 0,
          width: block.width,
          height: block.height,
          class: 'nf-block',
        })
      );
      block.group.appendChild(
        createSvgElement(
          'text',
          {
            x: block.width / 2,
            y: block.height / 2,
            class: 'block-text upright',
            'text-anchor': 'middle',
            'dominant-baseline': 'middle',
          },
          engineNativeBlockLabel(type)
        )
      );
    },
  };
}
//=============================================================================
export function makeBlockCreation(ctx: RendererCtx) {
  function createBlock(type: string, x = 60, y = 60, options: any = {}) {
    if (state.locked) return null;
    const typeKey = String(type || '');
    const typeNorm = typeKey.toLowerCase();
    let template = (blockTemplates as any)[typeKey];
    let _foundBy: string | null = null;
    if (!template) {
      // try to rebuild templates (tests sometimes mutate loaded libraries)
      try {
        refreshBlockTemplates();
        template = (blockTemplates as any)[typeKey];
        if (template) _foundBy = 'module-after-refresh';
      } catch (_e) {
        // ignore
      }
    }
    if (!template) {
      // fallback: try to find a namespaced template key that matches the type
      try {
        const keys = Object.keys((blockTemplates as any) || {});
        for (const k of keys) {
          try {
            const kNorm = String(k || '').toLowerCase();
            if (k === typeKey) continue;
            if (kNorm === typeNorm || kNorm.endsWith('.' + typeNorm) || k.endsWith('.' + typeKey)) {
              template = (blockTemplates as any)[k];
              _foundBy = `module-ns:${k}`;
              break;
            }
          } catch (_e) {}
        }
      } catch (_e) {
        // ignore and fall through
      }
    }
    if (!template) {
      // Lowered mechanisms are intentionally absent from the palette, but a
      // saved diagram or subsystem view still needs to render their ports.
      if (isEngineNativeBlockType(typeKey)) {
        template = engineNativeTemplate(typeKey, options);
        _foundBy = 'engine-native';
      }
    }
    if (!template) {
      // Host-injected windows may update `window.blockTemplates` without the
      // module-level `blockTemplates` being in sync: try the window value, then
      // request a module refresh.
      try {
        const wb = (window as any).blockTemplates || {};
        const keys = Object.keys(wb || {});
        for (const k of keys) {
          try {
            const kNorm = String(k || '').toLowerCase();
            if (
              k === typeKey ||
              kNorm === typeNorm ||
              kNorm.endsWith('.' + typeNorm) ||
              k.endsWith('.' + typeKey)
            ) {
              template = wb[k];
              break;
            }
          } catch (_e) {}
        }
        if (template) {
          _foundBy = _foundBy || 'window-ns';
          try {
            refreshBlockTemplates();
          } catch (_e) {}
        }
      } catch (_e) {
        // ignore
      }
    }
    if (!template) {
      return null;
    }
    const id = options.id || `b${(ctx.state as any).nextId++}`;
    if (options.id) {
      const match = /^b(\d+)$/.exec(options.id);
      if (match) {
        const nextId = Number(match[1]) + 1;
        if (Number.isFinite(nextId))
          (ctx.state as any).nextId = Math.max((ctx.state as any).nextId, nextId);
      }
    }
    const params = { ...(template.defaultParams || {}), ...(options.params || {}) };
    Object.keys(params).forEach((key) => {
      if (Array.isArray(params[key])) params[key] = [...params[key]];
    });
    if (!params._visible || typeof params._visible !== 'object') params._visible = {};
    let blockWidth = template.width;
    let blockHeight = template.height;
    let dynamicInputs: any = null;
    let dynamicOutputs: any = null;
    if (
      type === 'scope' ||
      type === 'xyScope' ||
      type === 'comment' ||
      type === FieldNames.kSubsystem
    ) {
      const paramWidth = Number(params.width);
      const paramHeight = Number(params.height);
      if (Number.isFinite(paramWidth)) blockWidth = paramWidth;
      if (Number.isFinite(paramHeight)) blockHeight = paramHeight;
    }
    if (typeof template.resize === 'function') {
      const probe: any = {
        id,
        type,
        width: blockWidth,
        height: blockHeight,
        params,
        inputs: template.inputs.length,
        outputs: template.outputs.length,
      };
      template.resize(probe);
      blockWidth = probe.width;
      blockHeight = probe.height;
      dynamicInputs = Array.isArray(probe.dynamicInputs)
        ? probe.dynamicInputs.map((p: any) => ({ ...p }))
        : null;
      dynamicOutputs = Array.isArray(probe.dynamicOutputs)
        ? probe.dynamicOutputs.map((p: any) => ({ ...p }))
        : null;
    }
    // If no explicit serialized `ports` were provided, but this block is a
    // `subsystem` with `externalInputs`/`externalOutputs`, synthesize port
    // positions so the host block displays the same ports as the subsystem.
    try {
      const subs: any = (params && (params as any).subsystem) || null;
      if (!Array.isArray((options as any).ports) && subs && typeof subs === 'object') {
        const extIns = Array.isArray(subs.externalInputs) ? subs.externalInputs : [];
        const extOuts = Array.isArray(subs.externalOutputs) ? subs.externalOutputs : [];
        if (Array.isArray(subs.externalInputs) || Array.isArray(subs.externalOutputs)) {
          // Same 20 px pitch as every other block while it fits; a subsystem
          // with more ports than that spreads them evenly instead.
          const makeY = (i: number, n: number) =>
            20 * (n + 1) <= blockHeight
              ? blockHeight / 2 + (i - (n - 1) / 2) * 20
              : Math.round(((i + 1) * blockHeight) / (n + 1));
          dynamicInputs = extIns.map((_: any, idx: number) => ({
            x: 0,
            y: makeY(idx, extIns.length),
            side: 'left',
            wireX: 0,
            wireY: makeY(idx, extIns.length),
          }));
          dynamicOutputs = extOuts.map((_: any, idx: number) => ({
            x: blockWidth,
            y: makeY(idx, extOuts.length),
            side: 'right',
            wireX: blockWidth,
            wireY: makeY(idx, extOuts.length),
          }));
        }
      }
    } catch (_e) {
      // ignore synthesis failures and fall back to template defaults
    }
    // Serialized `ports` from a saved snapshot become dynamicInputs/dynamicOutputs so
    // the renderer creates the same visible ports as the UI.
    if (Array.isArray(options.ports)) {
      try {
        const portsArr = options.ports as any[];
        const inSpecs = portsArr
          .filter((p) => String(p.type) === 'in')
          .sort((a, b) => Number(a.index ?? 0) - Number(b.index ?? 0))
          .map((p) => ({
            x: Number(p.x) || 0,
            y: Number(p.y) || 0,
            side: p.side || 'left',
            wireX: p.wireX ?? p.x,
            wireY: p.wireY ?? p.y,
          }));
        const outSpecs = portsArr
          .filter((p) => String(p.type) === 'out')
          .sort((a, b) => Number(a.index ?? 0) - Number(b.index ?? 0))
          .map((p) => ({
            x: Number(p.x) || 0,
            y: Number(p.y) || 0,
            side: p.side || 'right',
            wireX: p.wireX ?? p.x,
            wireY: p.wireY ?? p.y,
          }));
        if (inSpecs.length) dynamicInputs = inSpecs;
        if (outSpecs.length) dynamicOutputs = outSpecs;
      } catch (_e) {
        // ignore malformed ports
      }
    }
    const inputSpecs = dynamicInputs || template.inputs;
    const outputSpecs = dynamicOutputs || template.outputs;
    const group = createSvgElement('g', { class: `svg-block type-${type}`, 'data-block-id': id });
    const anchorProbe = {
      x,
      y,
      width: blockWidth,
      height: blockHeight,
      rotation: options.rotation ?? 0,
      ports: [
        ...inputSpecs.map((p: any) => ({ x: p.wireX ?? p.x, y: p.wireY ?? p.y, type: 'in' })),
        ...outputSpecs.map((p: any) => ({ x: p.wireX ?? p.x, y: p.wireY ?? p.y, type: 'out' })),
      ],
    };
    const _snappedStart = alignBlockToGrid({ x, y }, blockPortAnchor(anchorProbe));
    const block: any = {
      id,
      type,
      x: _snappedStart.x,
      y: _snappedStart.y,
      width: blockWidth,
      height: blockHeight,
      rotation: options.rotation ?? 0,
      inputs: inputSpecs.length,
      outputs: outputSpecs.length,
      params,
      group,
      ports: [],
      paramLabels: {},
      dynamicInputs: dynamicInputs || undefined,
      dynamicOutputs: dynamicOutputs || undefined,
    };
    try {
      template.render(block);
    } catch (e) {
      throw e;
    }
    if (type === 'expression' || type === 'tf' || type === 'dtf')
      (ctx as any).resizeUserFuncFromLabel?.(block, { force: true });
    const dragHeight = type === 'scope' ? 24 : block.height;
    const minDragSize = 80;
    const dragWidth = type === 'scope' ? block.width : Math.max(block.width, minDragSize);
    const dragBoxHeight = type === 'scope' ? dragHeight : Math.max(dragHeight, minDragSize);
    const dragX = type === 'scope' ? 0 : (block.width - dragWidth) / 2;
    const dragY = type === 'scope' ? 0 : (block.height - dragBoxHeight) / 2;
    const dragRect = createSvgElement('rect', {
      x: dragX,
      y: dragY,
      width: dragWidth,
      height: dragBoxHeight,
      class: 'drag-handle',
    });
    if (type.startsWith('dashboard')) group.insertBefore(dragRect, group.firstChild);
    else group.appendChild(dragRect);
    block.dragRect = dragRect;
    inputSpecs.forEach((port: any, index: number) => {
      const circle = (ctx as any).createPortCircle(id, 'in', index, port, type);
      group.appendChild(circle);
      block.ports.push({
        ...port,
        type: 'in',
        index,
        el: circle,
        wireX: port.wireX ?? port.x,
        wireY: port.wireY ?? port.y,
      });
    });
    outputSpecs.forEach((port: any, index: number) => {
      const circle = (ctx as any).createPortCircle(id, 'out', index, port, type);
      group.appendChild(circle);
      block.ports.push({
        ...port,
        type: 'out',
        index,
        el: circle,
        wireX: port.wireX ?? port.x,
        wireY: port.wireY ?? port.y,
      });
    });
    // Acausal blocks: render undirected physical pins and register them as
    // connectable 'phys' ports. No-op for causal blocks (no physicalPins).
    const manifestPins = (template as any).physicalPins ?? (options as any).physicalPins;
    let physPins = Array.isArray(manifestPins) ? manifestPins.slice() : [];
    // Per-instance named frames (PlanarBody.params.frames) become extra physical
    // pins, appended after the manifest pins so a wire to "<id>.<frame>" (e.g.
    // "r0.prox") round-trips through save/Run. block.physicalPins is the single
    // source the import (name->index) and export (index->name) both read, so
    // adding them here fixes the whole round-trip. No-op when there are no frames.
    const dom = (template as any).domain ?? (options as any).domain;
    if (params.frames && typeof params.frames === 'object' && !Array.isArray(params.frames)) {
      physPins = layoutPhysicalPinsWithFrames(
        physPins,
        params.frames as Record<string, unknown>,
        block.width,
        block.height,
        typeof dom === 'string' ? dom : 'planar'
      );
    }
    if (physPins.length > 0) {
      appendPhysicalPins(group as any, block as any, physPins, createSvgElement as any);
      // Tag the instance as acausal so the save/Run path lifts its physical-pin
      // connections into `physicalWires` (blockPinInfoFromManifest reads these).
      block.acausal = true;
      block.physicalPins = physPins;
      if (typeof dom === 'string') block.domain = dom;
      // Render the name/value caption now that the instance is tagged acausal
      // (the icon render above ran before this flag was set, so its inline
      // finishOverlays could not draw it yet).
      renderAcausalCaptionOverlay(
        block as any,
        group as any,
        (tag: string, attrs?: Record<string, unknown>, content?: string | null) =>
          createSvgElement(
            tag,
            (attrs as Record<string, string | number>) ?? {},
            content ?? ''
          ) as any,
        block.width,
        block.height
      );
    }
    if (
      type === 'scope' ||
      type === 'xyScope' ||
      type === 'comment' ||
      type === 'display' ||
      type.startsWith('dashboard') ||
      type === FieldNames.kToggleSwitch
    ) {
      const resizeHandle = createSvgElement('rect', {
        class: 'resize-handle',
        x: block.width - 16,
        y: block.height - 16,
        width: 12,
        height: 12,
      });
      group.appendChild(resizeHandle);
      block.resizeHandle = resizeHandle;
      (ctx as any).enableResize?.(block, resizeHandle);
    }
    try {
      // debug: omitted collection of ctx keys to reduce runtime overhead
    } catch (_e) {}

    (ctx as any).blockLayer.appendChild(group);
    (ctx.state as any).blocks.set(id, block);
    if (type === 'scope' || type === 'xyScope') (ctx as any).updateScopeLayout?.(block);
    else if (type === 'comment') (ctx as any).updateCommentLayout?.(block);
    else if (type === 'display') (ctx as any).updateDisplayLayout?.(block);
    else if (type === FieldNames.kToggleSwitch) (ctx as any).updateToggleLayout?.(block);
    if (type === 'constant' || type === FieldNames.kGain) (ctx as any).updateBlockLabel?.(block);
    (ctx as any).updateBlockTransform?.(block);
    (ctx as any).enableDrag?.(block, dragRect);
    (ctx as any).enableSelection?.(block, dragRect);
    (ctx as any).enableSubsystemOpen?.(block, dragRect);
    (ctx as any).enableScopeHover?.(block);
    (ctx.state as any).routingDirty = true;
    (ctx as any).updateConnections?.();

    // Comment blocks: double-click edits in place and writes block.params.CommentText.
    if (typeof document !== 'undefined' && type === 'comment' && (block as any).commentTextEl) {
      try {
        const el = (block as any).commentTextEl as HTMLElement;
        // Prevent pointer events from bubbling to block drag/selection.
        el.addEventListener('pointerdown', (ev) => ev.stopPropagation());

        el.addEventListener('dblclick', (ev) => {
          ev.stopPropagation();
          try {
            el.contentEditable = 'true';
            const range = document.createRange();
            range.selectNodeContents(el);
            const sel = window.getSelection();
            if (sel) {
              sel.removeAllRanges();
              sel.addRange(range);
            }
            el.focus();
          } catch (_e) {}
        });

        el.addEventListener('input', () => {
          try {
            (block as any).params = (block as any).params || {};
            (block as any).params[FieldNames.kCommentText] = String(el.textContent || '');
            (ctx as any).updateCommentLayout?.(block);
          } catch (_e) {}
        });

        el.addEventListener('blur', () => {
          try {
            el.contentEditable = 'false';
            (block as any).params = (block as any).params || {};
            (block as any).params[FieldNames.kCommentText] = String(el.textContent || '');
            (ctx as any).updateBlockLabel?.(block);
          } catch (_e) {}
        });

        el.addEventListener('keydown', (e: KeyboardEvent) => {
          try {
            if (e.key === 'Enter' && !e.shiftKey) {
              e.preventDefault();
              (el as HTMLElement).blur();
            }
            if (e.key === 'Escape') {
              e.preventDefault();
              el.contentEditable = 'false';
              const current = String((block as any).params?.[FieldNames.kCommentText] || '');
              el.textContent = current;
              el.blur();
            }
          } catch (_e) {}
        });
      } catch (_e) {
        // ignore attach failures
      }
    }

    if (typeof window !== 'undefined') {
      window.dispatchEvent(new CustomEvent('diagramChanged'));
      window.dispatchEvent(new CustomEvent('updateStatusBar'));
    }
    return block;
  }

  return { createBlock };
}
//=============================================================================
