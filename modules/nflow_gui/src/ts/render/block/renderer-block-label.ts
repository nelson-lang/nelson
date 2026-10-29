//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { blockTemplates } from './templates.js';
import { applyBlockTypeLabels } from './renderer-block-label-types.js';
import type { RendererCtx } from '../core/renderer-types.js';
import { dashboardRenderer } from '../../libraries/dashboard/dashboard.js';
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
export function makeBlockLabelHandlers(ctx: RendererCtx) {
  function operatorLabel(value: unknown, fallback: string): string {
    const op = String(value ?? fallback);
    switch (op) {
      case 'ge':
        return '>=';
      case 'gt':
        return '>';
      case 'le':
        return '<=';
      case 'lt':
        return '<';
      case 'eq':
        return '==';
      case 'ne':
        return '!=';
      default:
        return fallback;
    }
  }

  function updateLogicOperatorDisplay(block: any) {
    const fallback =
      block?.type === 'compareToZero'
        ? '!='
        : block?.type === 'compareToConstant' || block?.type === 'relationalOperator'
        ? '>='
        : '';
    if (!fallback) return;

    const op = operatorLabel(block?.params?.Operator ?? block?.params?.relop, fallback);
    const expression =
      block?.type === 'compareToZero'
        ? `u ${op} 0`
        : block?.type === 'relationalOperator'
        ? `u1 ${op} u2`
        : `u ${op} c`;

    const expressionEl = block?.group?.querySelector?.('text.logic-expression-display') as
      | SVGTextElement
      | undefined;
    if (expressionEl) expressionEl.textContent = expression;

    const legacyOperatorEl = block?.group?.querySelector?.('text.logic-operator-display') as
      | SVGTextElement
      | undefined;
    if (legacyOperatorEl) legacyOperatorEl.textContent = op;
  }

  function handleBlockLabelResize(block: any) {
    const template = (blockTemplates as any)[(block as any).type];
    if (!template?.resize) return;
    const previous = { width: (block as any).width, height: (block as any).height };
    template.resize(block);
    const sizeChanged =
      previous.width !== (block as any).width || previous.height !== (block as any).height;
    const desiredInputSpecs = Array.isArray((block as any).dynamicInputs)
      ? (block as any).dynamicInputs
      : template.inputs;
    const desiredOutputSpecs = Array.isArray((block as any).dynamicOutputs)
      ? (block as any).dynamicOutputs
      : template.outputs;
    const currentInputs = (block as any).ports
      .filter((port: any) => port.type === 'in')
      .sort((a: any, b: any) => a.index - b.index);
    const currentOutputs = (block as any).ports
      .filter((port: any) => port.type === 'out')
      .sort((a: any, b: any) => a.index - b.index);
    const inputsChanged =
      currentInputs.length !== desiredInputSpecs.length ||
      currentInputs.some((port: any, index: number) => {
        const spec = desiredInputSpecs[index];
        return !spec || port.x !== spec.x || port.y !== spec.y || port.side !== spec.side;
      });
    const outputsChanged =
      currentOutputs.length !== desiredOutputSpecs.length ||
      currentOutputs.some((port: any, index: number) => {
        const spec = desiredOutputSpecs[index];
        return !spec || port.x !== spec.x || port.y !== spec.y || port.side !== spec.side;
      });
    const needsPortRebuild = inputsChanged || outputsChanged;

    if (sizeChanged) {
      const rect = (block as any).group.querySelector('rect.nf-block');
      if (rect) {
        rect.setAttribute('width', String((block as any).width));
        rect.setAttribute('height', String((block as any).height));
      }
      if ((block as any).dragRect) {
        const minDragSize = 80;
        const dragHeight = (block as any).type === 'scope' ? 24 : (block as any).height;
        const dragWidth =
          (block as any).type === 'scope'
            ? (block as any).width
            : Math.max((block as any).width, minDragSize);
        const dragBoxHeight =
          (block as any).type === 'scope' ? dragHeight : Math.max(dragHeight, minDragSize);
        (block as any).dragRect.setAttribute(
          'x',
          (block as any).type === 'scope' ? '0' : String(((block as any).width - dragWidth) / 2)
        );
        (block as any).dragRect.setAttribute(
          'y',
          (block as any).type === 'scope'
            ? '0'
            : String(((block as any).height - dragBoxHeight) / 2)
        );
        (block as any).dragRect.setAttribute('width', String(dragWidth));
        (block as any).dragRect.setAttribute('height', String(dragBoxHeight));
      }
      (ctx as any).updateBlockTransform?.(block);
    }

    if (needsPortRebuild) {
      (block as any).ports.forEach((port: any) => {
        if (port?.el?.parentNode) port.el.parentNode.removeChild(port.el);
      });
      (block as any).ports = [];
      desiredInputSpecs.forEach((spec: any, index: number) => {
        const circle = (ctx as any).createPortCircle(
          (block as any).id,
          'in',
          index,
          spec,
          (block as any).type
        );
        (block as any).group.appendChild(circle);
        (block as any).ports.push({
          ...spec,
          type: 'in',
          index,
          el: circle,
          wireX: spec.wireX ?? spec.x,
          wireY: spec.wireY ?? spec.y,
        });
      });
      desiredOutputSpecs.forEach((spec: any, index: number) => {
        const circle = (ctx as any).createPortCircle(
          (block as any).id,
          'out',
          index,
          spec,
          (block as any).type
        );
        (block as any).group.appendChild(circle);
        (block as any).ports.push({
          ...spec,
          type: 'out',
          index,
          el: circle,
          wireX: spec.wireX ?? spec.x,
          wireY: spec.wireY ?? spec.y,
        });
      });
    } else if (sizeChanged) {
      currentInputs.forEach((port: any, index: number) => {
        const spec = desiredInputSpecs[index];
        if (!spec) return;
        port.x = spec.x;
        port.y = spec.y;
        port.side = spec.side;
        port.wireX = spec.wireX ?? spec.x;
        port.wireY = spec.wireY ?? spec.y;
        (ctx as any).updatePortElement?.(port);
      });
      currentOutputs.forEach((port: any, index: number) => {
        const spec = desiredOutputSpecs[index];
        if (!spec) return;
        port.x = spec.x;
        port.y = spec.y;
        port.side = spec.side;
        port.wireX = spec.wireX ?? spec.x;
        port.wireY = spec.wireY ?? spec.y;
        (ctx as any).updatePortElement?.(port);
      });
    }

    if (sizeChanged || needsPortRebuild) {
      (ctx.state as any).routingDirty = true;
      (ctx as any).updateConnections?.(true);
    }
  }

  function updateBlockLabel(block: any) {
    handleBlockLabelResize(block);
    applyBlockTypeLabels(block, ctx as any);
    updateLogicOperatorDisplay(block);
    (ctx as any).updateParamDisplay?.(block);
    // Dashboard widgets bake their scale/colors/format/caption at render time;
    // re-render the whole widget so inspector edits take effect immediately.
    if (block?.group && String(block.type ?? '').startsWith('dashboard')) {
      try {
        dashboardRenderer(block, undefined as never);
      } catch (_e) {}
    }
  }

  return { updateBlockLabel };
}
//=============================================================================
