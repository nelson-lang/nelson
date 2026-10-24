//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { blockBounds } from '../geometry.js';
import { resizeSvgMathText } from '../svg-elements.js';
import { positionAcausalCaption } from '../../libraries/generic-block-renderer.js';
import type { RendererCtx, Block } from '../core/renderer-types.js';
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
const USERFUNC_SETTLE_RETRIES = 4;
const USERFUNC_SETTLE_DELAY_MS = 60;
export function makeBlockTransformHandlers(ctx: RendererCtx) {
  function updateBlockTransform(block: any | Block) {
    const angle = (block as any).rotation || 0;
    const cx = (block as any).width / 2;
    const cy = (block as any).height / 2;
    const scaleX = (block as any).mirrored ? -1 : 1;
    (block as any).group.setAttribute(
      'transform',
      `translate(${(block as any).x}, ${
        (block as any).y
      }) translate(${cx}, ${cy}) scale(${scaleX}, 1) translate(${-cx}, ${-cy}) rotate(${angle} ${cx} ${cy})`
    );
    (block as any).group.setAttribute('data-rotation', String(angle));
    (block as any).group.setAttribute('data-mirrored', String((block as any).mirrored || false));
    // Keep an acausal name/value caption upright and correctly placed (below a
    // horizontal body, beside a vertical one) when the block is rotated: the
    // whole group rotates, so the caption carries a counter-rotation.
    const cap = (block as any).group.querySelector?.('g.acausal-caption');
    if (cap) positionAcausalCaption(block as any, cap, (block as any).width, (block as any).height);
  }

  function resizeBlock(block: any | Block, width: number, height: number) {
    if (
      !block ||
      ((block as any).type !== 'scope' &&
        (block as any).type !== 'xyScope' &&
        (block as any).type !== 'comment')
    )
      return;
    (block as any).width = width;
    (block as any).height = height;
    if ((block as any).type === 'comment') (ctx as any).updateCommentLayout?.(block);
    else (ctx as any).updateScopeLayout?.(block);
    (ctx.state as any).routingDirty = true;
    (ctx.state as any).dirtyBlocks?.add((block as any).id);
    (ctx as any).updateConnections?.(true);
  }

  function resizeUserFuncFromLabel(block: any | Block, { force = false } = {}) {
    const config = (ctx as any).getAutoMathConfig?.(block);
    if (!config) return;
    const mathGroup = (block as any).group.querySelector(config.className);
    if (!mathGroup) return;
    if ((block as any).type === 'expression') {
      const { width, height } = (ctx as any).computeUserFuncSize?.(block, { force, mathGroup }) || {
        width: 0,
        height: 0,
      };
      applyUserFuncSize(block, width, height, { force });
    } else {
      const rendered = (ctx as any).computeRenderedAutoMathSize?.(block, mathGroup);
      const width = rendered ? rendered.width : Math.max((block as any).width, config.minWidth);
      applyUserFuncSize(block, width, config.fixedHeight, { force });
    }
    scheduleUserFuncFit(block);
  }

  function applyUserFuncSize(
    block: any | Block,
    width: number,
    height: number,
    { force = false } = {}
  ) {
    if (!block) return;
    if (!force && width === (block as any).width && height === (block as any).height) return;
    (block as any).width = width;
    (block as any).height = height;
    const body = (block as any).group.querySelector('rect.nf-block');
    if (body) {
      body.setAttribute('width', String(width));
      body.setAttribute('height', String(height));
    }
    const config = (ctx as any).getAutoMathConfig?.(block);
    const mathGroup = config ? (block as any).group.querySelector(config.className) : null;
    if (mathGroup) {
      const foreign = mathGroup.querySelector('foreignObject');
      if (foreign) {
        foreign.setAttribute('width', String(width));
        foreign.setAttribute('height', String(height));
      }
      resizeSvgMathText(mathGroup, width, height);
    }
    (block as any).dynamicInputs = [{ x: 0, y: height / 2, side: 'left' }];
    (block as any).dynamicOutputs = [{ x: width, y: height / 2, side: 'right' }];
    if ((block as any).dragRect) {
      const minDragSize = 80;
      const dragWidth = Math.max(width, minDragSize);
      const dragBoxHeight = Math.max(height, minDragSize);
      (block as any).dragRect.setAttribute('x', String((width - dragWidth) / 2));
      (block as any).dragRect.setAttribute('y', String((height - dragBoxHeight) / 2));
      (block as any).dragRect.setAttribute('width', String(dragWidth));
      (block as any).dragRect.setAttribute('height', String(dragBoxHeight));
    }
    (block as any).ports
      .filter((p: any) => p.type === 'in')
      .sort((a: any, b: any) => a.index - b.index)
      .forEach((port: any, index: number) => {
        const spec = (block as any).dynamicInputs[index];
        if (spec) {
          port.x = spec.x;
          port.y = spec.y;
          port.side = spec.side;
          port.wireX = spec.x;
          port.wireY = spec.y;
          (ctx as any).updatePortElement?.(port);
        }
      });
    (block as any).ports
      .filter((p: any) => p.type === 'out')
      .sort((a: any, b: any) => a.index - b.index)
      .forEach((port: any, index: number) => {
        const spec = (block as any).dynamicOutputs[index];
        if (spec) {
          port.x = spec.x;
          port.y = spec.y;
          port.side = spec.side;
          port.wireX = spec.x;
          port.wireY = spec.y;
          (ctx as any).updatePortElement?.(port);
        }
      });
    updateBlockTransform(block);
    (ctx.state as any).routingDirty = true;
    (ctx as any).updateConnections?.(true);
  }

  function scheduleUserFuncFit(block: any | Block) {
    const config = (ctx as any).getAutoMathConfig?.(block);
    if (!config) return;
    const id = (block as any).id;
    const attempts = (ctx as any).userFuncResizeAttempts.get(id) || 0;
    if (attempts >= USERFUNC_SETTLE_RETRIES) {
      (ctx as any).userFuncResizeAttempts.delete(id);
      return;
    }
    (ctx as any).userFuncResizeAttempts.set(id, attempts + 1);
    setTimeout(() => {
      const mathGroup = (block as any).group?.querySelector?.(config.className);
      const renderedSize = (ctx as any).computeRenderedAutoMathSize?.(block, mathGroup);
      if (!renderedSize) {
        scheduleUserFuncFit(block);
        return;
      }
      const targetWidth = Math.max(config.minWidth, renderedSize.width);
      if (targetWidth !== (block as any).width) {
        applyUserFuncSize(block, targetWidth, config.fixedHeight, { force: true });
        scheduleUserFuncFit(block);
      } else {
        (ctx as any).userFuncResizeAttempts.delete(id);
      }
    }, USERFUNC_SETTLE_DELAY_MS);
  }

  function rectsOverlap(a: any, b: any) {
    return a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top;
  }

  function detectBlockOverlap(ids: string[]) {
    if (!ids || ids.length === 0) return false;
    const movedIds = new Set(ids);
    const movedBlocks = ids.map((id) => (ctx.state as any).blocks.get(id)).filter(Boolean);
    if (movedBlocks.length === 0) return false;
    const movedRects = movedBlocks.map((block: any) => ({
      id: block.id,
      rect: blockBounds(block),
    }));
    const otherRects: Array<any> = [];
    (ctx.state as any).blocks.forEach((block: any) => {
      if (!movedIds.has(block.id)) otherRects.push({ id: block.id, rect: blockBounds(block) });
    });
    for (let i = 0; i < movedRects.length; i += 1) {
      for (let j = 0; j < otherRects.length; j += 1) {
        if (rectsOverlap(movedRects[i].rect, otherRects[j].rect)) return true;
      }
    }
    return false;
  }

  if (typeof window !== 'undefined') {
    (window as any).nflowResizeMathBlock = (blockId: string) => {
      const block = (ctx.state as any).blocks.get(blockId);
      if (block && (block.type === 'expression' || block.type === 'tf' || block.type === 'dtf'))
        resizeUserFuncFromLabel(block, { force: true });
    };
    (window as any).nflowResizeUserFunc = (blockId: string) => {
      const block = (ctx.state as any).blocks.get(blockId);
      if (block && (block.type === 'expression' || block.type === 'tf' || block.type === 'dtf'))
        resizeUserFuncFromLabel(block, { force: true });
    };
    (window as any).nflowUpdateParamDisplay = (blockId: string) => {
      const block = (ctx.state as any).blocks.get(blockId);
      if (!block) return;
      (ctx as any).updateParamDisplay?.(block);
    };
  }

  return {
    updateBlockTransform,
    resizeBlock,
    resizeUserFuncFromLabel,
    applyUserFuncSize,
    scheduleUserFuncFit,
    rectsOverlap,
    detectBlockOverlap,
  };
}
//=============================================================================
