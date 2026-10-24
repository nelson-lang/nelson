//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { snapToGrid } from '../geometry.js';
import { renderScope } from '../../sim/render-scope.js';
import type { Block } from './renderer-types.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
type BlockLike = Block & {
  params?: { width?: number; height?: number; CommentText?: string; ShowBorder?: boolean };
  bodyRect?: Element | null;
  scopePlot?: Element | null;
  scopeClipRect?: Element | null;
  dragRect?: Element | null;
  resizeHandle?: Element | null;
  scopeInputHints?: Element[];
  commentBody?: Element | null;
  commentForeign?: Element | null;
  commentTextEl?: Element | null;
  commentTextFallback?: Element | null;
  scopeData?: unknown;
  displayTitleEl?: Element | null;
  displayValueEl?: Element | null;
  scopeBlockTitle?: Element | null;
};
//=============================================================================
type PortLike = {
  el?: Element | null;
  x?: number;
  y?: number;
  type?: string;
  index?: number;
  side?: string;
  wireX?: number;
  wireY?: number;
};
//=============================================================================
const SCOPE_MIN_W = 160;
const SCOPE_MIN_H = 120;
const COMMENT_MIN_W = 80;
const COMMENT_MIN_H = 60;
const DISPLAY_MIN_W = 80;
const DISPLAY_MIN_H = 40;
const SCOPE_HANDLE_SIZE = 12;
const SCOPE_HANDLE_INSET = 4;
const TOGGLE_MIN_W = 48;
const TOGGLE_MIN_H = 30;
export function makeScopeCommentLayout(ctx: unknown) {
  const C = ctx as unknown as {
    updateBlockTransform: (b: Block) => void;
    updateSelectionBox: () => void;
  };

  function clampScopeSize(width: unknown, height: unknown) {
    return {
      width: Math.max(
        SCOPE_MIN_W,
        snapToGrid(Number.isFinite(width as number) ? (width as number) : SCOPE_MIN_W)
      ),
      height: Math.max(
        SCOPE_MIN_H,
        snapToGrid(Number.isFinite(height as number) ? (height as number) : SCOPE_MIN_H)
      ),
    };
  }

  function clampCommentSize(width: unknown, height: unknown) {
    return {
      width: Math.max(
        COMMENT_MIN_W,
        snapToGrid(Number.isFinite(width as number) ? (width as number) : COMMENT_MIN_W)
      ),
      height: Math.max(
        COMMENT_MIN_H,
        snapToGrid(Number.isFinite(height as number) ? (height as number) : COMMENT_MIN_H)
      ),
    };
  }

  function updatePortElement(port: PortLike) {
    if (!port?.el) return;
    const hit = port.el.querySelector('.port-hit');
    if (hit) {
      hit.setAttribute('cx', String(port.x ?? 0));
      hit.setAttribute('cy', String(port.y ?? 0));
    }
    const shape = port.el.querySelector('.port');
    if (!shape) return;
    if (shape.tagName.toLowerCase() === 'rect') {
      shape.setAttribute('x', String((port.x ?? 0) - 6));
      shape.setAttribute('y', String((port.y ?? 0) - 6));
    } else {
      shape.setAttribute('cx', String(port.x ?? 0));
      shape.setAttribute('cy', String(port.y ?? 0));
    }
  }

  function updateScopeLayout(block: BlockLike | null | undefined) {
    if (!block || (block.type !== 'scope' && block.type !== 'xyScope' && block.type !== 'xyzScope'))
      return;
    const { width, height } = clampScopeSize(block.width, block.height);
    (block as BlockLike).width = width;
    (block as BlockLike).height = height;
    if (block.params) {
      block.params.width = width;
      block.params.height = height;
    }
    if (block.bodyRect) {
      block.bodyRect.setAttribute('width', String(width));
      block.bodyRect.setAttribute('height', String(height));
    }
    if (block.scopePlot) {
      const plotW = Math.max(40, width - 20);
      const plotH = Math.max(40, height - 40);
      block.scopePlot.setAttribute('width', String(plotW));
      block.scopePlot.setAttribute('height', String(plotH));
      if (block.scopeClipRect) {
        block.scopeClipRect.setAttribute('x', String(block.scopePlot.getAttribute('x') ?? ''));
        block.scopeClipRect.setAttribute('y', String(block.scopePlot.getAttribute('y') ?? ''));
        block.scopeClipRect.setAttribute('width', String(plotW));
        block.scopeClipRect.setAttribute('height', String(plotH));
      }
    }
    if (block.dragRect) block.dragRect.setAttribute('width', String(width));
    if (block.resizeHandle) {
      block.resizeHandle.setAttribute('x', String(width - SCOPE_HANDLE_SIZE - SCOPE_HANDLE_INSET));
      block.resizeHandle.setAttribute('y', String(height - SCOPE_HANDLE_SIZE - SCOPE_HANDLE_INSET));
      block.resizeHandle.setAttribute('width', String(SCOPE_HANDLE_SIZE));
      block.resizeHandle.setAttribute('height', String(SCOPE_HANDLE_SIZE));
    }
    if (block.scopeBlockTitle) {
      const plotX = 10;
      const plotW2 = Math.max(40, width - 20);
      block.scopeBlockTitle.setAttribute('x', String(plotX + plotW2 / 2));
    }
    const inputPorts = (block.ports || [])
      .filter((p) => (p as PortLike).type === 'in')
      .sort((a, b) => ((a as PortLike).index ?? 0) - ((b as PortLike).index ?? 0)) as PortLike[];
    const count = inputPorts.length;
    const top = 40;
    const bottom = Math.max(top, height - 40);
    inputPorts.forEach((port: PortLike, index: number) => {
      const t = count > 1 ? index / (count - 1) : 0.5;
      port.x = 0;
      port.y = snapToGrid(top + (bottom - top) * t);
      port.side = 'left';
      port.wireX = port.x;
      port.wireY = port.y;
      updatePortElement(port);
    });
    if (block.type !== 'xyScope' && block.scopeInputHints && block.scopeInputHints.length) {
      inputPorts.forEach((port, index: number) => {
        const hint = block.scopeInputHints?.[index];
        if (hint) {
          hint.setAttribute('cx', '5');
          hint.setAttribute('cy', String(port.y ?? 0));
        }
      });
    }
    C.updateBlockTransform(block);
    if ((block as BlockLike).scopeData || block.type === 'xyScope') {
      (renderScope as unknown as (b: unknown) => void)(block);
    }
    C.updateSelectionBox();
  }

  function updateCommentLayout(block: BlockLike | null | undefined) {
    if (!block || block.type !== 'comment') return;
    const { width, height } = clampCommentSize(block.width, block.height);
    block.width = width;
    block.height = height;
    if (!block.params) block.params = {};
    block.params.width = width;
    block.params.height = height;
    const body = block.commentBody || (block.group && block.group.querySelector('.comment-body'));
    if (body) {
      body.setAttribute('width', String(width));
      body.setAttribute('height', String(height));
      (body as HTMLElement).style.display =
        block.params && block.params[FieldNames.kShowBorder] !== false ? '' : 'none';
    }
    const textPadding = 10;
    const foreign =
      block.commentForeign || (block.group && block.group.querySelector('.comment-foreign'));
    if (foreign) {
      foreign.setAttribute('x', String(textPadding));
      foreign.setAttribute('y', String(textPadding));
      foreign.setAttribute('width', String(Math.max(1, width - textPadding * 2)));
      foreign.setAttribute('height', String(Math.max(1, height - textPadding * 2)));
    }
    const textEl =
      block.commentTextEl || (block.group && block.group.querySelector('.comment-text'));
    const commentText = String(block.params?.[FieldNames.kCommentText] || '');
    try {
      if (textEl) {
        try {
          const maybeEl = textEl as unknown as HTMLElement;
          maybeEl.textContent = commentText;
          try {
            maybeEl.innerText = commentText;
          } catch (_e) {}
          try {
            (maybeEl.style as CSSStyleDeclaration).whiteSpace = 'pre-wrap';
          } catch (_e) {}
        } catch (_e) {
          // fallback for SVG text nodes
          try {
            (textEl as Element).textContent = commentText;
          } catch (_e2) {}
        }
      }
    } catch (_e) {}
    const fallback =
      block.commentTextFallback ||
      (block.group && block.group.querySelector('.comment-text-fallback'));
    if (fallback) fallback.textContent = commentText;

    // If the manifest-based math renderer added a math group (e.g. '\\#'),
    // hide it so the comment text (foreignObject/div) is visible instead.
    try {
      const mathGroup =
        (block.group &&
          (block.group.querySelector('.comment-math') ||
            block.group.querySelector('.nf-block-formula'))) ||
        null;
      if (mathGroup) {
        try {
          (mathGroup as unknown as HTMLElement).style.display = 'none';
        } catch (_e) {
          try {
            (mathGroup as Element).setAttribute('display', 'none');
          } catch (_e2) {}
        }
      }
    } catch (_e) {}
    if (block.dragRect) {
      const minDragSize = 80;
      const dragWidth = Math.max(width, minDragSize);
      const dragHeight = Math.max(height, minDragSize);
      block.dragRect.setAttribute('x', String((width - dragWidth) / 2));
      block.dragRect.setAttribute('y', String((height - dragHeight) / 2));
      block.dragRect.setAttribute('width', String(dragWidth));
      block.dragRect.setAttribute('height', String(dragHeight));
    }
    if (block.resizeHandle) {
      block.resizeHandle.setAttribute('x', String(width - SCOPE_HANDLE_SIZE - SCOPE_HANDLE_INSET));
      block.resizeHandle.setAttribute('y', String(height - SCOPE_HANDLE_SIZE - SCOPE_HANDLE_INSET));
      block.resizeHandle.setAttribute('width', String(SCOPE_HANDLE_SIZE));
      block.resizeHandle.setAttribute('height', String(SCOPE_HANDLE_SIZE));
    }
    C.updateBlockTransform(block);
    C.updateSelectionBox();
  }

  function clampDisplaySize(width: unknown, height: unknown) {
    return {
      width: Math.max(
        DISPLAY_MIN_W,
        snapToGrid(Number.isFinite(width as number) ? (width as number) : DISPLAY_MIN_W)
      ),
      height: Math.max(
        DISPLAY_MIN_H,
        snapToGrid(Number.isFinite(height as number) ? (height as number) : DISPLAY_MIN_H)
      ),
    };
  }

  function updateDisplayLayout(block: BlockLike | null | undefined) {
    if (!block || block.type !== 'display') return;
    const { width, height } = clampDisplaySize(block.width, block.height);
    block.width = width;
    block.height = height;
    if (block.params) {
      block.params.width = width;
      block.params.height = height;
    }
    if (block.bodyRect) {
      block.bodyRect.setAttribute('width', String(width));
      block.bodyRect.setAttribute('height', String(height));
    }
    const titleBar = block.group && block.group.querySelector('.display-title-bar');
    if (titleBar) {
      titleBar.setAttribute('width', String(width));
      titleBar.setAttribute('height', '16');
    }
    if (block.displayTitleEl) {
      block.displayTitleEl.setAttribute('x', String(width / 2));
      block.displayTitleEl.setAttribute('y', '11');
    }
    const valueArea = block.group && block.group.querySelector('.display-value-area');
    if (valueArea) {
      valueArea.setAttribute('x', '4');
      valueArea.setAttribute('y', '20');
      valueArea.setAttribute('width', String(Math.max(1, width - 8)));
      valueArea.setAttribute('height', String(Math.max(1, height - 24)));
    }
    if (block.displayValueEl) {
      block.displayValueEl.setAttribute('x', String(width / 2));
      block.displayValueEl.setAttribute('y', String(20 + Math.floor((height - 24) / 2) + 5));
    }
    const inputPorts = (block.ports || [])
      .filter((p) => (p as PortLike).type === 'in')
      .sort((a, b) => ((a as PortLike).index ?? 0) - ((b as PortLike).index ?? 0)) as PortLike[];
    inputPorts.forEach((port: PortLike) => {
      port.x = 0;
      port.y = snapToGrid(Math.floor(height / 2));
      port.side = 'left';
      port.wireX = port.x;
      port.wireY = port.y;
      updatePortElement(port);
    });
    if (block.dragRect) {
      const minDragSize = 80;
      const dragWidth = Math.max(width, minDragSize);
      const dragHeight = Math.max(height, minDragSize);
      block.dragRect.setAttribute('x', String((width - dragWidth) / 2));
      block.dragRect.setAttribute('y', String((height - dragHeight) / 2));
      block.dragRect.setAttribute('width', String(dragWidth));
      block.dragRect.setAttribute('height', String(dragHeight));
    }
    if (block.resizeHandle) {
      block.resizeHandle.setAttribute('x', String(width - SCOPE_HANDLE_SIZE - SCOPE_HANDLE_INSET));
      block.resizeHandle.setAttribute('y', String(height - SCOPE_HANDLE_SIZE - SCOPE_HANDLE_INSET));
      block.resizeHandle.setAttribute('width', String(SCOPE_HANDLE_SIZE));
      block.resizeHandle.setAttribute('height', String(SCOPE_HANDLE_SIZE));
    }
    C.updateBlockTransform(block);
    C.updateSelectionBox();
  }

  function updateToggleLayout(block: BlockLike | null | undefined) {
    if (!block || block.type !== FieldNames.kToggleSwitch) return;
    const width = Math.max(
      TOGGLE_MIN_W,
      snapToGrid(Number.isFinite(block.width as number) ? (block.width as number) : TOGGLE_MIN_W)
    );
    const height = Math.max(
      TOGGLE_MIN_H,
      snapToGrid(Number.isFinite(block.height as number) ? (block.height as number) : TOGGLE_MIN_H)
    );
    block.width = width;
    block.height = height;
    if (block.params) {
      block.params.width = width;
      block.params.height = height;
    }
    const g = block.group ?? null;
    const body = block.bodyRect || (g && g.querySelector('rect.nf-block'));
    if (body) {
      body.setAttribute('width', String(width));
      body.setAttribute('height', String(height));
    }
    if (!g) {
      C.updateBlockTransform(block);
      C.updateSelectionBox();
      return;
    }
    const track = g.querySelector('.toggle-track') as SVGRectElement | null;
    if (track) {
      const trackW = Math.max(24, Math.floor(width - 24));
      const trackH = Math.max(12, Math.min(22, Math.floor(height - 12)));
      const trackX = Math.max(4, Math.floor((width - trackW) / 2));
      const trackY = Math.max(4, Math.floor((height - trackH) / 2));
      track.setAttribute('x', String(trackX));
      track.setAttribute('y', String(trackY));
      track.setAttribute('width', String(trackW));
      track.setAttribute('height', String(trackH));
      track.setAttribute('rx', String(Math.floor(trackH / 2)));
      track.setAttribute('ry', String(Math.floor(trackH / 2)));

      const knob = g.querySelector('.toggle-knob') as SVGCircleElement | null;
      const params = block.params as unknown as {
        State?: number | boolean | string;
      };
      if (knob) {
        const kr = Math.floor(trackH / 2) - 2;
        const leftCx = trackX + kr + 2;
        const rightCx = trackX + trackW - kr - 2;
        const cy = trackY + Math.floor(trackH / 2);
        knob.setAttribute('cx', String(leftCx));
        knob.setAttribute('cy', String(cy));
        knob.setAttribute('r', String(kr));
        try {
          const kEl = knob as unknown as SVGElement & { style: CSSStyleDeclaration };
          const s = Number(params?.[FieldNames.kState] ?? 0) ? 1 : 0;
          kEl.style.transform = s ? `translate(${rightCx - leftCx}px, 0)` : 'translate(0, 0)';
        } catch (_e) {
          knob.setAttribute(
            'cx',
            String(Number(params?.[FieldNames.kState] ?? 0) ? rightCx : leftCx)
          );
        }
      }

      const label = g.querySelector('.toggle-label') as SVGTextElement | null;
      if (label) {
        const labelX = Math.floor(width / 2);
        const labelY = trackY + trackH + 14;
        label.setAttribute('x', String(labelX));
        label.setAttribute('y', String(labelY));
      }
    }

    (block.ports || []).forEach((p: PortLike) => {
      if (p.type === 'out') {
        p.x = width;
        p.y = Math.floor(height / 2);
        p.wireX = p.x;
        p.wireY = p.y;
        updatePortElement(p);
      }
    });

    if (block.dragRect) {
      block.dragRect.setAttribute('width', String(width));
    }
    if (block.resizeHandle) {
      block.resizeHandle.setAttribute('x', String(width - SCOPE_HANDLE_SIZE - SCOPE_HANDLE_INSET));
      block.resizeHandle.setAttribute('y', String(height - SCOPE_HANDLE_SIZE - SCOPE_HANDLE_INSET));
      block.resizeHandle.setAttribute('width', String(SCOPE_HANDLE_SIZE));
      block.resizeHandle.setAttribute('height', String(SCOPE_HANDLE_SIZE));
    }
    C.updateBlockTransform(block);
    C.updateSelectionBox();
  }

  return {
    clampScopeSize,
    clampCommentSize,
    updatePortElement,
    updateScopeLayout,
    updateCommentLayout,
    updateDisplayLayout,
    updateToggleLayout,
  };
}
//=============================================================================
