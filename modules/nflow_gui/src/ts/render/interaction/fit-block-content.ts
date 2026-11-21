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
import { measureUserFuncTex } from '../svg-elements.js';

const formulas: Record<string, string> = {
  constant: '.nf-constant-formula',
  gain: '.nf-gain-formula',
};

export function canFitBlockContent(block: any): boolean {
  return !!block?.resizeHandle && (block.type === 'comment' || block.type in formulas);
}

export function fitBlockToContent(block: any): boolean {
  if (!canFitBlockContent(block)) return false;
  let width: number;
  let height: number;
  if (block.type === 'comment') {
    const lines = String(block.params?.CommentText ?? '').split(/\r?\n/);
    const text = block.commentTextEl as HTMLElement | undefined;
    const style = text && typeof getComputedStyle === 'function' ? getComputedStyle(text) : null;
    const fontSize = Number.parseFloat(style?.fontSize || '') || 14;
    const lineHeight = Number.parseFloat(style?.lineHeight || '') || fontSize * 1.3;
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    if (ctx) ctx.font = style?.font || `${fontSize}px sans-serif`;
    width = Math.max(
      80,
      Math.ceil(
        Math.max(
          ...lines.map((line) => ctx?.measureText(line).width ?? line.length * fontSize * 0.65)
        ) + 24
      )
    );
    height = Math.max(40, Math.ceil(lines.length * lineHeight + 24));
  } else {
    const group = block.group?.querySelector(formulas[block.type]);
    if (!group) return false;
    const tex = group.querySelector('[data-tex]')?.getAttribute('data-tex');
    const measured = tex ? measureUserFuncTex(tex) : null;
    const natural = Array.from(group.querySelectorAll('[data-natural-width]')).map(
      (el: any) => Number(el.getAttribute('data-natural-width')) || 0
    );
    const contentWidth = measured?.w || Math.max(0, ...natural);
    if (!contentWidth) return false;
    const contentHeight = measured?.h || 20;
    width = Math.max(40, Math.ceil((contentWidth + 20) / (block.type === 'gain' ? 0.6 : 1)));
    height = Math.max(40, Math.ceil(contentHeight + 20));
  }
  block.resizeHandle.dispatchEvent(
    new CustomEvent('nflow:resize-to', { detail: { width, height } })
  );
  return true;
}
