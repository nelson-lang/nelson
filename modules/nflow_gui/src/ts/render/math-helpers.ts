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
import { exprToLatex, estimateLatexWidth } from '../utils/expr.js';
import { measureUserFuncTex } from './svg-elements.js';
//=============================================================================
const USERFUNC_MIN_WIDTH = 80;
const USERFUNC_FIXED_HEIGHT = 80;
const USERFUNC_PADDING_X = 12;
const USERFUNC_PADDING_Y = 16;
export function makeMathHelpers({ svg }: any) {
  function getSvgScale() {
    const viewBox = svg.getAttribute('viewBox');
    if (!viewBox) return { x: 1, y: 1 };
    const parts = viewBox.split(' ').map((v: string) => Number(v));
    if (parts.length !== 4 || parts.some((v: number) => !Number.isFinite(v))) {
      return { x: 1, y: 1 };
    }
    const [, , vbWidth, vbHeight] = parts;
    if (!vbWidth || !vbHeight) return { x: 1, y: 1 };
    const scaleX = svg.clientWidth / vbWidth;
    const scaleY = svg.clientHeight / vbHeight;
    return { x: scaleX || 1, y: scaleY || 1 };
  }
  function pxToSvg(px: number, axis: 'x' | 'y', scale: any = null) {
    const currentScale = scale || getSvgScale();
    const divisor = axis === 'y' ? currentScale.y || 1 : currentScale.x || 1;
    return px / divisor;
  }
  function scaleMathToFit(mathGroup: any, targetWidth: number, targetHeight: number, padding = 8) {
    if (!mathGroup) return;
    const span = mathGroup.querySelector('span');
    const wrapper = mathGroup.querySelector('.math-foreign');
    if (!span || !wrapper || span.classList.contains('katex-target')) return;
    const rect = span.getBoundingClientRect();
    if (!rect.width || !rect.height) return;
    const scale = getSvgScale();
    const measuredW = rect.width / scale.x;
    const measuredH = rect.height / scale.y;
    const availableW = Math.max(1, targetWidth - padding * 2);
    const availableH = Math.max(1, targetHeight - padding * 2);
    const factor = Math.min(1, availableW / measuredW, availableH / measuredH);
    if (factor < 0.999) {
      wrapper.style.transformOrigin = 'center center';
      wrapper.style.transform = `scale(${factor})`;
    } else {
      wrapper.style.transform = '';
    }
  }
  function getMathSpanSize(mathGroup: any, { rawPx = false } = {}) {
    const span = mathGroup?.querySelector?.('span');
    if (!span || span.classList.contains('katex-target')) return null;
    const target = mathGroup.querySelector('.katex') || span;
    const rect = target.getBoundingClientRect();
    const scrollW = target.scrollWidth || 0;
    const scrollH = target.scrollHeight || 0;
    const width = Math.max(rect.width, scrollW);
    const height = Math.max(rect.height, scrollH);
    if (!width || !height) return null;
    if (rawPx) {
      return { w: width, h: height };
    }
    const scale = getSvgScale();
    return {
      w: pxToSvg(width, 'x', scale),
      h: pxToSvg(height, 'y', scale),
    };
  }
  function setUserFuncMathBox(mathGroup: any, width: number, height: number) {
    if (!mathGroup) return;
    const foreign = mathGroup.querySelector('foreignObject');
    if (!foreign) return;
    foreign.setAttribute('width', String(width));
    foreign.setAttribute('height', String(height));
  }
  function computeUserFuncSize(block: any, { force = false, mathGroup = null } = {}) {
    const rawExpr = String(block.params?.Expr || 'u');
    const latex = exprToLatex(rawExpr);
    const estimateWidth = estimateLatexWidth(latex);
    const rawEstimate = USERFUNC_MIN_WIDTH;
    let width = rawEstimate;
    let height = USERFUNC_FIXED_HEIGHT;
    const measuredTex = measureUserFuncTex(`\\scriptsize{${latex}}`);
    const hasMeasuredTex = Boolean(
      measuredTex && Number.isFinite(measuredTex.w) && Number.isFinite(measuredTex.h)
    );
    if (!hasMeasuredTex) {
      width = Math.max(width, estimateWidth);
    }
    if (hasMeasuredTex) {
      width = Math.max(width, Math.ceil((measuredTex?.w ?? 0) + USERFUNC_PADDING_X * 2));
      height = USERFUNC_FIXED_HEIGHT;
    }
    if (mathGroup) {
      for (let pass = 0; pass < 2; pass += 1) {
        setUserFuncMathBox(mathGroup, width, height);
        const size = getMathSpanSize(mathGroup);
        if (!size) continue;
        const neededW = Math.ceil(size.w + USERFUNC_PADDING_X * 2);
        const targetW = Math.max(USERFUNC_MIN_WIDTH, neededW);
        const needsResize = targetW !== width;
        if (!needsResize && !force) break;
        width = targetW;
        height = USERFUNC_FIXED_HEIGHT;
      }
    }
    return {
      width,
      height,
      rawExpr,
      latex,
      estimateWidth,
      rawEstimate,
      measuredTex,
      hasMeasuredTex,
      paddingX: USERFUNC_PADDING_X,
      paddingY: USERFUNC_PADDING_Y,
    };
  }
  function _computeRenderedUserFuncSize(mathGroup: any) {
    const size = getMathSpanSize(mathGroup);
    if (!size) return null;
    return {
      width: Math.max(USERFUNC_MIN_WIDTH, Math.ceil(size.w + USERFUNC_PADDING_X * 2)),
      height: USERFUNC_FIXED_HEIGHT,
    };
  }
  function getAutoMathConfig(block: any) {
    if (!block) return null;
    if (block.type === 'expression') {
      return {
        className: '.nf-userfunc-formula',
        minWidth: USERFUNC_MIN_WIDTH,
        fixedHeight: USERFUNC_FIXED_HEIGHT,
        paddingX: USERFUNC_PADDING_X,
      };
    }
    if (block.type === 'tf') {
      return {
        className: '.nf-tf-formula',
        minWidth: 80,
        fixedHeight: 80,
        paddingX: USERFUNC_PADDING_X,
      };
    }
    if (block.type === 'dtf') {
      return {
        className: '.dtf-math',
        minWidth: 80,
        fixedHeight: 80,
        paddingX: USERFUNC_PADDING_X,
      };
    }
    return null;
  }
  function computeRenderedAutoMathSize(block: any, mathGroup: any) {
    const config = getAutoMathConfig(block);
    if (!config) return null;
    const size = getMathSpanSize(mathGroup);
    if (!size) return null;
    return {
      width: Math.max(config.minWidth, Math.ceil(size.w + config.paddingX * 2)),
      height: config.fixedHeight,
    };
  }
  function positionGainMath(mathGroup: any, block: any, padding = 8) {
    if (!mathGroup || !block) return;
    const size = getMathSpanSize(mathGroup);
    if (!size) return;
    const desiredCenter = block.width * 0.34;
    const half = size.w / 2;
    const minCenter = padding + half;
    const maxCenter = block.width - padding - half;
    const clampedCenter = Math.min(maxCenter, Math.max(minCenter, desiredCenter));
    const shiftX = clampedCenter - block.width / 2;
    mathGroup.setAttribute('transform', `translate(${shiftX}, 0)`);
  }
  function toLeadingMinusTex(value: any, fallback = '0') {
    const text = String(value ?? fallback).trim();
    if (!text) return { tex: fallback, hasLeadingNegative: false };
    if (text.startsWith('-')) {
      const rest = text.slice(1).trimStart();
      return {
        tex: `\\text{-}${rest || fallback}`,
        hasLeadingNegative: true,
      };
    }
    return { tex: text, hasLeadingNegative: false };
  }
  return {
    getSvgScale,
    pxToSvg,
    scaleMathToFit,
    getMathSpanSize,
    setUserFuncMathBox,
    computeUserFuncSize,
    _computeRenderedUserFuncSize,
    getAutoMathConfig,
    computeRenderedAutoMathSize,
    positionGainMath,
    toLeadingMinusTex,
  };
}
//=============================================================================
