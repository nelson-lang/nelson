//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createSvgElement } from '../svg-elements.js';
import type { RendererCtx, Block } from '../core/renderer-types.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
export function makeParamDisplayHandlers(ctx: RendererCtx) {
  void ctx;
  function updateParamDisplay(block: any | Block) {
    const getScopeFallbackValue = (key: string) => {
      const limits = (block as any).computedLimits || {};
      if ((block as any).type === 'scope') {
        if (key === FieldNames.kTMin) return limits.tMin ?? 0;
        if (key === FieldNames.kTMax) {
          const runtimeEl =
            typeof document !== 'undefined' ? document.getElementById('runtimeInput') : null;
          const runtimeValue = Number((runtimeEl as HTMLInputElement | null)?.value);
          return limits.tMax ?? (Number.isFinite(runtimeValue) ? runtimeValue : 1);
        }
        if (key === FieldNames.kYMin) return limits.yMin ?? -1.2;
        if (key === FieldNames.kYMax) return limits.yMax ?? 1.2;
      }
      if ((block as any).type === 'xyScope') {
        if (key === FieldNames.kXMin) return limits.xMin ?? -1.2;
        if (key === FieldNames.kXMax) return limits.xMax ?? 1.2;
        if (key === FieldNames.kYMin) return limits.yMin ?? -1.2;
        if (key === FieldNames.kYMax) return limits.yMax ?? 1.2;
      }
      return null;
    };

    const getDisplayValue = (key: string) => {
      const fallback = getScopeFallbackValue(key);
      if (fallback != null) return String(fallback);
      const raw = (block as any).params?.[key];
      if (raw != null && String(raw).trim() !== '') return String(raw);
      return '';
    };

    const textEl = (block as any).paramDisplay;
    if (!textEl) return;
    const visible = (block as any).params?._visible;
    if (!visible || typeof visible !== 'object') {
      textEl.setAttribute('display', 'none');
      textEl.textContent = '';
      return;
    }
    const labels = ((block as any).paramLabels as Record<string, string>) || {};
    const paramsObj = (block as any).params || {};
    const entries = Object.entries(visible).filter(([key, on]) => {
      if (!on || key === '_visible') return false;
      if (Object.prototype.hasOwnProperty.call(labels, key)) return true;
      if (Object.prototype.hasOwnProperty.call(paramsObj, key)) return true;
      if (getScopeFallbackValue(key) != null) return true;
      return false;
    });
    if (!entries.length) {
      textEl.setAttribute('display', 'none');
      textEl.textContent = '';
      return;
    }
    while (textEl.firstChild) textEl.removeChild(textEl.firstChild);
    const baseX = (block as any).width / 2;
    const baseY = (block as any).height + 14;
    entries.forEach(([key], index) => {
      const label = ((block as any).paramLabels && (block as any).paramLabels[key]) || key;
      const tspan = createSvgElement('tspan', { x: baseX, y: baseY + index * 12 });
      tspan.textContent = `${label}: ${getDisplayValue(key)}`;
      textEl.appendChild(tspan);
    });
    textEl.setAttribute('display', 'block');
  }

  return { updateParamDisplay };
}
//=============================================================================
