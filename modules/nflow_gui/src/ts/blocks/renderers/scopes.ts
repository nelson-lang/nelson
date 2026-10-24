//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import * as FieldNames from '../../FieldNames.js';
import { setupFieldsetToggles, setupSignalsCollapseAndBindings } from '../inspector-utils.js';
import { registerRenderer } from '../inspector-registry.js';
import {
  renderScopeInspectorPanel,
  renderXyScopeInspectorPanel,
  renderXyzScopeInspectorPanel,
} from '../components/scope-inspector-panel.js';
import { readInspectorSectionOpen, writeInspectorSectionOpen } from '../inspector-section-prefs.js';
//=============================================================================
export function renderScopes(block: Block, deps: BlockPanelDeps): boolean {
  const { inspectorBody, callUpdate, callResize, renderScope, getRuntimeSeconds } = deps;
  const inspectorEl = inspectorBody as HTMLElement | null;

  const qs = <T extends Element = Element>(sel: string) => inspectorEl?.querySelector<T>(sel);
  if (block.type === 'scope') {
    const runtime = typeof getRuntimeSeconds === 'function' ? getRuntimeSeconds() : null;
    renderScopeInspectorPanel(inspectorEl, block, runtime);

    [
      FieldNames.kTMin,
      FieldNames.kTMax,
      FieldNames.kYMin,
      FieldNames.kYMax,
      FieldNames.kTimeWindow,
    ].forEach((key) => {
      const input = qs<HTMLInputElement>(`input[data-edit='${key}']`);
      if (!input) return;
      input.addEventListener('input', () => {
        (block.params as Record<string, unknown>)[key] = input.value;
        renderScope?.(block);
      });
    });

    try {
      const signalsWrapper = inspectorEl?.querySelector<HTMLDivElement>('.signals-wrapper');
      const signalsFieldset = signalsWrapper?.closest('fieldset.param-group') as HTMLElement | null;
      if (signalsWrapper && signalsFieldset) {
        if ((signalsFieldset as HTMLElement).dataset.toggleBound !== 'true') {
          (signalsFieldset as HTMLElement).dataset.toggleBound = 'true';
          const legend = signalsFieldset.querySelector('legend');
          const visKey = 'signals';
          const blockVisible =
            (block.params._visible as Record<string, unknown> | undefined)?.[visKey] === true;
          const visible = readInspectorSectionOpen(`group-${visKey}`, blockVisible);
          if (visible) {
            signalsWrapper.classList.remove('collapsed');
            Array.from(signalsWrapper.children as unknown as Element[]).forEach((c) => {
              (c as HTMLElement).style.display = '';
            });
          } else {
            signalsWrapper.classList.add('collapsed');
            Array.from(signalsWrapper.children as unknown as Element[]).forEach((c) => {
              (c as HTMLElement).style.display = 'none';
            });
          }
          if (legend) {
            legend.setAttribute('role', 'button');
            legend.setAttribute('tabindex', '0');
            legend.setAttribute('aria-expanded', String(visible));
            legend.style.cursor = 'pointer';
            const toggle = () => {
              const nowCollapsed = signalsWrapper.classList.toggle('collapsed');
              Array.from(signalsWrapper.children as unknown as Element[]).forEach((c) => {
                (c as HTMLElement).style.display = nowCollapsed ? 'none' : '';
              });
              if (!block.params._visible) block.params._visible = {} as Record<string, unknown>;
              (block.params._visible as Record<string, unknown>)[visKey] = !nowCollapsed;
              writeInspectorSectionOpen(`group-${visKey}`, !nowCollapsed);
              legend.setAttribute('aria-expanded', String(!nowCollapsed));
              callUpdate(block);
            };
            legend.addEventListener('click', toggle);
            legend.addEventListener('keydown', (ev: KeyboardEvent) => {
              if (ev.key === 'Enter' || ev.key === ' ') {
                ev.preventDefault();
                toggle();
              }
            });
          }
        }
      }
    } catch (e) {}

    setupFieldsetToggles(block, inspectorEl, callUpdate, [
      'Display',
      'Appearance',
      'Advanced',
      'Labels',
    ]);

    [FieldNames.kWidth, FieldNames.kHeight].forEach((key) => {
      const input = qs<HTMLInputElement>(`input[data-edit='${key}']`);
      if (!input) return;
      input.addEventListener('change', () => {
        const widthValue = Number(
          qs<HTMLInputElement>(`[data-edit="${FieldNames.kWidth}"]`)?.value
        );
        const heightValue = Number(
          qs<HTMLInputElement>(`[data-edit="${FieldNames.kHeight}"]`)?.value
        );
        callResize(block, widthValue, heightValue);
        input.value = String(key === FieldNames.kWidth ? block.width ?? '' : block.height ?? '');
      });
    });

    const showTickLabelsInput = qs<HTMLInputElement>(
      `input[data-edit="${FieldNames.kShowTickLabels}"]`
    );
    if (showTickLabelsInput) {
      showTickLabelsInput.addEventListener('change', () => {
        block.params.ShowTickLabels = showTickLabelsInput.checked;
        renderScope?.(block);
      });
    }

    setupSignalsCollapseAndBindings(block, inspectorEl, callUpdate, renderScope);

    return true;
  } else if (block.type === 'xyScope') {
    renderXyScopeInspectorPanel(inspectorEl, block);

    for (const key of [
      FieldNames.kShowGrid,
      FieldNames.kShowLegend,
      FieldNames.kShowTitle,
    ] as const) {
      const cb = qs<HTMLInputElement>(`input[data-edit="${key}"]`);
      if (cb)
        cb.addEventListener('change', () => {
          block.params[key] = cb.checked;
          if (key === FieldNames.kShowTitle && cb.checked) {
            try {
              if (!block.params.name || String(block.params.name).trim() === '') {
                block.params.name = (block && (block.label as string)) || String(block.type || '');
              }
            } catch (_e) {}
          }
          renderScope?.(block);
        });
    }

    const themeSelectXY = qs<HTMLSelectElement>(`select[data-edit="${FieldNames.kTheme}"]`);
    if (themeSelectXY)
      themeSelectXY.addEventListener('change', () => {
        block.params.theme = themeSelectXY.value;
        renderScope?.(block);
      });

    const bgInputXY = qs<HTMLInputElement>(`input[data-edit="backgroundColor"]`);
    if (bgInputXY) {
      try {
        let initialColor: string | undefined =
          (block.params && (block.params.backgroundColor as string)) || undefined;
        if (!initialColor) {
          try {
            const plotEl = (block as Record<string, unknown>)['scopePlot'] as Element | undefined;
            if (plotEl && plotEl.getAttribute) {
              const attrFill = plotEl.getAttribute('fill');
              if (attrFill && attrFill !== 'none') initialColor = attrFill;
              else {
                const cs = window.getComputedStyle(plotEl as Element);
                const compFill = cs?.getPropertyValue('fill');
                if (compFill && compFill !== 'none') initialColor = compFill;
              }
            }
          } catch (e) {}
        }
        if (!initialColor)
          initialColor =
            (block.params && (block.params.theme as string)) === 'light' ? '#ffffff' : '#101010';
        const colorToHex = (c?: string | null): string | null => {
          if (!c) return null;
          const s = String(c).trim();
          if (!s) return null;
          if (s.startsWith('#')) {
            if (s.length === 4) return '#' + s[1] + s[1] + s[2] + s[2] + s[3] + s[3];
            return s;
          }
          const m = s.match(/rgba?\(\s*(\d{1,3})[^\d]*(\d{1,3})[^\d]*(\d{1,3})/i);
          if (m) {
            const r = Number(m[1]);
            const g = Number(m[2]);
            const b = Number(m[3]);
            if ([r, g, b].every((n) => Number.isFinite(n))) {
              const toHex = (n: number) => n.toString(16).padStart(2, '0');
              return '#' + toHex(r) + toHex(g) + toHex(b);
            }
          }
          try {
            const tmp = document.createElement('div');
            tmp.style.color = s;
            document.body.appendChild(tmp);
            const comp = window.getComputedStyle(tmp).color;
            document.body.removeChild(tmp);
            const mm = String(comp).match(/rgb\(\s*(\d{1,3})[^\d]*(\d{1,3})[^\d]*(\d{1,3})/);
            if (mm) {
              const r = Number(mm[1]);
              const g = Number(mm[2]);
              const b = Number(mm[3]);
              const toHex = (n: number) => n.toString(16).padStart(2, '0');
              return '#' + toHex(r) + toHex(g) + toHex(b);
            }
          } catch (e) {}
          return null;
        };
        const hex = colorToHex(initialColor);
        if (hex) bgInputXY.value = hex;
        else bgInputXY.value = initialColor || '';
        try {
          const plotEl = (block as Record<string, unknown>)['scopePlot'] as
            | Element
            | undefined
            | null;
          const applyCol = (hex || initialColor) as string | undefined | null;
          if (plotEl && applyCol) {
            try {
              if (plotEl && (plotEl as HTMLElement).style)
                (plotEl as HTMLElement).style.setProperty('fill', String(applyCol), 'important');
            } catch (e) {}
            try {
              plotEl.setAttribute('fill', String(applyCol));
            } catch (e) {}
          }
        } catch (e) {}
      } catch (e) {}

      bgInputXY.addEventListener('input', () => {
        if (!block.params) block.params = {} as Record<string, unknown>;
        block.params.backgroundColor = bgInputXY.value;
        try {
          const plotEl = (block as Record<string, unknown>)['scopePlot'] as
            | Element
            | undefined
            | null;
          if (plotEl) {
            try {
              if (plotEl && (plotEl as HTMLElement).style)
                (plotEl as HTMLElement).style.setProperty('fill', bgInputXY.value, 'important');
            } catch (e) {}
            try {
              plotEl.setAttribute('fill', bgInputXY.value);
            } catch (e) {}
          }
        } catch (e) {}
        try {
          callUpdate(block);
        } catch (e) {}
        try {
          renderScope?.(block);
        } catch (e) {}
      });
    }

    for (const key of [FieldNames.kXLabel, FieldNames.kYLabel] as const) {
      const input = qs<HTMLInputElement>(`input[data-edit="${key}"]`);
      if (!input) continue;
      input.addEventListener('input', () => {
        if (!block.params) block.params = {} as Record<string, unknown>;
        block.params[key] = input.value;
        try {
          callUpdate(block);
        } catch (e) {}
        try {
          renderScope?.(block);
        } catch (e) {}
      });
    }

    setupSignalsCollapseAndBindings(block, inspectorEl, callUpdate, renderScope);

    return true;
  } else if (block.type === 'xyzScope') {
    renderXyzScopeInspectorPanel(inspectorEl, block);

    setupFieldsetToggles(block, inspectorEl, callUpdate, [
      'Display',
      'Appearance',
      'Advanced',
      'Labels',
    ]);

    [
      FieldNames.kXMin,
      FieldNames.kXMax,
      FieldNames.kYMin,
      FieldNames.kYMax,
      FieldNames.kZMin,
      FieldNames.kZMax,
    ].forEach((key) => {
      const input = qs<HTMLInputElement>(`input[data-edit='${key}']`);
      if (!input) return;
      input.addEventListener('input', () => {
        (block.params as Record<string, unknown>)[key] = input.value;
        renderScope?.(block);
      });
    });

    [FieldNames.kRotationX, FieldNames.kRotationY].forEach((key) => {
      const input = qs<HTMLInputElement>(`input[data-edit='${key}']`);
      if (!input) return;
      input.addEventListener('input', () => {
        block.params[key] = Number(input.value);
        renderScope?.(block);
      });
    });

    [FieldNames.kWidth, FieldNames.kHeight].forEach((key) => {
      const input = qs<HTMLInputElement>(`input[data-edit='${key}']`);
      if (!input) return;
      input.addEventListener('change', () => {
        const widthValue = Number(
          qs<HTMLInputElement>(`[data-edit=\"${FieldNames.kWidth}\"]`)?.value
        );
        const heightValue = Number(
          qs<HTMLInputElement>(`[data-edit=\"${FieldNames.kHeight}\"]`)?.value
        );
        callResize(block, widthValue, heightValue);
        input.value = String(key === FieldNames.kWidth ? block.width ?? '' : block.height ?? '');
      });
    });

    const showTickLabelsInput2 = qs<HTMLInputElement>(
      `input[data-edit=\"${FieldNames.kShowTickLabels}\"]`
    );
    if (showTickLabelsInput2)
      showTickLabelsInput2.addEventListener('change', () => {
        block.params.ShowTickLabels = showTickLabelsInput2.checked;
        renderScope?.(block);
      });

    setupSignalsCollapseAndBindings(block, inspectorEl, callUpdate, renderScope);

    return true;
  }

  return false;
}
//=============================================================================
try {
  registerRenderer(renderScopes);
} catch (e) {}
//=============================================================================
