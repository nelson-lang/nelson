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
import type { Block } from './inspector.js';
import type { BlockPanelDeps } from './inspector-block.js';
import { readInspectorSectionOpen, writeInspectorSectionOpen } from './inspector-section-prefs.js';
//=============================================================================
export function parseList(value: string) {
  return value
    .split(',')
    .map((v) => v.trim())
    .filter((v) => v.length)
    .map((v) => {
      const num = Number(v);
      return Number.isFinite(num) ? num : v;
    });
}
//=============================================================================
export function asDisplayValue(rawValue: unknown, fallbackValue?: unknown) {
  const rawText = rawValue == null ? '' : String(rawValue);
  if (rawText.trim()) return rawText;
  if (fallbackValue == null) return '';
  if (typeof fallbackValue === 'number') {
    return Number.isFinite(fallbackValue) ? String(fallbackValue) : '';
  }
  const fallbackText = String(fallbackValue);
  return fallbackText.trim();
}
//=============================================================================
export function formatSignificant(value: unknown, digits = 2) {
  if (typeof value !== 'number' || !Number.isFinite(value)) return '';
  if (value === 0) return '0';
  return Number(value.toPrecision(digits)).toString();
}
//=============================================================================
export function asDisplayValueWithFormatter(
  rawValue: unknown,
  fallbackValue: unknown,
  formatter?: (v: unknown) => string
) {
  const rawText = rawValue == null ? '' : String(rawValue);
  if (rawText.trim()) return rawText;
  if (fallbackValue == null) return '';
  if (typeof formatter === 'function') return formatter(fallbackValue);
  return asDisplayValue(rawValue, fallbackValue);
}
//=============================================================================
export function setupParamVisibility(
  block: Block | null,
  inspectorBody: HTMLElement | null,
  callUpdate: (b: Block) => void
): void {
  if (!block || !inspectorBody) return;
  const paramRows = Array.from(inspectorBody.querySelectorAll<HTMLElement>('.param'));
  if (!paramRows.length) return;
  if (!block.params) block.params = {};
  if (!block.params._visible || typeof block.params._visible !== 'object') {
    block.params._visible = {};
  }
  const visible = block.params._visible as Record<string, unknown>;

  paramRows.forEach((row) => {
    const input = row.querySelector<HTMLElement>('[data-edit]');
    if (!input) return;
    const key = input.getAttribute('data-edit');
    if (!key) return;
    const textNodes = Array.from(row.childNodes)
      .filter((node) => node.nodeType === Node.TEXT_NODE)
      .map((node) => node.textContent || '')
      .join(' ')
      .trim();
    const existingNameEl = row.querySelector<HTMLElement>('.param-name');
    const labelText = textNodes || existingNameEl?.textContent?.trim() || key;
    const labels =
      (block.paramLabels as Record<string, string>) ||
      (block.paramLabels = {} as Record<string, string>);
    labels[key] = labelText;
    let nameEl = row.querySelector<HTMLElement>('.param-name');
    if (!nameEl) {
      Array.from(row.childNodes)
        .filter((node) => node.nodeType === Node.TEXT_NODE)
        .forEach((node) => row.removeChild(node));
      nameEl = document.createElement('span');
      nameEl.className = 'param-name';
      nameEl.textContent = labelText;
      row.insertBefore(nameEl, input);
    } else {
      nameEl.textContent = labelText;
    }

    if (!(input as HTMLElement).dataset?.paramVisibilityBound) {
      (input as HTMLElement).dataset.paramVisibilityBound = 'true';
      (input as HTMLElement).addEventListener('input', () => {
        if (block) callUpdate(block);
      });
      if ((input as HTMLInputElement).type === 'checkbox') {
        (input as HTMLInputElement).addEventListener('change', () => {
          if (block) callUpdate(block);
        });
      }
    }

    if (!(input as HTMLElement).dataset?.paramValidationBound) {
      (input as HTMLElement).dataset.paramValidationBound = 'true';
      if ((input as HTMLInputElement).type === 'number' || input.hasAttribute('step')) {
        (input as HTMLInputElement).addEventListener('blur', () => {
          const v = (input as HTMLInputElement).value;
          const num = Number(v);
          if (!Number.isFinite(num)) {
            (input as HTMLElement).classList.add('input-error');
            (input as HTMLElement).title = 'Invalid number';
          } else {
            (input as HTMLElement).classList.remove('input-error');
            (input as HTMLElement).title = '';
          }
        });
      }
    }

    const updateState = () => {
      row.classList.toggle('param-visible', Boolean(visible[key]));
    };
    updateState();

    if (!row.dataset.paramVisibilityBound) {
      row.dataset.paramVisibilityBound = 'true';
      row.addEventListener('click', (event) => {
        const target = event.target as Element | null;
        if (!target) return;
        if (target.closest('input, select, textarea, button, a')) return;
        visible[key] = !visible[key];
        updateState();
        if (block) callUpdate(block);
      });
    }
  });
}
/** Propagate data-edit values as name attributes so browsers can identify
 * form fields and DevTools autofill warnings are suppressed. */
export function applyDataEditNames(container: HTMLElement | null, idPrefix?: string): void {
  if (!container) return;
  container
    .querySelectorAll<HTMLElement>('input[data-edit], select[data-edit], textarea[data-edit]')
    .forEach((el, idx) => {
      const editName = el.getAttribute('data-edit') || `field${idx}`;
      if (!el.getAttribute('name')) {
        el.setAttribute('name', editName);
      }
      if (!el.getAttribute('id')) {
        const safePrefix = idPrefix ? String(idPrefix).replace(/[^a-zA-Z0-9_-]/g, '') : 'inspector';
        el.setAttribute('id', `${safePrefix}-${editName}-${idx}`);
      }
    });
}
//=============================================================================
export type FieldDef = {
  key: string;
  label: string;
  inputType?: 'text' | 'number' | 'checkbox' | 'select' | 'color' | 'textarea';
  attrs?: Record<string, string | number>;
  options?: Array<{ value: string; label: string }>;
  event?: 'input' | 'change';
  coerce?: (el: HTMLInputElement | HTMLSelectElement) => unknown;
  callUpdate?: boolean;
};
/**
 * Bind collapsible toggles for fieldsets whose legend matches `targetNames`.
 * Stores visibility in `block.params._visible[visKey]` where `visKey` is legend text lowercased.
 */
export function setupFieldsetToggles(
  block: Block | null,
  inspectorBody: HTMLElement | null,
  callUpdate: (b: Block) => void,
  targetNames?: string[]
): void {
  if (!block || !inspectorBody) return;
  const names = targetNames || ['Display', 'Appearance', 'Advanced', 'Labels'];
  try {
    const allFieldsets = Array.from(
      inspectorBody.querySelectorAll<HTMLFieldSetElement>('fieldset.param-group')
    ) as HTMLFieldSetElement[];
    allFieldsets.forEach((fs) => {
      if (fs.dataset.toggleBound === 'true') return;
      const legend = fs.querySelector<HTMLElement>('legend');
      if (!legend) return;
      const chevronEl = legend.querySelector<HTMLElement>('.collapse-chevron');
      let labelText = (legend.textContent || '').trim();
      if (chevronEl) labelText = labelText.replace((chevronEl.textContent || '').trim(), '').trim();
      if (!names.includes(labelText)) return;

      fs.dataset.toggleBound = 'true';
      if (!chevronEl) {
        const span = document.createElement('span');
        span.className = 'collapse-chevron';
        span.setAttribute('aria-hidden', 'true');
        legend.insertBefore(span, legend.firstChild);
      }

      const visKey = labelText.toLowerCase();
      const blockVisible =
        (block.params._visible as Record<string, unknown> | undefined)?.[visKey] === true;
      const visible = readInspectorSectionOpen(`group-${visKey}`, blockVisible);
      if (visible) {
        fs.classList.remove('collapsed');
        Array.from(fs.children as unknown as Element[]).forEach((c) => {
          if ((c as HTMLElement).tagName.toLowerCase() === 'legend') return;
          (c as HTMLElement).style.display = '';
        });
      } else {
        fs.classList.add('collapsed');
        Array.from(fs.children as unknown as Element[]).forEach((c) => {
          if ((c as HTMLElement).tagName.toLowerCase() === 'legend') return;
          (c as HTMLElement).style.display = 'none';
        });
      }

      legend.setAttribute('role', 'button');
      legend.setAttribute('tabindex', '0');
      legend.setAttribute('aria-expanded', String(visible));

      const toggle = () => {
        const nowCollapsed = fs.classList.toggle('collapsed');
        Array.from(fs.children as unknown as Element[]).forEach((c) => {
          if ((c as HTMLElement).tagName.toLowerCase() === 'legend') return;
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
    });
  } catch (e) {}
}
/** Binds the signals fieldset toggle and the per-signal inputs (calls `callUpdate`, and `renderScope` when given). */
export function setupSignalsCollapseAndBindings(
  block: Block | null,
  inspectorBody: HTMLElement | null,
  callUpdate: (b: Block) => void,
  renderScope?: (b: Block) => void
): void {
  if (!block || !inspectorBody) return;
  try {
    const signalsWrapper = inspectorBody.querySelector<HTMLDivElement>('.signals-wrapper');
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
          Array.from(signalsWrapper.children as unknown as Element[]).forEach(
            (c) => ((c as HTMLElement).style.display = '')
          );
        } else {
          signalsWrapper.classList.add('collapsed');
          Array.from(signalsWrapper.children as unknown as Element[]).forEach(
            (c) => ((c as HTMLElement).style.display = 'none')
          );
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
          legend.addEventListener('keydown', (evt: KeyboardEvent) => {
            if (evt.key === 'Enter' || evt.key === ' ') {
              evt.preventDefault();
              toggle();
            }
          });
        }
      }
    }
  } catch (e) {}

  try {
    inspectorBody.querySelectorAll<HTMLInputElement>('[data-signal-edit]').forEach((it) => {
      try {
        const idx = Number(it.getAttribute('data-signal-index'));
        const key = it.getAttribute('data-signal-edit');
        if (!key) return;
        const setSig = (k: string, v: string | number) => {
          if (!block.params) block.params = {} as Record<string, unknown>;
          if (!Array.isArray(block.params.signals)) block.params.signals = [];
          const arr = (block.params.signals as Record<string, unknown>[]) || [];
          const sig = arr[idx] || (arr[idx] = {});
          sig[k] = v;
          callUpdate(block);
          try {
            renderScope?.(block);
          } catch (e) {}
        };
        if (it.type === 'color') it.addEventListener('input', () => setSig(key, it.value));
        else if (it.type === 'number')
          it.addEventListener('input', () => setSig(key, Number(it.value)));
        else it.addEventListener('input', () => setSig(key, it.value));
      } catch (e) {}
    });
  } catch (e) {}
}
//=============================================================================
const esc = (v: unknown) =>
  String(v == null ? '' : v)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
export const fieldToHTML = (f: FieldDef, value: unknown) => {
  const attrs = f.attrs
    ? Object.entries(f.attrs)
        .map(([k, v]) => `${k}="${esc(v)}"`)
        .join(' ')
    : '';
  switch (f.inputType) {
    case 'textarea':
      return `<label class="param param-stack"><span class="param-name">${esc(
        f.label
      )}</span><textarea data-edit="${esc(f.key)}" ${attrs}>${esc(value)}</textarea></label>`;
    case 'checkbox':
      return `<div class="param param-inline"><span class="param-name">${esc(
        f.label
      )}</span><input type="checkbox" data-edit="${esc(f.key)}" ${value ? 'checked' : ''}></div>`;
    case 'select':
      return `<label class="param">${esc(f.label)}<select data-edit="${esc(f.key)}">${(
        f.options || []
      )
        .map(
          (o) =>
            `<option value="${esc(o.value)}" ${o.value === value ? 'selected' : ''}>${esc(
              o.label
            )}</option>`
        )
        .join('')}</select></label>`;
    case 'color':
      return `<label class="param">${esc(f.label)}<input type="color" data-edit="${esc(
        f.key
      )}" value="${esc(value)}" ${attrs}></label>`;
    case 'number':
      return `<label class="param">${esc(f.label)}<input type="number" data-edit="${esc(
        f.key
      )}" value="${esc(value)}" ${attrs}></label>`;
    default:
      return `<label class="param">${esc(f.label)}<input type="text" data-edit="${esc(
        f.key
      )}" value="${esc(value)}" ${attrs}></label>`;
  }
};
//=============================================================================
export const bindGenericInputs = (
  inspectorEl: HTMLElement | null,
  block: Block,
  deps: BlockPanelDeps,
  defs: FieldDef[]
) => {
  if (!inspectorEl) return;
  const callUpdate = deps.callUpdate;
  const defMap = new Map(defs.map((d) => [d.key, d]));

  inspectorEl
    .querySelectorAll<HTMLInputElement | HTMLSelectElement>('[data-edit]')
    .forEach((el) => {
      const key = (el.getAttribute('data-edit') || '').trim();
      if (!key) return;
      const def = defMap.get(key);
      const ev =
        def?.event ||
        (el.tagName === 'SELECT'
          ? 'change'
          : el.getAttribute('type') === 'checkbox'
          ? 'change'
          : 'input');
      const handler = () => {
        try {
          if (!block.params) block.params = {} as Record<string, unknown>;
          let v: unknown;
          if (def?.coerce) v = def.coerce(el as HTMLInputElement | HTMLSelectElement);
          else if (el instanceof HTMLInputElement && el.type === 'checkbox') v = el.checked;
          else v = (el as HTMLInputElement | HTMLSelectElement).value;
          (block.params as Record<string, unknown>)[key] = v;
          if (def?.callUpdate) callUpdate?.(block);
        } catch (e) {}
      };
      el.addEventListener(ev, handler);
    });

  // data-signal-edit bindings are handled by renderer-level helpers when needed.
};
//=============================================================================
