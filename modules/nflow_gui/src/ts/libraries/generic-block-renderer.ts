//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { Block, Helpers, CreateSvgElement, UiBlock } from './types.js';
import {
  RenderDescriptor,
  MathRenderDescriptor,
  PlotRenderDescriptor,
  ImageRenderDescriptor,
  ToggleRenderDescriptor,
  LibraryManifest,
  RenderColorOverrides,
} from './block-descriptor.js';
import { nflowRpc } from '../app/native-rpc.js';
import {
  getIcon,
  getIconSync,
  toExportsUrl,
  builtinSpritePending,
  type CachedIcon,
} from './icon-cache.js';
import BlockRegistry, { BlockDefinition } from '../app/block-registry.js';
import { scope as scopeRenderer } from './sink/scope.js';
import { xyScope as xyScopeRenderer } from './sink/xyScope.js';
import { xyzScope as xyzScopeRenderer } from './sink/xyzScope.js';
import { display as displayRenderer } from './sink/display.js';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
// Indirection: keeps `new URL(rel, base)` resolved at runtime instead of
// being rewritten by Vite's build-time asset analysis.
const MODULE_URL: string = import.meta.url;
// Escape LaTeX-special characters so the string renders literally inside `\mathtt{...}`
// or `\text{...}`. Without it an underscore in a variable or function name (e.g.
// `command_bode_out`) turns into a subscript.
function escapeLatexLiteral(value: string): string {
  return value.replace(/[\\{}$&#%_^~]/g, (ch) => {
    switch (ch) {
      case '\\':
        return '\\backslash ';
      case '~':
        return '\\textasciitilde ';
      case '^':
        return '\\textasciicircum ';
      default:
        return `\\${ch}`;
    }
  });
}
//=============================================================================
function interpolateFormula(
  formula: string,
  params: Record<string, unknown> | undefined,
  { escapeLatex = false } = {}
): string {
  if (!formula) return '';
  return formula.replace(/\{params\.([^}]+)\}/g, (_m, key) => {
    const p = params as Record<string, unknown> | undefined;
    const v = p ? p[key] : undefined;
    if (v === undefined || v === null) return '';
    const s = String(v);
    return escapeLatex ? escapeLatexLiteral(s) : s;
  });
}
//=============================================================================
// A `\mathtt{...}` / `\text{...}` label (variable name, function body) is a
// literal string, not math: it must be LaTeX-escaped when interpolated and
// shrunk to fit its block instead of clipping over the port.
function isLiteralLabelFormula(formula: string | undefined): boolean {
  return typeof formula === 'string' && /\\(?:mathtt|text|texttt)\s*\{/.test(formula);
}
//=============================================================================
function evaluateExpression(expr: string, vars: Record<string, number>): string {
  try {
    const fn = new Function(...Object.keys(vars), `return (${expr});`);
    const val = fn(...Object.values(vars));
    return String(val);
  } catch (e) {
    return '';
  }
}
//=============================================================================
type IconColors = { fill?: string; stroke?: string } | null;
const iconColorCache: Map<string, IconColors> = new Map();
// Apply icon-sampled fill/stroke to a block group: paints the body rect and
// sets the --block-* CSS variables. Module-scoped twin of the closure used by
// tryApplyIconColorsFor, so the synchronous cache path can reuse it.
function applyIconColorsToGroup(group: Element, colors: { fill?: string; stroke?: string }): void {
  const fill = colors.fill;
  const stroke = colors.stroke;
  if (!fill && !stroke) return;
  try {
    const body =
      (group as Element).querySelector('.nf-block') ||
      (group as Element).querySelector('rect') ||
      (group as Element).firstElementChild;
    if (body) {
      try {
        if (fill) (body as Element).setAttribute('fill', fill);
        if (stroke) (body as Element).setAttribute('stroke', stroke);
        const bAny = body as unknown as { style?: CSSStyleDeclaration };
        if (bAny && bAny.style) {
          if (fill) bAny.style.setProperty('fill', String(fill));
          if (stroke) bAny.style.setProperty('stroke', String(stroke));
        }
      } catch {
        // ignore
      }
    }
    const g = group as unknown as HTMLElement;
    if (fill) {
      g.style.setProperty('--block-fill', String(fill));
      g.style.setProperty('--block-fill-top', String(fill));
    }
    if (stroke) g.style.setProperty('--block-stroke', String(stroke));
  } catch {
    // ignore
  }
}
//=============================================================================
function kebabCase(s: string | undefined) {
  if (!s) return '';
  return String(s)
    .replace(/([a-z0-9])([A-Z])/g, '$1-$2')
    .toLowerCase();
}
//=============================================================================
async function tryApplyIconColorsFor(
  group: Element,
  blockType?: string,
  foundDir?: string
): Promise<void> {
  try {
    if (!blockType) {
      try {
        blockType = (group as HTMLElement).dataset?.blockType || undefined;
      } catch {}
    }
    if (!foundDir) {
      try {
        foundDir = (group as HTMLElement).dataset?.libDir || undefined;
      } catch {}
    }
    if (!foundDir || !blockType) return;

    const baseNames = [blockType, kebabCase(blockType)].filter(Boolean) as string[];
    const candidates: string[] = [];
    for (const base of baseNames) {
      candidates.push(`../libraries/${foundDir}/${base}.svg`);
      candidates.push(`../libraries/${foundDir}/exports/${base}.svg`);
    }

    const parser = typeof DOMParser !== 'undefined' ? new DOMParser() : null;

    const applyColorsToGroup = (fill?: string | null, stroke?: string | null): boolean => {
      try {
        const body = (group as Element).querySelector('.nf-block') as Element | null;
        const target =
          body || (group as Element).querySelector('rect') || (group as Element).firstElementChild;
        if (target) {
          try {
            if (fill) {
              try {
                (target as Element).setAttribute('fill', fill);
              } catch {}
            }
            if (stroke) {
              try {
                (target as Element).setAttribute('stroke', stroke);
              } catch {}
            }
            try {
              const tAny = target as unknown as { style?: CSSStyleDeclaration };
              if (tAny && tAny.style) {
                if (fill) tAny.style.setProperty('fill', String(fill));
                if (stroke) tAny.style.setProperty('stroke', String(stroke));
              } else {
                const prev = (target as Element).getAttribute('style') || '';
                const pieces: string[] = [];
                if (fill) pieces.push(`fill: ${fill};`);
                if (stroke) pieces.push(`stroke: ${stroke};`);
                if (pieces.length)
                  (target as Element).setAttribute('style', pieces.join(' ') + ' ' + prev);
              }
            } catch {}
          } catch {}
          try {
            const g = group as unknown as HTMLElement;
            if (fill) {
              g.style.setProperty('--block-fill', String(fill));
              g.style.setProperty('--block-fill-top', String(fill));
            }
            if (stroke) g.style.setProperty('--block-stroke', String(stroke));
          } catch {}
          return true;
        }
        try {
          const g = group as unknown as HTMLElement;
          if (fill) {
            g.style.setProperty('--block-fill', String(fill));
            g.style.setProperty('--block-fill-top', String(fill));
          }
          if (stroke) g.style.setProperty('--block-stroke', String(stroke));
          return true;
        } catch {}
        return false;
      } catch {
        return false;
      }
    };

    for (const rel of candidates) {
      try {
        const url = new URL(rel, MODULE_URL).href;
        if (iconColorCache.has(url)) {
          const cached = iconColorCache.get(url) || null;
          if (cached) {
            applyColorsToGroup(cached.fill ?? undefined, cached.stroke ?? undefined);
            return;
          }
          continue;
        }
        let resp: Response | null = null;
        try {
          resp = await fetch(url);
        } catch {
          iconColorCache.set(url, null);
          continue;
        }
        if (!resp || !resp.ok) {
          iconColorCache.set(url, null);
          continue;
        }
        const text = await resp.text();
        if (!text) {
          iconColorCache.set(url, null);
          continue;
        }
        if (!parser) {
          iconColorCache.set(url, null);
          continue;
        }
        const doc = parser.parseFromString(text, 'image/svg+xml');
        const svg = doc.querySelector('svg') || (doc as unknown as Element);
        if (!svg) {
          iconColorCache.set(url, null);
          continue;
        }

        let fill: string | null = null;
        let stroke: string | null = null;

        try {
          const f = svg.getAttribute('fill');
          const s = svg.getAttribute('stroke');
          if (f && f !== 'none') fill = f;
          if (s && s !== 'none') stroke = s;
        } catch {}

        if (!fill) {
          const el = doc.querySelector(
            'path[fill], rect[fill], circle[fill], ellipse[fill], polygon[fill], polyline[fill]'
          );
          if (el) {
            try {
              const f = el.getAttribute('fill');
              if (f && f !== 'none') fill = f;
            } catch {}
          }
        }
        if (!stroke) {
          const el = doc.querySelector(
            'path[stroke], rect[stroke], circle[stroke], ellipse[stroke], polygon[stroke], polyline[stroke]'
          );
          if (el) {
            try {
              const s = el.getAttribute('stroke');
              if (s && s !== 'none') stroke = s;
            } catch {}
          }
        }

        if ((!fill || !stroke) && doc.querySelectorAll) {
          const els = Array.from(doc.querySelectorAll('path,rect,circle,ellipse,polygon,polyline'));
          for (const e of els) {
            if (!fill && e.getAttribute) {
              const f = e.getAttribute('fill') || '';
              if (f && f !== 'none') {
                fill = f;
              } else {
                const style = e.getAttribute('style') || '';
                const m = style.match(/fill:\s*([^;]+)/);
                if (m && m[1] && m[1] !== 'none') fill = m[1].trim();
              }
            }
            if (!stroke && e.getAttribute) {
              const s = e.getAttribute('stroke') || '';
              if (s && s !== 'none') {
                stroke = s;
              } else {
                const style = e.getAttribute('style') || '';
                const m2 = style.match(/stroke:\s*([^;]+)/);
                if (m2 && m2[1] && m2[1] !== 'none') stroke = m2[1].trim();
              }
            }
            if (fill && stroke) break;
          }
        }

        const colors: IconColors = { fill: fill ?? undefined, stroke: stroke ?? undefined };
        iconColorCache.set(url, colors.fill || colors.stroke ? colors : null);
        if (colors.fill || colors.stroke) {
          applyColorsToGroup(colors.fill ?? undefined, colors.stroke ?? undefined);
          return;
        }
      } catch {
        continue;
      }
    }
  } catch {
    // swallow errors
  }
}
//=============================================================================
function applyDescriptorColors(
  group: Element,
  desc: { backgroundColor?: string; borderColor?: string; textColor?: string }
) {
  try {
    const gEl = group as unknown as HTMLElement;
    const bg = desc.backgroundColor;
    const border = desc.borderColor;
    const text = desc.textColor;

    if (bg) {
      try {
        const body = (group as Element).querySelector('.nf-block') as Element | null;
        if (body) {
          try {
            body.setAttribute('fill', String(bg));
          } catch {}
          try {
            (body as unknown as HTMLElement).style.setProperty('fill', String(bg));
          } catch {}
        }
        try {
          gEl.style.setProperty('--block-fill', String(bg));
          gEl.style.setProperty('--block-fill-top', String(bg));
        } catch {}
      } catch {}
    }
    if (border) {
      try {
        const body = (group as Element).querySelector('.nf-block') as Element | null;
        if (body) {
          try {
            body.setAttribute('stroke', String(border));
          } catch {}
          try {
            (body as unknown as HTMLElement).style.setProperty('stroke', String(border));
          } catch {}
        }
        try {
          gEl.style.setProperty('--block-stroke', String(border));
        } catch {}
      } catch {}
    }
    if (text) {
      try {
        try {
          gEl.style.setProperty('--block-text-color', String(text));
        } catch {}
        const texts = Array.from((group as Element).querySelectorAll('text')) as Element[];
        for (const t of texts) {
          try {
            t.setAttribute('fill', String(text));
          } catch {}
          try {
            (t as unknown as HTMLElement).style.setProperty('fill', String(text));
          } catch {}
        }
      } catch {}
    }
  } catch {}
}
//=============================================================================
function clockDisplayEnabled(block: Block): boolean {
  const params = (block.params || {}) as Record<string, unknown>;
  const dt = params[FieldNames.kDisplayTime];
  return dt === true || dt === 1 || dt === 'true';
}
//=============================================================================
function formatClockDisplayTime(value: unknown): string {
  const n = Number(value);
  if (!Number.isFinite(n)) return '0';
  if (Math.abs(n) >= 1000 || (Math.abs(n) > 0 && Math.abs(n) < 0.001)) {
    return n.toExponential(2);
  }
  const fixed = n.toFixed(3).replace(/\.?0+$/, '');
  return fixed === '-0' || fixed === '' ? '0' : fixed;
}
//=============================================================================
function renderClockDisplayOverlay(
  block: Block,
  group: Element,
  createEl: (
    tag: string,
    attrs?: Record<string, string | number | boolean | null>,
    content?: string | null
  ) => Element,
  width: number,
  height: number
) {
  if ((block as { type?: unknown }).type !== 'clock') return;

  const uiBlock = block as Block & {
    clockDisplayEl?: Element | null;
    clockDisplayTime?: number;
  };
  const existing = group.querySelector('text.clock-time-display');
  const showTime = clockDisplayEnabled(block);

  const y = Math.max(18, Math.min(height - 12, Math.round(height * 0.73)));
  let textEl = existing;
  if (!textEl) {
    textEl = createEl('text', {
      class: 'clock-time-display',
      x: Math.round(width / 2),
      y,
      'text-anchor': 'middle',
      'dominant-baseline': 'middle',
      'font-family': 'sans-serif',
      'font-size': '12',
      'font-weight': '500',
      fill: '#633806',
    });
    try {
      (textEl as HTMLElement).style.setProperty('pointer-events', 'none');
    } catch {}
    group.appendChild(textEl);
  }

  textEl.textContent = showTime ? formatClockDisplayTime(uiBlock.clockDisplayTime ?? 0) : 'Clock';
  uiBlock.clockDisplayEl = textEl;
}
//=============================================================================
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
//=============================================================================
function renderLogicOperatorOverlay(
  block: Block,
  group: Element,
  createEl: (
    tag: string,
    attrs?: Record<string, string | number | boolean | null>,
    content?: string | null
  ) => Element,
  width: number,
  height: number
) {
  const blockType = String((block as { type?: unknown }).type ?? '');
  const fallback =
    blockType === 'compareToZero'
      ? '!='
      : blockType === 'compareToConstant' || blockType === 'relationalOperator'
      ? '>='
      : '';
  if (!fallback) return;

  const params = (block.params || {}) as Record<string, unknown>;
  const op = operatorLabel(params[FieldNames.kOperator] ?? params[FieldNames.kRelop], fallback);
  const text =
    blockType === 'compareToZero'
      ? `u ${op} 0`
      : blockType === 'relationalOperator'
      ? `u1 ${op} u2`
      : `u ${op} c`;
  const x = Math.round(width / 2);
  const y = Math.round(height * 0.48);

  let cover = group.querySelector('rect.logic-expression-cover');
  if (!cover) {
    cover = createEl('rect', {
      class: 'logic-expression-cover',
      x: Math.round(width * 0.15),
      y: y - 13,
      width: Math.round(width * 0.7),
      height: 24,
      rx: 4,
      fill: '#F1EFE8',
    });
    try {
      (cover as HTMLElement).style.setProperty('pointer-events', 'none');
    } catch {}
    group.appendChild(cover);
  }

  let textEl = group.querySelector('text.logic-expression-display');
  if (!textEl) {
    textEl = createEl('text', {
      class: 'logic-expression-display',
      x,
      y,
      'text-anchor': 'middle',
      'dominant-baseline': 'middle',
      'font-family': 'sans-serif',
      'font-size': blockType === 'relationalOperator' ? '14' : '15',
      'font-weight': '500',
      fill: '#444441',
    });
    try {
      (textEl as HTMLElement).style.setProperty('pointer-events', 'none');
    } catch {}
    group.appendChild(textEl);
  }
  textEl.textContent = text;
}
//=============================================================================
// Primary defining parameter (+ unit) per acausal component type, used to print
// a "value" line under the icon (e.g. a resistor shows its resistance). Types
// not listed show their name with no value line.
const ACAUSAL_VALUE_PARAM: Record<string, [string, string]> = {
  Resistor: ['R', 'Ω'],
  Conductor: ['G', 'S'],
  Capacitor: ['C', 'F'],
  Inductor: ['L', 'H'],
  ConstantVoltage: ['V', 'V'],
  SineVoltage: ['Amplitude', 'V'],
  ConstantCurrent: ['I', 'A'],
  SineCurrent: ['Amplitude', 'A'],
};
//=============================================================================
// Compact engineering formatting (SI prefix, 3 significant digits) so component
// values read naturally: 1e-6 F -> "1 uF", 1000 -> "1 k".
function formatEngineeringValue(value: number, unit: string): string {
  if (!Number.isFinite(value)) return '';
  if (value === 0) return `0 ${unit}`;
  const abs = Math.abs(value);
  const prefixes: Array<[number, string]> = [
    [1e9, 'G'],
    [1e6, 'M'],
    [1e3, 'k'],
    [1, ''],
    [1e-3, 'm'],
    [1e-6, 'µ'],
    [1e-9, 'n'],
    [1e-12, 'p'],
  ];
  let factor = 1;
  let prefix = '';
  for (const [f, p] of prefixes) {
    if (abs >= f) {
      factor = f;
      prefix = p;
      break;
    }
  }
  const scaled = parseFloat((value / factor).toPrecision(3));
  return `${scaled} ${prefix}${unit}`.replace('  ', ' ');
}
//=============================================================================
function acausalCaptionParts(block: Block): { name: string; value: string } {
  const params = (block.params || {}) as Record<string, unknown>;
  const name = String(params[FieldNames.kName] ?? params[FieldNames.kLabel] ?? '').trim();
  let value = '';
  const map = ACAUSAL_VALUE_PARAM[String((block as { type?: unknown }).type ?? '')];
  if (map) {
    const raw = params[map[0]];
    const num = Number(raw);
    if (raw !== undefined && raw !== '' && Number.isFinite(num))
      value = formatEngineeringValue(num, map[1]);
  }
  return { name, value };
}
/**
 * Position an acausal block's name/value caption directly below the block's
 * on-screen bounding box, upright. The block group is rotated as a whole, so the
 * caption carries a counter-rotation (rotate(-angle)) to stay horizontal; the
 * vertical offset uses the on-screen half-height (which is bw/2 for a 90/270
 * rotation). Exported so the transform handler can refresh it when a block is
 * rotated after creation.
 */
export function positionAcausalCaption(
  block: Block,
  captionGroup: Element,
  bw: number,
  bh: number
): void {
  const angle = (((Number((block as { rotation?: unknown }).rotation) || 0) % 360) + 360) % 360;
  const cx = bw / 2;
  const cy = bh / 2;
  const horizontal = angle % 180 === 0;
  // On-screen half extents (a 90/270 rotation swaps width and height).
  const halfX = horizontal ? bw / 2 : bh / 2;
  const halfY = horizontal ? bh / 2 : bw / 2;
  captionGroup.setAttribute('transform', `rotate(${-angle} ${cx} ${cy})`);
  const text = captionGroup.querySelector('text.acausal-caption-text');
  if (!text) return;
  const tspans = Array.from(text.querySelectorAll('tspan'));
  if (horizontal) {
    const x = cx;
    text.setAttribute('text-anchor', 'middle');
    text.setAttribute('x', String(x));
    text.setAttribute('y', String(cy + halfY + 15));
    tspans.forEach((t, i) => {
      t.setAttribute('x', String(x));
      t.setAttribute('dy', String(i === 0 ? 0 : 12));
    });
  } else {
    // A vertical component (source/shunt/sensor) fills the rail-to-return band, so there
    // is no room below it. Put the label to the right, vertically centred.
    const x = cx + halfX + 8;
    text.setAttribute('text-anchor', 'start');
    text.setAttribute('x', String(x));
    text.setAttribute('y', String(cy - (tspans.length > 1 ? 4 : 0)));
    tspans.forEach((t, i) => {
      t.setAttribute('x', String(x));
      t.setAttribute('dy', String(i === 0 ? 0 : 12));
    });
  }
}
//=============================================================================
export function renderAcausalCaptionOverlay(
  block: Block,
  group: Element,
  createEl: (
    tag: string,
    attrs?: Record<string, string | number | boolean | null>,
    content?: string | null
  ) => Element,
  bw: number,
  bh: number
) {
  if (!(block as { acausal?: unknown }).acausal) return;
  const { name, value } = acausalCaptionParts(block);
  let g = group.querySelector('g.acausal-caption');
  if (!name && !value) {
    if (g && g.parentNode) g.parentNode.removeChild(g);
    return;
  }
  if (!g) {
    g = createEl('g', { class: 'acausal-caption' });
    try {
      (g as HTMLElement).style.setProperty('pointer-events', 'none');
    } catch {}
    group.appendChild(g);
  } else {
    while (g.firstChild) g.removeChild(g.firstChild);
  }
  const cx = bw / 2;
  const text = createEl('text', {
    class: 'acausal-caption-text',
    x: cx,
    y: 0,
    'text-anchor': 'middle',
  });
  if (name) text.appendChild(createEl('tspan', { class: 'cap-name', x: cx, dy: 0 }, name));
  if (value)
    text.appendChild(createEl('tspan', { class: 'cap-value', x: cx, dy: name ? 12 : 0 }, value));
  g.appendChild(text);
  positionAcausalCaption(block, g, bw, bh);
}
//=============================================================================
export function createBlockRenderer(desc: RenderDescriptor, blockType?: string, foundDir?: string) {
  if (desc.type === 'toggle') {
    const td = desc as ToggleRenderDescriptor;
    return function (block: Block, helpers: Helpers) {
      const h = helpers as unknown as Partial<Helpers> & { createSvgElement?: CreateSvgElement };
      const group = block.group;

      const createEl = (
        tag: string,
        attrs?: Record<string, string | number | boolean | null>,
        content?: string | null
      ): Element & { dataset?: Record<string, string> } => {
        if (typeof h.createSvgElement === 'function') {
          if (content === undefined || content === null)
            return (h.createSvgElement as CreateSvgElement)(tag, attrs);
          return (h.createSvgElement as CreateSvgElement)(tag, attrs, content);
        }
        if (typeof document !== 'undefined' && typeof document.createElementNS === 'function') {
          const ns = 'http://www.w3.org/2000/svg';
          const el = document.createElementNS(ns, tag);
          if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
          if (content) el.textContent = content;
          return el as Element & { dataset?: Record<string, string> };
        }
        return {} as Element & { dataset?: Record<string, string> };
      };

      const bodyClass = td.bodyClass ?? 'nf-block toggle-block';
      if (helpers.svgRect) {
        group.appendChild(helpers.svgRect!(0, 0, block.width, block.height, bodyClass));
      } else {
        group.appendChild(
          createEl('rect', {
            x: 0,
            y: 0,
            width: block.width,
            height: block.height,
            class: bodyClass,
          })
        );
      }

      const trackW = td.trackWidth ?? 56;
      const trackH = td.trackHeight ?? 22;
      const trackX = Math.max(4, Math.floor((block.width - trackW) / 2));
      const trackY = Math.max(6, Math.floor((block.height - trackH) / 2));

      const track = createEl('rect', {
        x: String(trackX),
        y: String(trackY),
        rx: String(Math.floor(trackH / 2)),
        ry: String(Math.floor(trackH / 2)),
        width: String(trackW),
        height: String(trackH),
        class: td.trackClass ?? 'toggle-track',
      });
      group.appendChild(track);

      const knobR = td.knobRadius ?? Math.floor(trackH / 2) - 2;
      const leftCx = trackX + knobR + 2;
      const rightCx = trackX + trackW - knobR - 2;
      const cy = trackY + Math.floor(trackH / 2);

      const params = block.params as Record<string, unknown> | undefined;
      const state = Number(params?.[FieldNames.kState] ?? 0) ? 1 : 0;

      const knob = createEl('circle', {
        cx: String(leftCx),
        cy: String(cy),
        r: String(knobR),
        class: td.knobClass ?? 'toggle-knob',
      });
      group.appendChild(knob);
      const knobEl = knob as unknown as SVGElement & { style: CSSStyleDeclaration };
      const knobDelta = rightCx - leftCx;
      try {
        knobEl.style.transition = 'transform 160ms cubic-bezier(.2,.9,.3,1)';
        knobEl.style.transformBox = 'fill-box';
        knobEl.style.transformOrigin = 'center';
        knobEl.style.transform = state ? `translate(${knobDelta}px, 0)` : 'translate(0, 0)';
      } catch (_e) {}

      const labelText = state
        ? String(params?.[FieldNames.kOnLabel] ?? 'ON')
        : String(params?.[FieldNames.kOffLabel] ?? 'OFF');
      const label = createEl(
        'text',
        {
          x: String(Math.floor(block.width / 2)),
          y: String(trackY + trackH + 14),
          'text-anchor': 'middle',
          class: td.labelClass ?? 'toggle-label',
        },
        labelText
      );
      group.appendChild(label);

      const uiBlock = block as UiBlock & {
        toggleKnob?: Element;
        toggleLabel?: Element;
        toggleTrack?: Element;
      };
      uiBlock.toggleKnob = knob;
      uiBlock.toggleLabel = label;
      uiBlock.toggleTrack = track;

      try {
        track.setAttribute('style', 'cursor: pointer; pointer-events: all');
        knob.setAttribute('style', 'cursor: pointer; pointer-events: all');
        (label as Element).setAttribute('style', 'cursor: pointer;');
      } catch (_e) {}

      function updateVisuals(newState: number) {
        const trackEl = (block.group as Element).querySelector(
          '.toggle-track'
        ) as SVGRectElement | null;
        if (trackEl) {
          const tx = Number(trackEl.getAttribute('x') ?? 0);
          const tw = Number(trackEl.getAttribute('width') ?? 0);
          const th = Number(trackEl.getAttribute('height') ?? 0);
          const kr = Math.floor(th / 2) - 2;
          const left = tx + kr + 2;
          const right = tx + tw - kr - 2;
          try {
            knobEl.style.transform = `translate(${(newState ? right : left) - left}px, 0)`;
          } catch (_e) {
            knob.setAttribute('cx', String(newState ? right : left));
          }
        } else {
          try {
            knobEl.style.transform = `translate(${newState ? knobDelta : 0}px, 0)`;
          } catch (_e) {
            knob.setAttribute('cx', String(newState ? rightCx : leftCx));
          }
        }
        label.textContent = newState
          ? String(params?.[FieldNames.kOnLabel] ?? 'ON')
          : String(params?.[FieldNames.kOffLabel] ?? 'OFF');
      }

      function setDataState(newState: number) {
        try {
          (group as Element).setAttribute('data-state', String(newState));
        } catch (_e) {}
      }

      function sendRuntimeEvent(newState: number) {
        try {
          const payload = JSON.stringify({
            type: 'setParam',
            id: block.id,
            key: FieldNames.kState,
            value: newState,
          });
          void nflowRpc('nflow.sim.event', { name: 'SimControl', data: payload });
        } catch (_err) {
          // ignore
        }
      }

      function toggleState() {
        const bp = (block.params || {}) as Record<string, unknown>;
        bp.state = bp.state ? 0 : 1;
        const newState = bp.state ? 1 : 0;
        updateVisuals(newState);
        sendRuntimeEvent(newState);
        setDataState(newState);
      }

      let downX = 0;
      let downY = 0;
      let downId: number | null = null;
      const onPointerDown = (ev: unknown) => {
        const pev = ev as PointerEvent;
        if (pev.button !== 0) return;
        const rect = (group as Element).getBoundingClientRect();
        if (
          pev.clientX < rect.left ||
          pev.clientX > rect.right ||
          pev.clientY < rect.top ||
          pev.clientY > rect.bottom
        )
          return;
        downX = pev.clientX;
        downY = pev.clientY;
        downId = pev.pointerId;

        const onUp = (upEvAny: unknown) => {
          const upEv = upEvAny as PointerEvent;
          if (upEv.pointerId !== downId) return;
          window.removeEventListener('pointerup', onUp as EventListener, true);
          window.removeEventListener('pointercancel', onCancel as EventListener, true);
          const dx = Math.abs(upEv.clientX - downX);
          const dy = Math.abs(upEv.clientY - downY);
          if (dx <= 6 && dy <= 6) {
            toggleState();
          }
          downId = null;
        };
        const onCancel = (_: unknown) => {
          window.removeEventListener('pointerup', onUp as EventListener, true);
          window.removeEventListener('pointercancel', onCancel as EventListener, true);
          downId = null;
        };
        window.addEventListener('pointerup', onUp as EventListener, true);
        window.addEventListener('pointercancel', onCancel as EventListener, true);
      };
      group.addEventListener('pointerdown', onPointerDown as EventListener, true);

      setTimeout(() => updateVisuals(state), 0);
      setDataState(state);
    };
  }

  if (desc.type === 'math') {
    const md = desc as MathRenderDescriptor;
    return function (block: Block, helpers: Helpers) {
      const h = helpers as unknown as Partial<Helpers> & {
        createSvgElement?: CreateSvgElement;
        buildTransferTeX?: (...args: unknown[]) => string;
      };
      const group = block.group;
      const width = typeof block.width === 'number' ? block.width : md.width ?? 80;
      const height = typeof block.height === 'number' ? block.height : md.height ?? 80;

      const createEl = (
        tag: string,
        attrs?: Record<string, string | number | boolean | null>,
        content?: string | null
      ): Element & { dataset?: Record<string, string> } => {
        if (typeof h.createSvgElement === 'function') {
          if (content === undefined || content === null)
            return (h.createSvgElement as CreateSvgElement)(tag, attrs);
          return (h.createSvgElement as CreateSvgElement)(tag, attrs, content);
        }
        if (typeof document !== 'undefined' && typeof document.createElementNS === 'function') {
          const ns = 'http://www.w3.org/2000/svg';
          const el = document.createElementNS(ns, tag);
          if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
          if (content) el.textContent = content;
          return el as Element & { dataset?: Record<string, string> };
        }
        return {} as Element & { dataset?: Record<string, string> };
      };

      const explicitWidth = block && typeof block.width === 'number' ? block.width : undefined;
      const shouldDrawRect =
        !!md.useRectElement ||
        !!helpers.svgRect ||
        (typeof explicitWidth === 'number' && explicitWidth >= 40);

      if (shouldDrawRect) {
        const klass = md.bodyClass || 'nf-block';
        if (helpers.svgRect) {
          group.appendChild(helpers.svgRect!(0, 0, width, height, klass));
        } else {
          group.appendChild(createEl('rect', { x: 0, y: 0, width, height, class: klass }));
        }
      }

      let formula = '';
      const params = block.params as Record<string, unknown> | undefined;
      const num = params ? params[FieldNames.kNum] : undefined;
      const den = params ? params[FieldNames.kDen] : undefined;
      if (typeof h.buildTransferTeX === 'function' && (num !== undefined || den !== undefined)) {
        try {
          const tfVar = md.transferVariable;
          formula = tfVar
            ? (h.buildTransferTeX as (...args: unknown[]) => string)(num, den, tfVar)
            : (h.buildTransferTeX as (...args: unknown[]) => string)(num, den);
        } catch (e) {
          formula = interpolateFormula(md.formula, params, {
            escapeLatex: isLiteralLabelFormula(md.formula),
          });
        }
      } else {
        formula = interpolateFormula(md.formula, params, {
          escapeLatex: isLiteralLabelFormula(md.formula),
        });
      }

      if (blockType === 'display' && typeof displayRenderer === 'function') {
        displayRenderer(block as UiBlock, helpers as Helpers);
        return;
      }
      if (blockType === 'display' && typeof displayRenderer === 'function') {
        displayRenderer(block as UiBlock, helpers as Helpers);
        return;
      }

      // Special-case legacy math shapes: gain, mult and sum use explicit SVG shapes/text
      if (blockType === 'gain') {
        try {
          const refW = 72;
          const refH = 72;
          const basePts: [number, number][] = [
            [6, 66],
            [6, 6],
            [66, 36],
          ];
          const sx = width / refW;
          const sy = height / refH;
          const points = basePts
            .map(
              ([x, y]) => `${Math.max(1, Math.round(x * sx))},${Math.max(1, Math.round(y * sy))}`
            )
            .join(' ');
          const tri = createEl('polygon', { points, class: 'gain-triangle' });
          group.appendChild(tri);

          const defaultBg = '#EEEDFE';
          const defaultStroke = '#AFA9EC';
          const defaultSymbol = '#3C3489';
          const mdOverrides = md as RenderColorOverrides | undefined;

          const triFill =
            mdOverrides && mdOverrides.backgroundColor
              ? String(mdOverrides.backgroundColor)
              : defaultBg;
          const triStroke =
            mdOverrides && mdOverrides.borderColor
              ? String(mdOverrides.borderColor)
              : defaultStroke;
          try {
            tri.setAttribute('fill', triFill);
          } catch {}
          try {
            (tri as unknown as HTMLElement).style.setProperty('fill', triFill);
          } catch {}
          try {
            tri.setAttribute('stroke', triStroke);
          } catch {}
          try {
            (tri as unknown as HTMLElement).style.setProperty('stroke', triStroke);
          } catch {}

          const mathClass = (md && md.mathGroupClass) || 'nf-gain-formula';
          const mathGroup = createEl('g', { class: mathClass });
          group.appendChild(mathGroup);
          const symbolColor =
            mdOverrides && mdOverrides.textColor ? String(mdOverrides.textColor) : defaultSymbol;
          try {
            (mathGroup as unknown as HTMLElement).style.setProperty(
              '--block-text-color',
              symbolColor
            );
          } catch {}

          try {
            const ts = (md as unknown as { textSize?: string | number }).textSize;
            if (ts !== undefined && ts !== null) {
              const sizeStr = typeof ts === 'number' ? `${ts}px` : String(ts);
              try {
                (mathGroup as Element).setAttribute('data-text-size', sizeStr);
              } catch {}
              try {
                const ds = (mathGroup as unknown as HTMLElement).dataset;
                if (ds) (ds as DOMStringMap & Record<string, string>).textSize = sizeStr;
              } catch {}
            }
          } catch {}

          if (helpers.renderTeXMath) helpers.renderTeXMath(mathGroup, formula, width, height);
        } catch {}
        // Make the background rect transparent so only the triangle shows
        try {
          const bodyRect = (group as Element).querySelector('.nf-block') as Element | null;
          if (bodyRect) {
            try {
              bodyRect.setAttribute('fill', 'none');
            } catch {}
            try {
              (bodyRect as unknown as HTMLElement).style.setProperty('fill', 'none');
            } catch {}
          }
        } catch {}
        // For gain we intentionally keep colours self-contained (do not call applyDescriptorColors)
        return;
      }

      if (formula === '\\otimes') {
        const cx = Math.floor(width / 2);
        const cy = Math.floor(height / 2);
        const r = Math.max(2, Math.floor(Math.min(width, height) / 2) - 2);
        group.appendChild(createEl('circle', { cx, cy, r, class: 'sum-circle' }));
        group.appendChild(
          createEl('line', {
            x1: 2,
            y1: 2,
            x2: Math.max(2, width - 2),
            y2: Math.max(2, height - 2),
            class: 'sum-line',
          })
        );
        group.appendChild(
          createEl('line', {
            x1: Math.max(2, width - 2),
            y1: 2,
            x2: 2,
            y2: Math.max(2, height - 2),
            class: 'sum-line',
          })
        );
        if (
          (md as RenderColorOverrides).backgroundColor ||
          (md as RenderColorOverrides).borderColor ||
          (md as RenderColorOverrides).textColor
        ) {
          applyDescriptorColors(group, md as RenderColorOverrides);
        } else {
          void tryApplyIconColorsFor(group, blockType, foundDir);
        }
        return;
      }

      if (formula === '\\oplus') {
        const cx = Math.floor(width / 2);
        const cy = Math.floor(height / 2);
        const r = Math.max(2, Math.floor(Math.min(width, height) / 2) - 2);
        group.appendChild(createEl('circle', { cx, cy, r, class: 'sum-circle' }));
        // central '+' symbol drawn as two lines to avoid font/rendering issues
        const crossHalf = Math.max(2, Math.floor(r * 0.5));
        const strokeW = Math.max(1, Math.round(Math.min(width, height) / 18));
        group.appendChild(
          createEl('line', {
            x1: String(cx - crossHalf),
            y1: String(cy),
            x2: String(cx + crossHalf),
            y2: String(cy),
            class: 'sum-line',
            'stroke-width': String(strokeW),
          })
        );
        group.appendChild(
          createEl('line', {
            x1: String(cx),
            y1: String(cy - crossHalf),
            x2: String(cx),
            y2: String(cy + crossHalf),
            class: 'sum-line',
            'stroke-width': String(strokeW),
          })
        );
        if (
          (md as RenderColorOverrides).backgroundColor ||
          (md as RenderColorOverrides).borderColor ||
          (md as RenderColorOverrides).textColor
        ) {
          applyDescriptorColors(group, md as RenderColorOverrides);
        } else {
          void tryApplyIconColorsFor(group, blockType, foundDir);
        }
        return;
      }

      const isLabelMath =
        (md && md.mathGroupClass && String(md.mathGroupClass).includes('label')) ||
        (blockType && String(blockType).toLowerCase().includes('label'));

      if (isLabelMath && helpers.renderLabelNode) {
        try {
          const params = block.params as Record<string, unknown> | undefined;
          const label =
            (params &&
              ((params[FieldNames.kGotoTag] as string | undefined) ??
                (params['name'] as string | undefined))) ??
            formula;
          const showNode = params?.['showNode'] !== false;
          helpers.renderLabelNode(block, label, { showNode });
        } catch {}
      } else {
        const mathGroup = createEl('g', { class: md.mathGroupClass || 'nf-block-formula' });
        group.appendChild(mathGroup);

        // A multi-word display name (e.g. "Bus Creator") renders as centered plain text, one
        // word per line, so it fits a compact block instead of clipping. Single tokens and
        // real math keep the TeX path below.
        const nameStr = String(formula ?? '').trim();
        const words = nameStr.split(/\s+/).filter(Boolean);
        const isPlainName = nameStr.length > 0 && /^[A-Za-z0-9 ]+$/.test(nameStr);
        const isWrappableName = words.length > 1 && isPlainName;
        // A single long token (e.g. "Concatenate") has no wrap points: it gets a readable
        // base size and shrinks to fit. Real TeX formulas keep their own sizing, so blocks
        // like tf/pid/gain/constant are unchanged.
        const isSingleTokenLabel = words.length === 1 && isPlainName;
        if (isWrappableName) {
          const lineH = Math.max(11, Math.min(15, Math.floor(height / (words.length + 0.6))));
          const startY = height / 2 - ((words.length - 1) * lineH) / 2;
          const textEl = createEl('text', {
            x: width / 2,
            y: startY,
            'text-anchor': 'middle',
            'dominant-baseline': 'central',
            'font-size': `${Math.min(13, lineH)}px`,
            fill: 'var(--block-text-color, currentColor)',
            class: 'block-name-label',
          });
          words.forEach((w, i) => {
            textEl.appendChild(createEl('tspan', { x: width / 2, dy: i === 0 ? 0 : lineH }, w));
          });
          mathGroup.appendChild(textEl);
          if (
            (md as RenderColorOverrides).backgroundColor ||
            (md as RenderColorOverrides).borderColor ||
            (md as RenderColorOverrides).textColor
          ) {
            applyDescriptorColors(group, md as RenderColorOverrides);
          } else {
            void tryApplyIconColorsFor(group, blockType, foundDir);
          }
          return;
        }

        // Per-descriptor text size if provided; otherwise a plain single-token label gets a
        // readable base size (the .math-foreign default of 40px is too large for a compact block).
        try {
          const ts = (md as unknown as { textSize?: string | number }).textSize;
          let sizeStr: string | undefined;
          if (ts !== undefined && ts !== null) {
            sizeStr = typeof ts === 'number' ? `${ts}px` : String(ts);
          } else if (isSingleTokenLabel) {
            sizeStr = '13px';
          }
          if (sizeStr) {
            try {
              (mathGroup as Element).setAttribute('data-text-size', sizeStr);
            } catch {}
            try {
              const ds = (mathGroup as unknown as HTMLElement).dataset;
              if (ds) (ds as DOMStringMap & Record<string, string>).textSize = sizeStr;
            } catch {}
          }
        } catch {}

        // Mark single-token and literal string labels so KaTeX shrinks them to fit after
        // rendering, instead of clipping over the port.
        if (isSingleTokenLabel || isLiteralLabelFormula(md.formula)) {
          try {
            (mathGroup as Element).setAttribute('data-fit-text', '1');
          } catch {}
        }

        if (helpers.renderTeXMath) helpers.renderTeXMath(mathGroup, formula, width, height);
      }
      if (
        (md as RenderColorOverrides).backgroundColor ||
        (md as RenderColorOverrides).borderColor ||
        (md as RenderColorOverrides).textColor
      ) {
        applyDescriptorColors(group, md as RenderColorOverrides);
      } else {
        void tryApplyIconColorsFor(group, blockType, foundDir);
      }
    };
  }

  if (desc.type === 'plot') {
    const pd = desc as PlotRenderDescriptor;
    return function (block: Block, helpers: Helpers) {
      if (blockType === 'scope' && typeof scopeRenderer === 'function') {
        scopeRenderer(block as UiBlock, helpers as Helpers);
        return;
      }
      if (blockType === 'xyScope' && typeof xyScopeRenderer === 'function') {
        xyScopeRenderer(block as UiBlock, helpers as Helpers);
        return;
      }
      if (blockType === 'xyzScope' && typeof xyzScopeRenderer === 'function') {
        xyzScopeRenderer(block as UiBlock, helpers as Helpers);
        return;
      }
      const h = helpers as unknown as Partial<Helpers> & { createSvgElement?: CreateSvgElement };
      const group = block.group;
      const width = block.width || 80;
      const height = block.height || 80;

      const createEl = (
        tag: string,
        attrs?: Record<string, string | number | boolean | null>,
        content?: string | null
      ): Element & { dataset?: Record<string, string> } => {
        if (typeof h.createSvgElement === 'function') {
          if (content === undefined || content === null)
            return (h.createSvgElement as CreateSvgElement)(tag, attrs);
          return (h.createSvgElement as CreateSvgElement)(tag, attrs, content);
        }
        if (typeof document !== 'undefined' && typeof document.createElementNS === 'function') {
          const ns = 'http://www.w3.org/2000/svg';
          const el = document.createElementNS(ns, tag);
          if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
          if (content) el.textContent = content;
          return el as Element & { dataset?: Record<string, string> };
        }
        const el =
          typeof document !== 'undefined' && typeof document.createElement === 'function'
            ? document.createElement('div')
            : ({} as Element);
        return el as Element & { dataset?: Record<string, string> };
      };

      if (helpers.svgRect) {
        group.appendChild(helpers.svgRect!(0, 0, width, height, 'nf-block'));
      } else if (typeof h.createSvgElement === 'function') {
        group.appendChild(createEl('rect', { x: 0, y: 0, width, height, class: 'nf-block' }));
      } else if (
        typeof document !== 'undefined' &&
        typeof document.createElementNS === 'function'
      ) {
        const ns = 'http://www.w3.org/2000/svg';
        const r = document.createElementNS(ns, 'rect');
        r.setAttribute('x', '0');
        r.setAttribute('y', '0');
        r.setAttribute('width', String(width));
        r.setAttribute('height', String(height));
        r.setAttribute('class', 'nf-block');
        group.appendChild(r);
      }

      const axisX = 14;
      const axisTop = 14;
      const axisY = height - 16;
      const axisRight = width - 14;
      const midY = (axisY + axisTop) / 2;
      const amp = (axisY - axisTop) / 3;
      const w = axisRight - axisX - 4;

      const vars: Record<string, number> = {
        axisX,
        axisY,
        axisTop,
        axisRight,
        midY,
        amp,
        w,
        width,
        height,
      };

      const path = pd.path.replace(/\{([^}]+)\}/g, (_m, expr) => {
        return evaluateExpression(expr, vars);
      });

      if (helpers.renderSourcePlot) {
        helpers.renderSourcePlot!(group, width, height, path);
      } else if (typeof h.createSvgElement === 'function') {
        const pathEl = createEl('path', { d: path, class: 'block-plot' });
        group.appendChild(pathEl);
      }
      if (
        (pd as RenderColorOverrides).backgroundColor ||
        (pd as RenderColorOverrides).borderColor ||
        (pd as RenderColorOverrides).textColor
      ) {
        applyDescriptorColors(group, pd as RenderColorOverrides);
      } else {
        void tryApplyIconColorsFor(group, blockType, foundDir);
      }
    };
  }

  if (desc.type === 'image') {
    const id = desc as ImageRenderDescriptor;
    return function (block: Block, helpers: Helpers) {
      const h = helpers as unknown as Partial<Helpers> & { createSvgElement?: CreateSvgElement };
      const group = block.group as Element;
      const bw = block.width || 80;
      const bh = block.height || 80;

      const createEl = (
        tag: string,
        attrs?: Record<string, string | number | boolean | null>,
        content?: string | null
      ): Element & { dataset?: Record<string, string> } => {
        if (typeof h.createSvgElement === 'function') {
          if (content === undefined || content === null)
            return (h.createSvgElement as CreateSvgElement)(tag, attrs);
          return (h.createSvgElement as CreateSvgElement)(tag, attrs, content);
        }
        if (typeof document !== 'undefined' && typeof document.createElementNS === 'function') {
          const ns = 'http://www.w3.org/2000/svg';
          const el = document.createElementNS(ns, tag);
          if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
          if (content) el.textContent = content;
          return el as Element & { dataset?: Record<string, string> };
        }
        return {} as Element & { dataset?: Record<string, string> };
      };

      let bodyRect = (group as Element).querySelector('rect.nf-block') as Element | null;
      if (!bodyRect) {
        const cls = id.bodyClass ?? 'nf-block';
        if (helpers.svgRect) {
          try {
            bodyRect = helpers.svgRect!(0, 0, bw, bh, cls);
          } catch {
            bodyRect = null;
          }
        }
        if (!bodyRect) {
          try {
            bodyRect = createEl('rect', { x: 0, y: 0, width: bw, height: bh, class: cls });
          } catch {
            bodyRect = null;
          }
        }
        if (bodyRect) {
          // insert body rect as the first child so other content paints above it
          try {
            const first = (group as Element).firstChild;
            if (first) (group as Element).insertBefore(bodyRect, first);
            else (group as Element).appendChild(bodyRect);
          } catch {
            try {
              (group as Element).appendChild(bodyRect);
            } catch {}
          }
        }
      }

      // Create an explicit image layer so images render above the body rect
      let imageLayer = (group as Element).querySelector('g.block-image-layer') as Element | null;
      if (!imageLayer) {
        try {
          imageLayer = createEl('g', { class: 'block-image-layer' });
          // ensure pointer events pass through to the block
          try {
            (imageLayer as HTMLElement).style.setProperty('pointer-events', 'none');
          } catch {}
          (group as Element).appendChild(imageLayer);
        } catch {}
      }

      const x = typeof id.x === 'number' ? id.x : 0;
      const y = typeof id.y === 'number' ? id.y : 0;
      const w = typeof id.width === 'number' ? id.width : Math.max(0, bw - x);
      const hgt = typeof id.height === 'number' ? id.height : Math.max(0, bh - y);

      const src = id._src && id._src.length ? id._src : id.src;
      if (!src) return;

      const lower = src.toLowerCase().split('?')[0];
      const isDataUri = /^data:/i.test(src);
      const isSvgFile = !isDataUri && lower.endsWith('.svg');

      const hasDescriptorColors = Boolean(
        (id as RenderColorOverrides).backgroundColor ||
          (id as RenderColorOverrides).borderColor ||
          (id as RenderColorOverrides).textColor
      );
      const applyColors = (iconColors?: { fill?: string; stroke?: string } | null) => {
        if (hasDescriptorColors) {
          applyDescriptorColors(group, id as RenderColorOverrides);
        } else if (iconColors && (iconColors.fill || iconColors.stroke)) {
          applyIconColorsToGroup(group, iconColors);
        }
      };
      const finishOverlays = () => {
        renderClockDisplayOverlay(block, group, createEl, bw, bh);
        renderLogicOperatorOverlay(block, group, createEl, bw, bh);
        renderAcausalCaptionOverlay(block, group, createEl, bw, bh);
      };

      const appendImageEl = () => {
        const img = createEl('image', {
          href: src,
          x: String(x),
          y: String(y),
          width: String(w),
          height: String(hgt),
          preserveAspectRatio: id.preserveAspectRatio ?? 'xMidYMid meet',
        });
        try {
          const imgNs = img as unknown as {
            setAttributeNS?: (ns: string, name: string, value: string) => void;
          };
          if (typeof imgNs.setAttributeNS === 'function') {
            imgNs.setAttributeNS('http://www.w3.org/1999/xlink', 'href', src);
          }
        } catch {}
        try {
          if (imageLayer) imageLayer.appendChild(img);
          else group.appendChild(img);
          try {
            (img as HTMLElement).style.setProperty('pointer-events', 'none');
          } catch {}
        } catch {
          try {
            group.appendChild(img);
          } catch {}
        }
      };

      // SVG icon files: inline the parsed <svg> from the shared icon cache. A cached
      // icon paints in the same frame as the block body, and its colors come from the
      // same fetch, so no separate color-probe request is issued.
      if (isSvgFile) {
        const injectFrom = (cached: CachedIcon | null | undefined): boolean => {
          if (!cached || !cached.svgEl) return false;
          try {
            const clone = document.importNode(cached.svgEl, true) as SVGSVGElement;
            const hadViewBox = Boolean(clone.getAttribute('viewBox'));
            const ow = parseFloat(cached.svgEl.getAttribute('width') || '');
            const oh = parseFloat(cached.svgEl.getAttribute('height') || '');
            clone.setAttribute('x', String(x));
            clone.setAttribute('y', String(y));
            clone.setAttribute('width', String(w));
            clone.setAttribute('height', String(hgt));
            clone.setAttribute('preserveAspectRatio', id.preserveAspectRatio ?? 'xMidYMid meet');
            // Preserve intrinsic scaling: if the icon had no viewBox, synthesize
            // one from its intrinsic size so the nested <svg> scales like <image>.
            if (!hadViewBox && isFinite(ow) && isFinite(oh) && ow > 0 && oh > 0) {
              clone.setAttribute('viewBox', `0 0 ${ow} ${oh}`);
            }
            try {
              (clone as unknown as { style?: CSSStyleDeclaration }).style?.setProperty(
                'pointer-events',
                'none'
              );
            } catch {}
            if (imageLayer) imageLayer.appendChild(clone);
            else group.appendChild(clone);
          } catch {
            return false;
          }
          applyColors(cached.colors);
          finishOverlays();
          return true;
        };

        const cacheUrl = toExportsUrl(src);
        const sync = getIconSync(cacheUrl);
        if (sync && sync.svgEl && injectFrom(sync)) return;

        const mode = id.svgMode || 'element';

        // While the builtin sprite is loading or loaded, resolve through the cache and inject
        // inline. A throwaway <image> fetch would pop in late on WebView2. Also covers
        // svgMode:'inline'.
        if (mode === 'inline' || builtinSpritePending()) {
          void getIcon(cacheUrl).then((cached) => {
            if (injectFrom(cached)) return;
            // Sprite/fetch miss (e.g. an external icon): fall back to <image>.
            appendImageEl();
            applyColors(null);
            finishOverlays();
          });
          return;
        }

        // No sprite in play (e.g. dev server): paint the <image href> at once so the block is
        // never icon-less, then warm the cache so later instances inject inline.
        appendImageEl();
        finishOverlays();
        if (hasDescriptorColors) {
          applyColors(null);
        } else {
          void getIcon(cacheUrl).then((cached) => applyColors(cached.colors));
        }
        return;
      }

      // Raster or data: URI; keep the async <image> element (no inline parse).
      appendImageEl();
      if (hasDescriptorColors) {
        applyColors(null);
      } else {
        void tryApplyIconColorsFor(group, blockType, foundDir);
      }
      finishOverlays();
      return;
    };
  }

  return function (_block: Block, _helpers: Helpers) {};
}
//=============================================================================
export function buildRegistryFromManifest(manifest: LibraryManifest): BlockRegistry {
  const defs: Record<string, BlockDefinition> = {};
  for (const b of manifest.blocks || []) {
    const m = manifest as unknown as Record<string, unknown & { _dir?: string }>;
    const dir = m._dir as string | undefined;
    try {
      if (dir && b.render) {
        const r = b.render as unknown as Record<string, unknown>;
        if (r.type === 'image' && typeof r.src === 'string') {
          try {
            const base = new URL(`../libraries/${dir}/`, MODULE_URL);
            const r2 = r as unknown as { src?: string; _src?: string };
            r2._src = new URL(r2.src as string, base).href;
          } catch {}
        }
      }
    } catch {}

    // If manifest block lacks an explicit render descriptor, synthesize a
    // minimal 'math' descriptor so we can render a simple rectangle + label.
    const descriptor =
      (b.render as RenderDescriptor) ??
      ({
        type: 'math',
        formula: (b.label as string) || b.type,
        width: b.width,
        height: b.height,
      } as RenderDescriptor);

    const renderer = createBlockRenderer(descriptor, b.type, dir);
    const def: BlockDefinition = {
      type: b.type,
      width: b.width,
      height: b.height,
      inputs: b.inputs,
      outputs: b.outputs,
      defaultParams: b.defaultParams,
      render: renderer,
    };
    // Acausal blocks: carry the undirected physical pins (+ acausal/domain flags)
    // so the renderer draws connectable 'phys' ports. Absent for causal blocks.
    if (Array.isArray(b.physicalPins)) {
      def.physicalPins = b.physicalPins;
    }
    if (typeof b.acausal === 'boolean') {
      def.acausal = b.acausal;
    }
    if (typeof b.domain === 'string') {
      def.domain = b.domain;
    }
    defs[b.type] = def;
  }
  return new BlockRegistry(defs);
}
//=============================================================================
export function mergeRegistries(...registries: BlockRegistry[]): BlockRegistry {
  const merged: Record<string, BlockDefinition> = {};
  for (const r of registries) {
    Object.assign(merged, r.getDefinitions());
  }
  return new BlockRegistry(merged);
}
//=============================================================================
export async function loadManifestRegistry(
  pathOrManifest: string | LibraryManifest
): Promise<BlockRegistry> {
  if (typeof pathOrManifest === 'string') {
    try {
      const mod = await import(pathOrManifest);
      const manifestCandidate = ((mod && (mod as unknown as { default?: unknown }).default) ??
        mod) as unknown;
      const manifest = manifestCandidate as LibraryManifest;
      return buildRegistryFromManifest(manifest);
    } catch (e) {
      if (typeof fetch !== 'undefined') {
        const res = await fetch(pathOrManifest);
        const manifest = await res.json();
        return buildRegistryFromManifest(manifest as LibraryManifest);
      }
      throw e;
    }
  } else {
    return buildRegistryFromManifest(pathOrManifest);
  }
}
//=============================================================================
