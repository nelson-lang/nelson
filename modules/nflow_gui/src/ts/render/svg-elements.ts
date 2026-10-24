//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export function createSvgElement(
  tag: string,
  attrs: Record<string, string | number> = {},
  text = ''
) {
  const el = document.createElementNS('http://www.w3.org/2000/svg', tag);
  for (const k in attrs) {
    if (!Object.prototype.hasOwnProperty.call(attrs, k)) continue;
    const v = attrs[k] as string | number;
    try {
      el.setAttribute(k, String(v));
    } catch {
      // ignore individual attribute failures
    }
  }
  if (text) el.textContent = text;
  return el;
}
//=============================================================================
export function renderSvgMath(
  group: Element | null | undefined,
  mathMl: string,
  width: string | number,
  height: string | number
) {
  if (!group) return;
  const container = group as HTMLElement;
  container.innerHTML = '';
  const foreign = createSvgElement('foreignObject', {
    x: '0',
    y: '0',
    width: String(width),
    height: String(height),
    class: 'upright',
  });
  const host = document.createElement('div');
  host.setAttribute('xmlns', 'http://www.w3.org/1999/xhtml');
  host.className = 'math-foreign';
  try {
    host.innerHTML = mathMl;
  } catch {
    host.textContent = mathMl;
  }
  foreign.appendChild(host);
  container.appendChild(foreign);
}
//=============================================================================
export function renderTeXMath(
  group: Element | null | undefined,
  tex: string,
  width: string | number,
  height: string | number
) {
  if (!group) return;
  const container = group as HTMLElement;
  container.innerHTML = '';
  if (shouldUseSvgMathFallback()) {
    renderSvgMathText(container, tex, Number(width), Number(height));
    scheduleUserFuncResize(group);
    return;
  }
  const foreign = createSvgElement('foreignObject', {
    x: '0',
    y: '0',
    width: String(width),
    height: String(height),
    class: 'upright',
  });
  const wrapper = document.createElement('div');
  wrapper.setAttribute('xmlns', 'http://www.w3.org/1999/xhtml');
  wrapper.className = 'math-foreign';
  const span = document.createElement('span');
  span.className = 'katex-target';
  (span as HTMLElement).dataset.tex = tex;
  (span as HTMLElement).style.whiteSpace = 'nowrap';

  const pickFontSize = (el: HTMLElement | null): string | undefined => {
    try {
      if (!el) return undefined;
      const ds = (el.dataset?.textSize || '').toString().trim();
      if (ds) return ds;
      const inline = (el.style && el.style.fontSize) || '';
      if (inline && inline.toString().trim()) return inline.toString().trim();
      if (typeof window !== 'undefined' && typeof window.getComputedStyle === 'function') {
        const cs = window.getComputedStyle(el);
        const v = cs.getPropertyValue('--block-text-font-size') || cs.fontSize;
        if (v && v.toString().trim()) return v.toString().trim();
      }
    } catch {}
    return undefined;
  };

  const forcedSize = pickFontSize(group as HTMLElement | null);
  if (forcedSize) {
    try {
      (wrapper as HTMLElement).style.fontSize = forcedSize;
    } catch {}
    try {
      (span as HTMLElement).style.fontSize = forcedSize;
    } catch {}
  }

  const gw = window as unknown as GlobalWithKatex;
  if (gw.katex && typeof gw.katex.render === 'function') {
    try {
      gw.katex.render(tex, span, { throwOnError: false });
      span.classList.remove('katex-target');
      scheduleUserFuncResize(group);
      if (shouldFitText(group)) scheduleTexFit(group);
    } catch {
      span.textContent = tex;
    }
  } else {
    katexQueue.add(group as Element);
    queueKatexRender();
  }

  wrapper.appendChild(span);
  foreign.appendChild(wrapper);
  container.appendChild(foreign);
}
//=============================================================================
let katexRetryHandle: number | null = null;
export const katexQueue: Set<Element> = new Set();
type GlobalWithKatex = Window & {
  katex?: { render?: (tex: string, node: Element, opts?: unknown) => void };
  nflowResizeMathBlock?: (id: string) => void;
  nflowResizeUserFunc?: (id: string) => void;
};
//=============================================================================
function findBlockElement(el: Element | null | undefined): Element | null {
  let cur = el as Element | null;
  while (cur) {
    if (cur.classList && cur.classList.contains('svg-block')) return cur;
    cur = cur.parentElement;
  }
  return null;
}
//=============================================================================
function callResizeCallbacks(blockId: string) {
  const gw = window as unknown as GlobalWithKatex;
  if (typeof gw.nflowResizeMathBlock === 'function') {
    try {
      gw.nflowResizeMathBlock(blockId);
      return;
    } catch {}
  }
  if (typeof gw.nflowResizeUserFunc === 'function') {
    try {
      gw.nflowResizeUserFunc(blockId);
    } catch {}
  }
}
//=============================================================================
export function shouldUseSvgMathFallback(userAgent = globalThis.navigator?.userAgent ?? '') {
  // WebKit (non-Chromium) paints transformed SVG foreignObject content at the
  // root SVG origin, which leaves math-bearing blocks (constant, gain, transfer
  // function, ...) looking empty. This affects the native webviews on both
  // macOS (WKWebView) and Linux (WebKitGTK); Chromium-based engines, including
  // the Windows WebView2 runtime, place it correctly. Native SVG text follows
  // the block transform reliably, so fall back to it on any non-Chromium WebKit.
  const isWebKit = /AppleWebKit\//.test(userAgent);
  const isChromium = /(?:Chrome|Chromium|CriOS|Edg|OPR)\//.test(userAgent);
  return isWebKit && !isChromium;
}
//=============================================================================
export function texToSvgText(tex: string): string {
  let text = String(tex || '')
    .replace(/\\(?:left|right|scriptsize|small|displaystyle|textstyle)/g, '')
    .replace(/\\!/g, '')
    .replace(/\\(?:mathsf|mathrm|mathbf|text|operatorname)\{([^{}]*)\}/g, '$1')
    .replace(/\^\{([^{}]*)\}/g, '^$1')
    .replace(/_\{([^{}]*)\}/g, '_$1');
  for (let pass = 0; pass < 4; pass += 1) {
    const next = text.replace(/\\frac\{([^{}]*)\}\{([^{}]*)\}/g, '($1) / ($2)');
    if (next === text) break;
    text = next;
  }
  return text
    .replace(/\\sqrt\{([^{}]*)\}/g, '√($1)')
    .replace(/\\(?:ge|geq)/g, '≥')
    .replace(/\\(?:le|leq)/g, '≤')
    .replace(/\\ne/g, '≠')
    .replace(/\\times/g, '×')
    .replace(/\\cdot/g, '·')
    .replace(/\\pi/g, 'π')
    .replace(/\\_/g, '_')
    .replace(/\\([A-Za-z]+)/g, '$1')
    .replace(/[{}]/g, '')
    .replace(/\^(-?1)/g, (_match, exponent: string) => (exponent === '-1' ? '⁻¹' : '¹'))
    .replace(/\^2/g, '²')
    .replace(/\^3/g, '³')
    .replace(/\s+/g, ' ')
    .trim();
}
//=============================================================================
function svgMathFontSize(group: Element): number {
  const configuredSize = Number.parseFloat(group.getAttribute('data-text-size') || '');
  if (Number.isFinite(configuredSize) && configuredSize > 0) return configuredSize;

  const classes = group.getAttribute('class') || '';
  if (/subsystem-port-label/.test(classes)) return 13;
  if (/nf-label-sink-formula/.test(classes)) return 16;
  if (/(?:^|\s)nf-(?:and|or|xor|not)-formula(?:\s|$)/.test(classes)) return 16;
  if (/(?:ss|dss)-math/.test(classes)) return 20;
  if (/nf-pid-formula|(?:zoh|foh)-math/.test(classes)) return 21;
  if (/nf-userfunc-formula/.test(classes)) return 24;
  if (/nf-switch-formula--s/.test(classes)) return 16;
  if (/nf-switch-formula--m/.test(classes)) return 22;
  if (/nf-switch-formula--l/.test(classes)) return 30;
  if (/nf-gain-formula/.test(classes)) return 28;
  if (/nf-delay-formula/.test(classes)) return 30;
  if (/nf-constant-formula/.test(classes)) return 34;
  return 40;
}
//=============================================================================
function readTexGroup(source: string, start: number): { value: string; end: number } | null {
  if (source[start] !== '{') return null;
  let depth = 0;
  for (let index = start; index < source.length; index += 1) {
    if (source[index] === '{') depth += 1;
    if (source[index] === '}') depth -= 1;
    if (depth === 0) return { value: source.slice(start + 1, index), end: index + 1 };
  }
  return null;
}
//=============================================================================
export function splitTexFraction(tex: string): { numerator: string; denominator: string } | null {
  const source = String(tex || '').trim();
  if (!source.startsWith('\\frac')) return null;
  let offset = '\\frac'.length;
  while (/\s/.test(source[offset] || '')) offset += 1;
  const numerator = readTexGroup(source, offset);
  if (!numerator) return null;
  offset = numerator.end;
  while (/\s/.test(source[offset] || '')) offset += 1;
  const denominator = readTexGroup(source, offset);
  if (!denominator || source.slice(denominator.end).trim()) return null;
  return { numerator: numerator.value, denominator: denominator.value };
}
//=============================================================================
function createSvgMathText(
  plainText: string,
  width: number,
  height: number,
  fontSize: number,
  yRatio: number
) {
  return createSvgElement(
    'text',
    {
      x: width / 2,
      y: height * yRatio,
      class: 'svg-math-text upright',
      'text-anchor': 'middle',
      'dominant-baseline': 'central',
      'font-family': 'serif',
      'font-size': fontSize,
      fill: 'var(--nflow-foreground)',
      'data-natural-width': plainText.length * fontSize * 0.56,
      'data-y-ratio': yRatio,
    },
    plainText
  );
}
//=============================================================================
function renderSvgMathText(group: Element, tex: string, width: number, height: number) {
  const fontSize = svgMathFontSize(group);
  const fraction = splitTexFraction(tex);
  if (fraction) {
    const fractionFontSize = Math.min(fontSize * 0.58, height * 0.27);
    const numerator = createSvgMathText(
      texToSvgText(fraction.numerator),
      width,
      height,
      fractionFontSize,
      0.31
    );
    const denominator = createSvgMathText(
      texToSvgText(fraction.denominator),
      width,
      height,
      fractionFontSize,
      0.7
    );
    const lineWidth = Math.max(
      Number(numerator.getAttribute('data-natural-width')),
      Number(denominator.getAttribute('data-natural-width'))
    );
    group.appendChild(numerator);
    group.appendChild(
      createSvgElement('line', {
        class: 'svg-math-fraction-line',
        y1: height / 2,
        y2: height / 2,
        stroke: 'var(--nflow-foreground)',
        'stroke-width': 1,
        'data-natural-width': lineWidth + 4,
      })
    );
    group.appendChild(denominator);
  } else {
    group.appendChild(createSvgMathText(texToSvgText(tex), width, height, fontSize, 0.5));
  }
  resizeSvgMathText(group, width, height);
}
//=============================================================================
export function resizeSvgMathText(group: Element, width: number, height: number) {
  const availableWidth = Math.max(1, width - 12);
  group.querySelectorAll('.svg-math-text').forEach((text) => {
    const naturalWidth = Number(text.getAttribute('data-natural-width'));
    const yRatio = Number(text.getAttribute('data-y-ratio'));
    text.setAttribute('x', String(width / 2));
    text.setAttribute('y', String(height * (Number.isFinite(yRatio) ? yRatio : 0.5)));
    if (Number.isFinite(naturalWidth) && naturalWidth > availableWidth) {
      text.setAttribute('textLength', String(availableWidth));
      text.setAttribute('lengthAdjust', 'spacingAndGlyphs');
    } else {
      text.removeAttribute('textLength');
      text.removeAttribute('lengthAdjust');
    }
  });
  const line = group.querySelector('.svg-math-fraction-line');
  if (line) {
    const naturalWidth = Number(line.getAttribute('data-natural-width'));
    const lineWidth = Math.min(
      availableWidth,
      Number.isFinite(naturalWidth) ? naturalWidth : width
    );
    line.setAttribute('x1', String((width - lineWidth) / 2));
    line.setAttribute('x2', String((width + lineWidth) / 2));
    line.setAttribute('y1', String(height / 2));
    line.setAttribute('y2', String(height / 2));
  }
}
//=============================================================================
function notifyUserFuncResize(group: Element | null | undefined) {
  const blockEl = findBlockElement(group);
  const blockId =
    (blockEl && blockEl.getAttribute('data-block-id')) ||
    (blockEl as HTMLElement | null)?.dataset?.blockId;
  if (!blockId || typeof window === 'undefined') return;
  callResizeCallbacks(blockId);
}
//=============================================================================
export function scheduleUserFuncResize(group: Element | null | undefined) {
  notifyUserFuncResize(group);
  try {
    requestAnimationFrame(() => notifyUserFuncResize(group));
  } catch {}
  try {
    setTimeout(() => notifyUserFuncResize(group), 150);
  } catch {}
  if (typeof document === 'undefined') return;
  const doc = document as Document & { fonts?: { ready?: Promise<void> } };
  if (!doc.fonts?.ready) return;
  doc.fonts.ready.then(() => {
    try {
      requestAnimationFrame(() => notifyUserFuncResize(group));
    } catch {}
  });
}
//=============================================================================
function shouldFitText(group: Element | null | undefined): boolean {
  const el = group as HTMLElement | null;
  if (!el) return false;
  try {
    if (el.dataset && el.dataset.fitText) return true;
    if (typeof el.getAttribute === 'function' && el.getAttribute('data-fit-text')) return true;
  } catch {}
  return false;
}
//=============================================================================
// Shrinks a KaTeX label that renders wider or taller than its block so it fits
// instead of clipping. Sizes are measured in the foreignObject's own coordinate
// space (offset/scrollWidth are SVG units), so they hold at any SVG zoom. A
// minimum scale keeps very long single tokens readable.
export function fitTexToWidth(group: Element | null | undefined) {
  const host = group as Element | null;
  if (!host || typeof host.querySelector !== 'function') return;
  const foreign = host.querySelector('foreignObject');
  if (!foreign) return;
  const wrapper = foreign.querySelector('.math-foreign') as HTMLElement | null;
  const span = foreign.querySelector('span') as HTMLElement | null;
  if (!wrapper || !span) return;
  // Not rendered yet: the placeholder still carries the katex-target class.
  if (span.classList.contains('katex-target')) return;
  const availableW = Number(foreign.getAttribute('width')) || 0;
  const availableH = Number(foreign.getAttribute('height')) || 0;
  if (!availableW) return;
  const target = (span.querySelector('.katex') as HTMLElement | null) || span;
  const contentW = target.offsetWidth || target.scrollWidth || 0;
  const contentH = target.offsetHeight || target.scrollHeight || 0;
  if (!contentW) return;
  const padX = 4;
  const padY = 2;
  const usableW = Math.max(1, availableW - padX * 2);
  const usableH = availableH ? Math.max(1, availableH - padY * 2) : Number.POSITIVE_INFINITY;
  const MIN_SCALE = 0.5;
  let factor = Math.min(1, usableW / contentW, contentH ? usableH / contentH : 1);
  if (factor >= 0.999) {
    wrapper.style.transform = '';
    return;
  }
  factor = Math.max(MIN_SCALE, factor);
  wrapper.style.transformOrigin = 'center center';
  wrapper.style.transform = `scale(${factor})`;
}
//=============================================================================
export function scheduleTexFit(group: Element | null | undefined) {
  fitTexToWidth(group);
  try {
    requestAnimationFrame(() => fitTexToWidth(group));
  } catch {}
  try {
    setTimeout(() => fitTexToWidth(group), 150);
  } catch {}
  if (typeof document === 'undefined') return;
  const doc = document as Document & { fonts?: { ready?: Promise<void> } };
  if (!doc.fonts?.ready) return;
  doc.fonts.ready.then(() => {
    try {
      requestAnimationFrame(() => fitTexToWidth(group));
    } catch {}
  });
}
//=============================================================================
export function queueKatexRender() {
  if (typeof window === 'undefined') return;
  if (katexRetryHandle != null) return;
  katexRetryHandle = setTimeout(() => {
    katexRetryHandle = null;
    if (typeof window === 'undefined') {
      katexQueue.clear();
      return;
    }
    const gw = window as unknown as GlobalWithKatex;
    if (gw.katex && typeof gw.katex.render === 'function') {
      const queued = Array.from(katexQueue);
      katexQueue.clear();
      queued.forEach((group) => {
        try {
          const spans = group.querySelectorAll('.katex-target[data-tex]');
          spans.forEach((span) => {
            const tex = (span as HTMLElement).dataset.tex || '';
            try {
              gw.katex!.render!(tex, span, { throwOnError: false });
            } catch {
              span.textContent = tex;
            }
            span.classList.remove('katex-target');
          });
          scheduleUserFuncResize(group as Element);
          if (shouldFitText(group as Element)) scheduleTexFit(group as Element);
        } catch {}
      });
    } else if (katexQueue.size) {
      queueKatexRender();
    }
  }, 100) as unknown as number;
}
//=============================================================================
let userFuncMeasureRoot: { wrapper: HTMLDivElement; foreign: HTMLDivElement } | null = null;
export function ensureUserFuncMeasureRoot() {
  if (userFuncMeasureRoot || typeof document === 'undefined') return userFuncMeasureRoot;
  const host = document.createElement('div');
  host.style.position = 'absolute';
  host.style.left = '-100000px';
  host.style.top = '-100000px';
  host.style.visibility = 'hidden';
  host.style.pointerEvents = 'none';

  const container = document.createElement('div');
  container.className = 'minmax-math';
  const foreign = document.createElement('div');
  foreign.className = 'math-foreign';
  foreign.style.whiteSpace = 'nowrap';
  container.appendChild(foreign);
  host.appendChild(container);
  document.body.appendChild(host);
  userFuncMeasureRoot = { wrapper: host, foreign };
  return userFuncMeasureRoot;
}
//=============================================================================
export function measureUserFuncTex(tex: string) {
  if (!tex || typeof document === 'undefined') return null;
  const gw = window as unknown as GlobalWithKatex;
  if (!gw.katex || typeof gw.katex.render !== 'function') return null;
  const root = ensureUserFuncMeasureRoot();
  if (!root) return null;
  try {
    root.foreign.innerHTML = '';
    const span = document.createElement('span');
    span.style.whiteSpace = 'nowrap';
    root.foreign.appendChild(span);
    try {
      gw.katex.render!(tex, span, { throwOnError: false });
    } catch {
      span.textContent = tex;
    }
    const target = root.foreign.querySelector('.katex') || span;
    const rect = (target as Element).getBoundingClientRect();
    const width = Math.max(rect.width, (target as HTMLElement).scrollWidth || 0);
    const height = Math.max(rect.height, (target as HTMLElement).scrollHeight || 0);
    if (!width || !height) return null;
    return { w: width, h: height };
  } catch {
    return null;
  }
}
//=============================================================================
export {};
//=============================================================================
