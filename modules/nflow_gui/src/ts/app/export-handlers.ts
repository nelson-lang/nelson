//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { sanitizeFilename } from './file-operations.js';
import { getGridState, getGridSize } from './grid-manager.js';
import { state } from './state.js';
import { canUseNativeFiles, saveViaDialog } from './file-dialogs.js';
import { saveTextWithBrowserPicker } from './browser-file-operations.js';
import { getNFlowHostMode } from './host-mode.js';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
function getSvgCanvas(): SVGSVGElement | null {
  return document.getElementById('svgCanvas') as SVGSVGElement | null;
}
//=============================================================================
function injectGridIntoSvg(
  rootSvg: SVGElement,
  vx: number,
  vy: number,
  vw: number,
  vh: number
): void {
  try {
    const gstate = getGridState();
    if (!gstate || !gstate.visible) return;
    const gridSize = getGridSize() || 20;
    const svgNS = 'http://www.w3.org/2000/svg';
    let gridColor = '#cccccc';
    try {
      const canvas = document.getElementById('svgCanvas')?.parentElement as HTMLElement | null;
      if (canvas) {
        const cs = window.getComputedStyle(canvas);
        const v = cs.getPropertyValue('--canvas-grid-color');
        if (v && v.trim()) gridColor = v.trim();
      }
    } catch (e) {}

    const g = document.createElementNS(svgNS, 'g');
    g.setAttribute('id', 'exportGridLayer');
    g.setAttribute('aria-hidden', 'true');

    const startX = Math.floor(vx / gridSize) * gridSize;
    const endX = Math.ceil((vx + vw) / gridSize) * gridSize;
    for (let x = startX; x <= endX; x += gridSize) {
      const line = document.createElementNS(svgNS, 'line');
      line.setAttribute('x1', String(x));
      line.setAttribute('y1', String(vy));
      line.setAttribute('x2', String(x));
      line.setAttribute('y2', String(vy + vh));
      line.setAttribute('stroke', gridColor);
      line.setAttribute('stroke-width', '0.5');
      line.setAttribute('opacity', '1');
      g.appendChild(line);
    }

    const startY = Math.floor(vy / gridSize) * gridSize;
    const endY = Math.ceil((vy + vh) / gridSize) * gridSize;
    for (let y = startY; y <= endY; y += gridSize) {
      const line = document.createElementNS(svgNS, 'line');
      line.setAttribute('x1', String(vx));
      line.setAttribute('y1', String(y));
      line.setAttribute('x2', String(vx + vw));
      line.setAttribute('y2', String(y));
      line.setAttribute('stroke', gridColor);
      line.setAttribute('stroke-width', '0.5');
      line.setAttribute('opacity', '1');
      g.appendChild(line);
    }

    const blockLayer = rootSvg.querySelector('#blockLayer');
    const bg = rootSvg.querySelector('rect');
    if (blockLayer && blockLayer.parentNode) {
      blockLayer.parentNode.insertBefore(g, blockLayer);
    } else if (bg && bg.parentNode) {
      bg.parentNode.insertBefore(g, bg.nextSibling);
    } else {
      rootSvg.insertBefore(g, rootSvg.firstChild);
    }
  } catch (e) {
    /* ignore */
  }
}
//=============================================================================
function collectDocumentCss(): string {
  let css = '';
  for (const sheet of Array.from(document.styleSheets)) {
    try {
      const rules = (sheet as CSSStyleSheet).cssRules as CSSRuleList | undefined;
      if (!rules) continue;
      for (const rule of Array.from(rules)) {
        try {
          const r = rule as CSSRule;
          if (r && r.cssText) css += String(r.cssText) + '\n';
        } catch (e) {
          /* ignore */
        }
      }
    } catch (e) {
      continue;
    }
  }
  return css;
}
//=============================================================================
function getExportBounds(): { vx: number; vy: number; vw: number; vh: number } {
  const svg = getSvgCanvas();
  const pad = 40;
  let minX = Infinity,
    minY = Infinity,
    maxX = -Infinity,
    maxY = -Infinity;
  for (const id of ['blockLayer', 'wireLayer']) {
    const layer = svg ? svg.querySelector('#' + id) : null;
    if (!layer) continue;
    try {
      const bb = (layer as SVGGraphicsElement).getBBox();
      if (bb && bb.width > 0 && bb.height > 0 && isFinite(bb.x)) {
        minX = Math.min(minX, bb.x);
        minY = Math.min(minY, bb.y);
        maxX = Math.max(maxX, bb.x + bb.width);
        maxY = Math.max(maxY, bb.y + bb.height);
      }
    } catch (e) {}
  }
  if (isFinite(minX) && isFinite(minY) && maxX > minX && maxY > minY) {
    minX -= pad;
    minY -= pad;
    maxX += pad;
    maxY += pad;
    const vw = maxX - minX;
    const vh = maxY - minY;
    return { vx: minX, vy: minY, vw, vh };
  }
  const vbStr = svg ? svg.getAttribute('viewBox') || '' : '';
  const parts = vbStr.split(/[\s,]+/).map(Number);
  if (parts.length === 4 && parts.every((n) => isFinite(n))) {
    const [vx, vy, vw, vh] = parts;
    return { vx, vy, vw, vh };
  }
  const r = svg ? svg.getBoundingClientRect() : { width: 800, height: 600 };
  return { vx: 0, vy: 0, vw: Math.max(1, r.width), vh: Math.max(1, r.height) };
}
//=============================================================================
function makeSvgFilename(name?: string): string {
  const base = sanitizeFilename(name || 'diagram');
  if (base.toLowerCase().endsWith('.svg')) return base;
  return base + '.svg';
}
//=============================================================================
function inlineSvgStyles(svgEl: SVGSVGElement): void {
  const mathCss = `
    .math-foreign { width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; font-size: 40px; overflow: visible; color: #000; }
    .nf-gain-formula .math-foreign { font-size: 28px; }
    .nf-constant-formula .math-foreign { font-size: 34px; }
    .ss-math .math-foreign, .dss-math .math-foreign { font-size: 20px; }
    .nf-delay-formula .math-foreign { font-size: 30px; }
    .label-math .math-foreign { font-size: 16px; }
    .nf-pid-formula .math-foreign, .zoh-math .math-foreign, .foh-math .math-foreign { font-size: 21px; }
    .minmax-math .math-foreign { line-height: 1; }
    .katex { font: normal 1.21em KaTeX_Main, "Times New Roman", serif; line-height: 1.2; text-indent: 0; text-rendering: auto; }
    .katex .katex-mathml { position: absolute; clip: rect(1px, 1px, 1px, 1px); padding: 0; border: 0; height: 1px; width: 1px; overflow: hidden; }
    .katex .katex-html { display: inline-block; }
    .katex .base { display: inline-block; white-space: nowrap; position: relative; }
    .katex .strut { display: inline-block; }
    .katex .mord, .katex .mrel, .katex .mbin, .katex .mop, .katex .minner, .katex .mpunct, .katex .mopen, .katex .mclose { display: inline-block; }
    .katex .mfrac { display: inline-block; text-align: center; }
    .katex .mfrac > span { display: block; }
    .katex .mfrac .frac-line { width: 100%; border-bottom-style: solid; display: inline-block; min-height: 1px; }
    .katex .vlist-t { display: inline-table; table-layout: fixed; border-collapse: collapse; }
    .katex .vlist-r { display: table-row; }
    .katex .vlist { display: table-cell; vertical-align: bottom; position: relative; }
    .katex .vlist > span { display: block; height: 0; position: relative; }
    .katex .vlist > span > span { display: inline-block; }
    .katex .vlist-s { display: table-cell; vertical-align: bottom; font-size: 1px; width: 2px; min-width: 2px; }
    .katex .msupsub { text-align: left; }
    .katex .mathnormal { font-family: KaTeX_Math; font-style: italic; }
    .katex .mathit { font-family: KaTeX_Main; font-style: italic; }
    foreignObject { overflow: visible; }
  `;
  const styleEl = document.createElementNS('http://www.w3.org/2000/svg', 'style');
  styleEl.setAttribute('type', 'text/css');
  styleEl.textContent = mathCss;
  svgEl.insertBefore(styleEl, svgEl.firstChild);

  const propList = [
    'fill',
    'fill-opacity',
    'stroke',
    'stroke-width',
    'stroke-linecap',
    'stroke-linejoin',
    'stroke-dasharray',
    'stroke-opacity',
    'opacity',
    'color',
    'vector-effect',
    'filter',
    'clip-path',
    'clip-rule',
    'mask',
    'marker-start',
    'marker-mid',
    'marker-end',
    'background-image',
    'background-color',
    'background-size',
    'background-position',
    'background-repeat',
    'font-family',
    'font-size',
    'font-weight',
    'text-anchor',
    'dominant-baseline',
  ];
  let attached = false;
  let container: HTMLDivElement | null = null;
  try {
    if (!svgEl.ownerDocument || svgEl.ownerDocument !== document || !svgEl.isConnected) {
      container = document.createElement('div');
      container.style.position = 'absolute';
      container.style.left = '-9999px';
      container.style.top = '0';
      container.style.width = '1px';
      container.style.height = '1px';
      container.style.overflow = 'hidden';
      container.setAttribute('aria-hidden', 'true');
      container.appendChild(svgEl);
      document.body.appendChild(container);
      attached = true;
    }

    const elems = svgEl.querySelectorAll('*');
    elems.forEach((el) => {
      try {
        const cs = window.getComputedStyle(el as Element);
        if (!cs) return;
        const parts: string[] = [];
        for (const p of propList) {
          const v = cs.getPropertyValue(p);
          if (v === null || v === undefined) continue;
          let vv = String(v).trim();
          if (!vv || vv === 'normal') continue;
          if (vv === 'rgba(0, 0, 0, 0)') vv = 'transparent';
          parts.push(`${p}: ${vv}`);
        }
        if (parts.length) {
          const existing = (el as Element).getAttribute('style') || '';
          (el as Element).setAttribute(
            'style',
            existing ? existing + ';' + parts.join('; ') : parts.join('; ')
          );
        }
      } catch (e) {
        /* ignore per-element errors */
      }
    });
  } catch (e) {
    const css = collectDocumentCss();
    if (css) {
      const safeCss = css.replace(/#svgCanvas/g, '.exported-svg');
      const style = document.createElementNS('http://www.w3.org/2000/svg', 'style');
      style.setAttribute('type', 'text/css');
      style.textContent = '\n/* inlined styles */\n' + safeCss + '\n';
      try {
        svgEl.classList.add('exported-svg');
      } catch (e) {}
      const first = svgEl.firstChild;
      if (first) svgEl.insertBefore(style, first);
      else svgEl.appendChild(style);
    }
  } finally {
    if (attached && container && container.parentNode) container.parentNode.removeChild(container);
  }
}
//=============================================================================
export async function buildExportSvg(
  s?: { diagramName?: string } | undefined
): Promise<{ svgStr: string; filename: string } | null> {
  const svg = getSvgCanvas();
  if (!svg) return null;
  const { vx, vy, vw, vh } = getExportBounds();
  const clone = svg.cloneNode(true) as SVGSVGElement;
  clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
  try {
    clone.setAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
  } catch {}
  clone.setAttribute('viewBox', `${vx} ${vy} ${vw} ${vh}`);
  clone.setAttribute('width', String(vw));
  clone.setAttribute('height', String(vh));
  clone.removeAttribute(FieldNames.kId);

  const bgRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  bgRect.setAttribute('x', String(vx));
  bgRect.setAttribute('y', String(vy));
  bgRect.setAttribute('width', String(vw));
  bgRect.setAttribute('height', String(vh));
  try {
    const fill =
      state.canvasBackground && String(state.canvasBackground).trim()
        ? String(state.canvasBackground).trim()
        : '#ffffff';
    bgRect.setAttribute('fill', fill);
  } catch (e) {
    bgRect.setAttribute('fill', '#ffffff');
  }
  clone.insertBefore(bgRect, clone.firstChild);

  const overlayClone = clone.querySelector('#overlayLayer');
  if (overlayClone) overlayClone.innerHTML = '';

  const offscreen = document.createElement('div');
  offscreen.style.cssText =
    'position:absolute;left:-9999px;top:0;width:1px;height:1px;overflow:hidden;';
  offscreen.setAttribute('aria-hidden', 'true');
  offscreen.appendChild(clone);
  document.body.appendChild(offscreen);

  try {
    try {
      preserveForeignObjectHtmlStyles(clone);
    } catch (e) {}
    inlineSvgStyles(clone);
    try {
      injectGridIntoSvg(clone, vx, vy, vw, vh);
    } catch (e) {}
    try {
      await embedUsedFonts(clone);
    } catch (e) {}
    try {
      const remaining = await embedExternalImages(clone);
      if (remaining && remaining.length) {
        try {
          const svgNS = 'http://www.w3.org/2000/svg';
          const imgsRemain = Array.from(clone.querySelectorAll('image')) as SVGImageElement[];
          for (const imgEl of imgsRemain) {
            try {
              const href =
                imgEl.getAttribute('href') ||
                imgEl.getAttributeNS('http://www.w3.org/1999/xlink', 'href') ||
                '';
              if (!href || href.startsWith('data:')) continue;
              let rx = Number(imgEl.getAttribute('x') || '0');
              let ry = Number(imgEl.getAttribute('y') || '0');
              let rw = Number(imgEl.getAttribute('width') || '0');
              let rh = Number(imgEl.getAttribute('height') || '0');
              if (!rw || !rh) {
                try {
                  const bb = (imgEl as unknown as SVGGraphicsElement).getBBox();
                  rx = bb.x;
                  ry = bb.y;
                  rw = bb.width;
                  rh = bb.height;
                } catch (e) {}
              }
              if (rw > 0 && rh > 0) {
                const png = await rasterizeSvgRegion(clone, rx, ry, rw, rh);
                if (png) {
                  const newImg = document.createElementNS(svgNS, 'image');
                  newImg.setAttribute('x', String(rx));
                  newImg.setAttribute('y', String(ry));
                  newImg.setAttribute('width', String(rw));
                  newImg.setAttribute('height', String(rh));
                  try {
                    newImg.setAttribute('href', png);
                    try {
                      newImg.setAttributeNS('http://www.w3.org/1999/xlink', 'href', png);
                    } catch {}
                  } catch {}
                  try {
                    imgEl.parentNode?.replaceChild(newImg, imgEl);
                    continue;
                  } catch (e) {}
                }
              }
              // fallback: transparent placeholder
              const TRANSPARENT_PNG =
                'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR4nGNgYAAAAAMAASsJTYQAAAAASUVORK5CYII=';
              try {
                imgEl.setAttribute('href', TRANSPARENT_PNG);
                try {
                  imgEl.setAttributeNS('http://www.w3.org/1999/xlink', 'href', TRANSPARENT_PNG);
                } catch {}
              } catch {}
            } catch {}
          }
        } catch (e) {}
      }
    } catch (e) {}
  } finally {
    if (offscreen.parentNode) offscreen.parentNode.removeChild(offscreen);
  }

  clone.style.removeProperty('background-color');
  clone.style.removeProperty('background');
  clone.setAttribute(
    'style',
    `display: block; width: ${vw}px; height: ${vh}px; background: transparent;`
  );

  const svgStr = new XMLSerializer().serializeToString(clone);
  const filename = makeSvgFilename(s?.diagramName || state?.diagramName);
  return { svgStr, filename };
}
//=============================================================================
function fontUrlForFamily(family?: string): string {
  if (!family) return 'assets/katex/fonts/KaTeX_Main-Regular.woff2';
  const f = family.toLowerCase();
  if (f.includes('katex_main') || f.includes('katex-main') || f.includes('katex'))
    return 'assets/katex/fonts/KaTeX_Main-Regular.woff2';
  if (f.includes('times') || f.includes('serif'))
    return 'assets/katex/fonts/KaTeX_Main-Regular.woff2';
  if (f.includes('sans')) return 'assets/katex/fonts/KaTeX_SansSerif-Regular.woff2';
  return 'assets/katex/fonts/KaTeX_Main-Regular.woff2';
}
//=============================================================================
async function fetchFontAsDataUrl(url: string): Promise<string | null> {
  try {
    const res = await fetch(url);
    if (!res.ok) throw new Error('font fetch failed');
    const buf = await res.arrayBuffer();
    const bytes = new Uint8Array(buf);
    let binary = '';
    const chunk = 0x8000;
    for (let i = 0; i < bytes.length; i += chunk) {
      const slice = bytes.subarray(i, i + chunk);
      binary += String.fromCharCode.apply(null, Array.from(slice) as unknown as number[]);
    }
    const b64 = btoa(binary);
    return `data:font/woff2;base64,${b64}`;
  } catch (e) {
    return null;
  }
}
//=============================================================================
async function fetchResourceAsDataUrl(url: string): Promise<string | null> {
  try {
    const res = await fetch(url);
    if (!res.ok) return null;
    const buf = await res.arrayBuffer();
    const bytes = new Uint8Array(buf);
    let binary = '';
    const chunk = 0x8000;
    for (let i = 0; i < bytes.length; i += chunk) {
      const slice = bytes.subarray(i, i + chunk);
      binary += String.fromCharCode.apply(null, Array.from(slice) as unknown as number[]);
    }
    const b64 = btoa(binary);
    const ctype = res.headers.get('content-type') || 'application/octet-stream';
    return `data:${ctype};base64,${b64}`;
  } catch (e) {
    return null;
  }
}
//=============================================================================
async function embedExternalImages(rootSvg: SVGSVGElement): Promise<string[]> {
  try {
    const imgs = Array.from(rootSvg.querySelectorAll('image')) as SVGImageElement[];
    const urls = new Set<string>();
    imgs.forEach((img) => {
      const href =
        img.getAttribute('href') || img.getAttributeNS('http://www.w3.org/1999/xlink', 'href');
      if (href && !href.startsWith('data:')) urls.add(href);
    });

    const elems = Array.from(rootSvg.querySelectorAll('*')) as Element[];
    elems.forEach((el) => {
      const s = el.getAttribute('style') || '';
      const m = s.match(/url\(["']?([^"')]+)["']?\)/g);
      if (m) {
        for (const mm of m) {
          const urlMatch = mm.match(/url\(["']?([^"')]+)["']?\)/);
          if (urlMatch && urlMatch[1] && !urlMatch[1].startsWith('data:')) urls.add(urlMatch[1]);
        }
      }
    });

    for (const u of Array.from(urls)) {
      try {
        let data = await fetchResourceAsDataUrl(u);
        if (!data) {
          // Try Image+canvas fallback (may fail for cross-origin images)
          try {
            await new Promise<void>((resolve, reject) => {
              const img = new Image();
              img.crossOrigin = 'anonymous';
              img.onload = () => {
                try {
                  const c = document.createElement('canvas');
                  c.width = img.naturalWidth || img.width || 1;
                  c.height = img.naturalHeight || img.height || 1;
                  const ctx = c.getContext('2d');
                  if (!ctx) return reject(new Error('no-canvas-ctx'));
                  ctx.drawImage(img, 0, 0, c.width, c.height);
                  try {
                    const d = c.toDataURL();
                    if (d && d.startsWith('data:')) {
                      data = d;
                      return resolve();
                    }
                    return reject(new Error('toDataURL-fail'));
                  } catch (e) {
                    return reject(e);
                  }
                } catch (e) {
                  return reject(e);
                }
              };
              img.onerror = () => reject(new Error('img-load-failed'));
              img.src = u;
            });
          } catch {}
        }
        imgs.forEach((img) => {
          const href =
            img.getAttribute('href') || img.getAttributeNS('http://www.w3.org/1999/xlink', 'href');
          if (href === u) {
            try {
              img.setAttribute('href', data as string);
              try {
                img.setAttributeNS('http://www.w3.org/1999/xlink', 'href', data as string);
              } catch {}
            } catch {}
          }
        });
        elems.forEach((el) => {
          const s = el.getAttribute('style') || '';
          if (s && s.includes(u)) {
            try {
              el.setAttribute('style', s.split(u).join(data as string));
            } catch {}
          }
        });
      } catch {
        continue;
      }
    }
    const remaining: string[] = [];
    imgs.forEach((img) => {
      try {
        const href =
          img.getAttribute('href') || img.getAttributeNS('http://www.w3.org/1999/xlink', 'href');
        if (href && !href.startsWith('data:')) remaining.push(href);
      } catch (e) {
        /* ignore */
      }
    });
    return remaining;
  } catch (e) {}
  return [];
}
//=============================================================================
async function rasterizeSvgRegion(
  rootSvg: SVGSVGElement,
  rx: number,
  ry: number,
  rw: number,
  rh: number
): Promise<string | null> {
  try {
    const clone = rootSvg.cloneNode(true) as SVGSVGElement;
    if (!clone.getAttribute('xmlns')) clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
    try {
      if (!clone.getAttribute('xmlns:xlink'))
        clone.setAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
    } catch {}
    // Cap raster size to avoid embedding huge bitmaps into the SVG.
    const MAX_RASTER_DIM = 2048;
    const maxSide = Math.max(1, Math.max(rw, rh));
    const scale = Math.min(1, MAX_RASTER_DIM / maxSide);
    const outW = Math.max(1, Math.round(rw * scale));
    const outH = Math.max(1, Math.round(rh * scale));
    clone.setAttribute('viewBox', `${rx} ${ry} ${rw} ${rh}`);
    clone.setAttribute('width', String(outW));
    clone.setAttribute('height', String(outH));
    const svgStr = new XMLSerializer().serializeToString(clone);
    const blob = new Blob([svgStr], { type: 'image/svg+xml;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    try {
      const img = new Image();
      img.crossOrigin = 'anonymous';
      const dataUrl = await new Promise<string>((resolve, reject) => {
        img.onload = () => {
          try {
            const c = document.createElement('canvas');
            // canvas size uses the scaled output pixel dimensions
            c.width = outW;
            c.height = outH;
            const ctx = c.getContext('2d');
            if (!ctx) return reject(new Error('no-canvas-ctx'));
            ctx.drawImage(img, 0, 0, c.width, c.height);
            try {
              const d = c.toDataURL();
              resolve(d);
            } catch (e) {
              reject(e);
            }
          } catch (e) {
            reject(e);
          }
        };
        img.onerror = () => reject(new Error('img-load-failed'));
        img.src = url;
      });
      return dataUrl;
    } finally {
      URL.revokeObjectURL(url);
    }
  } catch {
    return null;
  }
}
//=============================================================================
function collectUsedFontFamilies(rootSvg: SVGSVGElement): string[] {
  const set = new Set<string>();
  try {
    const elems = Array.from(rootSvg.querySelectorAll('foreignObject, foreignObject *'));
    for (const el of elems) {
      try {
        const cs = window.getComputedStyle(el as Element);
        if (!cs) continue;
        const ff = cs.getPropertyValue('font-family');
        if (!ff) continue;
        const first = ff.split(',')[0].trim().replace(/['"]+/g, '');
        if (first) set.add(first);
      } catch (e) {
        continue;
      }
    }
  } catch (e) {}
  return Array.from(set);
}
//=============================================================================
async function embedUsedFonts(rootSvg: SVGSVGElement): Promise<void> {
  try {
    const families = collectUsedFontFamilies(rootSvg);
    if (!families.length) return;
    const seen = new Set<string>();
    const rules: string[] = [];
    for (const fam of families) {
      try {
        const url = fontUrlForFamily(fam);
        if (!url || seen.has(url)) continue;
        seen.add(url);
        const dataUrl = await fetchFontAsDataUrl(url);
        if (!dataUrl) continue;
        const safeName = fam.replace(/[^a-zA-Z0-9_-]/g, '_');
        rules.push(
          `@font-face{font-family:${safeName};src:url(${dataUrl}) format("woff2");font-weight:400;font-style:normal;}`
        );
        if (fam.toLowerCase().includes('katex')) {
          rules.push(
            `@font-face{font-family:KaTeX_Main;src:url(${dataUrl}) format("woff2");font-weight:400;font-style:normal;}`
          );
        }
      } catch (e) {
        continue;
      }
    }
    if (rules.length) {
      const style = document.createElementNS('http://www.w3.org/2000/svg', 'style');
      style.setAttribute('type', 'text/css');
      style.textContent = '\n/* embedded fonts */\n' + rules.join('\n') + '\n';
      rootSvg.insertBefore(style, rootSvg.firstChild);
    }
  } catch (e) {}
}
//=============================================================================
function copyComputedStylesRecursive(srcEl: Element, dstEl: Element): void {
  try {
    const propList = [
      'font-family',
      'font-size',
      'font-weight',
      'font-style',
      'color',
      'line-height',
      'white-space',
      'letter-spacing',
      'text-decoration',
      'text-anchor',
      'display',
      'position',
      'vertical-align',
      'text-align',
      'margin',
      'margin-top',
      'margin-right',
      'margin-bottom',
      'margin-left',
      'padding',
      'padding-top',
      'padding-right',
      'padding-bottom',
      'padding-left',
      'width',
      'height',
      'min-width',
      'min-height',
      'max-width',
      'max-height',
      'top',
      'left',
      'right',
      'bottom',
      'transform',
      'transform-origin',
      'filter',
      'clip-path',
      'clip-rule',
      'mask',
      'marker-start',
      'background-image',
      'background-color',
      'background-size',
      'background-position',
      'background-repeat',
      'background',
      'box-shadow',
      'border-radius',
      'marker-mid',
      'marker-end',
    ];
    const cs = window.getComputedStyle(srcEl as Element);
    if (cs) {
      const parts: string[] = [];
      for (const p of propList) {
        const v = cs.getPropertyValue(p);
        if (v && v.trim() && v !== 'normal' && v !== 'auto' && v !== 'none' && v !== '0px') {
          parts.push(`${p}: ${v.trim()}`);
        }
      }
      if (parts.length) {
        const existing = dstEl.getAttribute('style') || '';
        dstEl.setAttribute('style', existing + (existing ? '; ' : '') + parts.join('; '));
      }
    }
  } catch (e) {}
  const srcChildren = Array.from((srcEl as Element).children || []);
  const dstChildren = Array.from((dstEl as Element).children || []);
  for (let i = 0; i < Math.min(srcChildren.length, dstChildren.length); i++) {
    copyComputedStylesRecursive(srcChildren[i], dstChildren[i]);
  }
}
//=============================================================================
function preserveForeignObjectHtmlStyles(cloneSvg: SVGSVGElement): void {
  const liveSvg = document.getElementById('svgCanvas') as SVGSVGElement | null;
  if (!liveSvg) return;
  const liveFobjs = Array.from(liveSvg.querySelectorAll('foreignObject'));
  const cloneFobjs = Array.from(cloneSvg.querySelectorAll('foreignObject'));
  for (let i = 0; i < Math.min(liveFobjs.length, cloneFobjs.length); i++) {
    const liveFo = liveFobjs[i] as Element;
    const cloneFo = cloneFobjs[i] as Element;
    try {
      const liveInner = (liveFo.firstElementChild || liveFo.firstChild) as Element | null;
      const cloneInner = (cloneFo.firstElementChild || cloneFo.firstChild) as Element | null;
      if (liveInner && cloneInner) {
        const cs = window.getComputedStyle(liveInner);
        const fontSize = cs.getPropertyValue('font-size');
        const color = cs.getPropertyValue('color');
        if (fontSize) {
          (cloneInner as HTMLElement).style.setProperty('font-size', fontSize, 'important');
        }
        if (color && color !== 'rgba(0, 0, 0, 0)') {
          (cloneInner as HTMLElement).style.setProperty('color', color);
        }
        const katexEls = cloneFo.querySelectorAll('.katex, .katex-html, span');
        const liveKatexEls = liveFo.querySelectorAll('.katex, .katex-html, span');
        for (let j = 0; j < Math.min(katexEls.length, liveKatexEls.length); j++) {
          try {
            const liveCs = window.getComputedStyle(liveKatexEls[j] as Element);
            const fs = liveCs.getPropertyValue('font-size');
            if (fs) (katexEls[j] as HTMLElement).style.setProperty('font-size', fs);
          } catch (e) {}
        }
        copyComputedStylesRecursive(liveInner, cloneInner);
      }
    } catch (e) {
      continue;
    }
  }
}
//=============================================================================
export async function exportCanvasSVG(): Promise<void> {
  const svg = getSvgCanvas();
  if (!svg) return;
  const { vx, vy, vw, vh } = getExportBounds();
  const clone = svg.cloneNode(true) as SVGSVGElement;
  clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
  try {
    clone.setAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
  } catch {}
  clone.setAttribute('viewBox', `${vx} ${vy} ${vw} ${vh}`);
  clone.setAttribute('width', String(vw));
  clone.setAttribute('height', String(vh));
  clone.removeAttribute(FieldNames.kId);

  const bgRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  bgRect.setAttribute('x', String(vx));
  bgRect.setAttribute('y', String(vy));
  bgRect.setAttribute('width', String(vw));
  bgRect.setAttribute('height', String(vh));
  try {
    const fill =
      state.canvasBackground && String(state.canvasBackground).trim()
        ? String(state.canvasBackground).trim()
        : '#ffffff';
    bgRect.setAttribute('fill', fill);
  } catch (e) {
    bgRect.setAttribute('fill', '#ffffff');
  }
  clone.insertBefore(bgRect, clone.firstChild);

  const overlayClone = clone.querySelector('#overlayLayer');
  if (overlayClone) overlayClone.innerHTML = '';

  const offscreen = document.createElement('div');
  offscreen.style.cssText =
    'position:absolute;left:-9999px;top:0;width:1px;height:1px;overflow:hidden;';
  offscreen.setAttribute('aria-hidden', 'true');
  offscreen.appendChild(clone);
  document.body.appendChild(offscreen);

  try {
    try {
      preserveForeignObjectHtmlStyles(clone);
    } catch (e) {}
    inlineSvgStyles(clone);
    try {
      injectGridIntoSvg(clone, vx, vy, vw, vh);
    } catch (e) {}
    try {
      await embedUsedFonts(clone);
    } catch (e) {}
    try {
      const remaining = await embedExternalImages(clone);
      if (remaining && remaining.length) {
        try {
          const svgNS = 'http://www.w3.org/2000/svg';
          const imgsRemain = Array.from(clone.querySelectorAll('image')) as SVGImageElement[];
          for (const imgEl of imgsRemain) {
            try {
              const href =
                imgEl.getAttribute('href') ||
                imgEl.getAttributeNS('http://www.w3.org/1999/xlink', 'href') ||
                '';
              if (!href || href.startsWith('data:')) continue;
              let rx = Number(imgEl.getAttribute('x') || '0');
              let ry = Number(imgEl.getAttribute('y') || '0');
              let rw = Number(imgEl.getAttribute('width') || '0');
              let rh = Number(imgEl.getAttribute('height') || '0');
              if (!rw || !rh) {
                try {
                  const bb = (imgEl as unknown as SVGGraphicsElement).getBBox();
                  rx = bb.x;
                  ry = bb.y;
                  rw = bb.width;
                  rh = bb.height;
                } catch (e) {}
              }
              if (rw > 0 && rh > 0) {
                const png = await rasterizeSvgRegion(clone, rx, ry, rw, rh);
                if (png) {
                  const newImg = document.createElementNS(svgNS, 'image');
                  newImg.setAttribute('x', String(rx));
                  newImg.setAttribute('y', String(ry));
                  newImg.setAttribute('width', String(rw));
                  newImg.setAttribute('height', String(rh));
                  try {
                    newImg.setAttribute('href', png);
                    try {
                      newImg.setAttributeNS('http://www.w3.org/1999/xlink', 'href', png);
                    } catch {}
                  } catch {}
                  try {
                    imgEl.parentNode?.replaceChild(newImg, imgEl);
                    continue;
                  } catch (e) {}
                }
              }
              const TRANSPARENT_PNG =
                'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR4nGNgYAAAAAMAASsJTYQAAAAASUVORK5CYII=';
              try {
                imgEl.setAttribute('href', TRANSPARENT_PNG);
                try {
                  imgEl.setAttributeNS('http://www.w3.org/1999/xlink', 'href', TRANSPARENT_PNG);
                } catch {}
              } catch {}
            } catch {}
          }
        } catch (e) {}
      }
    } catch (e) {}
  } finally {
    if (offscreen.parentNode) offscreen.parentNode.removeChild(offscreen);
  }

  clone.style.removeProperty('background-color');
  clone.style.removeProperty('background');
  clone.setAttribute(
    'style',
    `display: block; width: ${vw}px; height: ${vh}px; background: transparent;`
  );

  const svgStr = new XMLSerializer().serializeToString(clone);
  const filename = makeSvgFilename(state?.diagramName);
  try {
    if (canUseNativeFiles() && (await getNFlowHostMode()) !== 'web') {
      await saveViaDialog({
        typeOfFile: 'svg',
        content: svgStr,
        defaultName: filename,
      });
    } else {
      await saveTextWithBrowserPicker({
        content: svgStr,
        defaultName: filename,
        extension: '.svg',
        mimeType: 'image/svg+xml',
        description: 'SVG image',
      });
    }
  } catch (e) {}
}
//=============================================================================
