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
function triggerDownload(url: string, filename: string) {
  const a = document.createElement('a');
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}
//=============================================================================
/** Collect all CSS text from document stylesheets that is relevant to scope elements. */
function collectScopeStyles(): string {
  const lines: string[] = [];
  try {
    Array.from(document.styleSheets).forEach((sheet) => {
      try {
        Array.from(sheet.cssRules ?? []).forEach((rule) => {
          const text = rule.cssText;
          if (text.includes('scope') || text.includes('nf-block') || text.includes('svg text')) {
            lines.push(text);
          }
        });
      } catch (_) {
        /* cross-origin sheet - skip */
      }
    });
  } catch (_) {}
  return lines.join('\n');
}
//=============================================================================
/** Builds a standalone SVG string from a block's group element. */
function buildStandaloneSvg(block: any): string {
  const group: Element = block.group;
  if (!group) return '';
  const svgEl = group.ownerDocument?.createElementNS('http://www.w3.org/2000/svg', 'svg');
  if (!svgEl) return '';

  const w = block.width ?? 300;
  const h = block.height ?? 200;
  svgEl.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
  svgEl.setAttribute('width', String(w));
  svgEl.setAttribute('height', String(h));
  svgEl.setAttribute('viewBox', `0 0 ${w} ${h}`);

  const style = group.ownerDocument!.createElementNS('http://www.w3.org/2000/svg', 'style');
  style.textContent = collectScopeStyles();
  svgEl.appendChild(style);

  const cloned = group.cloneNode(true) as Element;
  cloned.removeAttribute('transform');
  svgEl.appendChild(cloned);

  return new XMLSerializer().serializeToString(svgEl);
}
/** Exports a scope block as an SVG file with its CSS rules inlined. */
export function exportScopeAsSvg(block: any) {
  const name: string = (block.params?.name as string) || block.id || 'scope';
  const svgStr = buildStandaloneSvg(block);
  if (!svgStr) return;
  const blob = new Blob([svgStr], { type: 'image/svg+xml' });
  const url = URL.createObjectURL(blob);
  triggerDownload(url, `${name}.svg`);
}
/** Exports a scope block as a PNG, rendering the SVG on an off-screen canvas. */
export function exportScopeAsPng(block: any) {
  const name: string = (block.params?.name as string) || block.id || 'scope';
  const w: number = block.width ?? 300;
  const h: number = block.height ?? 200;
  const svgStr = buildStandaloneSvg(block);
  if (!svgStr) return;

  const blob = new Blob([svgStr], { type: 'image/svg+xml' });
  const url = URL.createObjectURL(blob);
  const img = new Image();
  img.onload = () => {
    const canvas = document.createElement('canvas');
    canvas.width = w * 2; // 2× for retina
    canvas.height = h * 2;
    const ctx = canvas.getContext('2d');
    if (!ctx) {
      URL.revokeObjectURL(url);
      return;
    }
    ctx.scale(2, 2);
    ctx.drawImage(img, 0, 0);
    URL.revokeObjectURL(url);
    const pngUrl = canvas.toDataURL('image/png');
    triggerDownload(pngUrl, `${name}.png`);
  };
  img.onerror = () => URL.revokeObjectURL(url);
  img.src = url;
}
//=============================================================================
