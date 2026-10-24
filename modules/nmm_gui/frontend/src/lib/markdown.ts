//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Minimal, safe markdown of the readme / changelog sections: everything is
// escaped first, then only a small subset (headings, lists, code, emphasis,
// https links) is rebuilt. No external library (not allowed in the webview).
export function escapeHtml(text: unknown): string {
  return String(text ?? '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]!);
}
//=============================================================================
export function renderMarkdown(src: string): string {
  const lines = String(src).replace(/\r\n?/g, '\n').split('\n');
  const inline = (s: string) => escapeHtml(s)
    .replace(/`([^`]+)`/g, '<code>$1</code>')
    .replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>')
    .replace(/(^|[^*])\*([^*\n]+)\*/g, '$1<em>$2</em>')
    .replace(/\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>');
  let html = '';
  let para: string[] = [];
  let list: string[] | null = null;
  let code: string[] | null = null;
  const flushPara = () => { if (para.length) { html += `<p>${inline(para.join(' '))}</p>`; para = []; } };
  const flushList = () => { if (list) { html += `<ul>${list.join('')}</ul>`; list = null; } };
  for (const line of lines) {
    if (/^```/.test(line)) {
      if (code === null) { flushPara(); flushList(); code = []; }
      else { html += `<pre><code>${escapeHtml(code.join('\n'))}</code></pre>`; code = null; }
      continue;
    }
    if (code !== null) { code.push(line); continue; }
    const h = line.match(/^(#{1,6})\s+(.*)$/);
    if (h) { flushPara(); flushList(); const lvl = Math.min(6, h[1].length); html += `<h${lvl}>${inline(h[2])}</h${lvl}>`; continue; }
    const li = line.match(/^\s*[-*+]\s+(.*)$/) || line.match(/^\s*\d+[.)]\s+(.*)$/);
    if (li) { flushPara(); if (!list) list = []; list.push(`<li>${inline(li[1])}</li>`); continue; }
    if (!line.trim()) { flushPara(); flushList(); continue; }
    flushList(); para.push(line.trim());
  }
  if (code !== null) html += `<pre><code>${escapeHtml(code.join('\n'))}</code></pre>`;
  flushPara(); flushList();
  return html;
}
//=============================================================================
