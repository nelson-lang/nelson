//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import DOMPurify from 'dompurify';
import katex from 'katex';
import 'katex/dist/katex.min.css';
//=============================================================================
// Turn an HTML fragment (from `inserthtml`, typically `markdown(...)`) into a
// safe DOM subtree with LaTeX typeset. cmark under CMARK_OPT_SAFE already
// strips raw HTML, but inserthtml also accepts arbitrary markup, so everything
// is sanitized here before it touches the document.
//
// Math is written as $…$ / $$…$$ (and \(…\) / \[…\]) in the source text and
// survives cmark as plain text; KaTeX typesets it after sanitization.
// Replace math delimiters in a text node with rendered KaTeX HTML. Returns the
// original text when no math is present, so non-math content is untouched.
function typesetMath(text: string): string | null {
  // Ordered so $$ is tried before $, and display before inline.
  const patterns: { re: RegExp; display: boolean }[] = [
    { re: /\$\$([\s\S]+?)\$\$/g, display: true },
    { re: /\\\[([\s\S]+?)\\\]/g, display: true },
    { re: /\\\(([\s\S]+?)\\\)/g, display: false },
    { re: /\$([^$\n]+?)\$/g, display: false },
  ];
  let changed = false;
  let out = text;
  for (const { re, display } of patterns) {
    out = out.replace(re, (_m, tex) => {
      changed = true;
      try {
        return katex.renderToString(tex, { displayMode: display, throwOnError: false });
      } catch {
        return _m; // leave the raw source on failure
      }
    });
  }
  return changed ? out : null;
}
//=============================================================================
function renderMathInTree(root: HTMLElement): void {
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  const targets: Text[] = [];
  let node = walker.nextNode();
  while (node) {
    // Never re-process inside code/pre or an already-rendered formula.
    const parent = node.parentElement;
    if (parent && !parent.closest('pre, code, .katex')) {
      targets.push(node as Text);
    }
    node = walker.nextNode();
  }
  for (const textNode of targets) {
    const html = typesetMath(textNode.nodeValue ?? '');
    if (html === null) continue;
    const span = document.createElement('span');
    // KaTeX output is trusted (generated here); the surrounding text is escaped
    // by assigning through a template whose text portions stay literal.
    span.innerHTML = html;
    textNode.replaceWith(span);
  }
}
//=============================================================================
export function renderRichHtml(html: string): HTMLElement {
  const container = document.createElement('div');
  container.className = 'rich-content';
  container.innerHTML = DOMPurify.sanitize(html, {
    ADD_ATTR: ['target'],
    // KaTeX needs these to render, and the source is sanitized anyway.
    ADD_TAGS: ['math', 'semantics', 'annotation', 'mrow', 'mi', 'mo', 'mn'],
  });
  // Links open in a new tab, never navigate the desktop away.
  container.querySelectorAll('a[href]').forEach((a) => {
    a.setAttribute('target', '_blank');
    a.setAttribute('rel', 'noopener noreferrer');
  });
  renderMathInTree(container);
  return container;
}
//=============================================================================
