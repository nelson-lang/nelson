//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, type CSSProperties, type ReactNode } from 'react';
import type { Line, Span } from '../console/ansi';
import { renderRichHtml } from '../console/richContent';
//=============================================================================
export function TextSegment({
  lines,
  onLink,
  highlight,
  tail,
}: {
  lines: Line[];
  onLink: (uri: string) => void;
  highlight?: string;
  tail?: JSX.Element;
}): JSX.Element {
  const lastIndex = lines.length - 1;
  return (
    <>
      {lines.map((line, i) => {
        const hasTail = tail && i === lastIndex;
        return (
          <div className={'console-line' + (hasTail ? ' console-line--active' : '')} key={i}>
            {line.length === 0 && !hasTail
              ? '\u00a0'
              : line.map((span, j) => (
                  <SpanView key={j} span={span} onLink={onLink} highlight={highlight} />
                ))}
            {hasTail ? tail : null}
          </div>
        );
      })}
    </>
  );
}
//=============================================================================
function withHighlight(text: string, term?: string): ReactNode {
  if (!term) return text;
  const parts: ReactNode[] = [];
  const lower = text.toLowerCase();
  const needle = term.toLowerCase();
  let from = 0;
  for (let at = lower.indexOf(needle, from); at !== -1; at = lower.indexOf(needle, from)) {
    if (at > from) parts.push(text.slice(from, at));
    parts.push(
      <mark className="console-find" key={at}>
        {text.slice(at, at + term.length)}
      </mark>
    );
    from = at + term.length;
  }
  if (parts.length === 0) return text;
  if (from < text.length) parts.push(text.slice(from));
  return parts;
}
//=============================================================================
function SpanView({
  span,
  onLink,
  highlight,
}: {
  span: Span;
  onLink: (uri: string) => void;
  highlight?: string;
}): JSX.Element {
  const style: CSSProperties = {
    color: span.style.fg,
    background: span.style.bg,
    fontWeight: span.style.bold ? 700 : undefined,
  };
  if (span.link) {
    return (
      <a
        className="console-link"
        style={style}
        onClick={(e) => {
          e.preventDefault();
          onLink(span.link!);
        }}
        href="#"
      >
        {withHighlight(span.text, highlight)}
      </a>
    );
  }
  return <span style={style}>{withHighlight(span.text, highlight)}</span>;
}
//=============================================================================
// A rich (HTML/markdown/LaTeX) block, rendered sanitized with KaTeX.
export function RichBlock({ html }: { html: string }): JSX.Element {
  const host = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const node = host.current;
    if (!node) return;
    node.replaceChildren(renderRichHtml(html));
  }, [html]);
  return <div className="console-rich" ref={host} />;
}
//=============================================================================
