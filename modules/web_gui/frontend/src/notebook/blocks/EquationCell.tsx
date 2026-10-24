//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useRef, useState } from 'react';
import katex from 'katex';
//=============================================================================
interface EquationCellProps {
  block: any;
  editor: any;
}
//=============================================================================
// A small visual equation editor: a symbol/structure palette, a live preview
// that updates as you type, and the LaTeX source. Clicking a palette item
// inserts its LaTeX at the caret.
const PALETTE: { label: string; insert: string }[] = [
  { label: 'a/b', insert: '\\frac{a}{b}' },
  { label: '√', insert: '\\sqrt{}' },
  { label: 'xⁿ', insert: '^{}' },
  { label: 'xₙ', insert: '_{}' },
  { label: 'Σ', insert: '\\sum_{}^{}' },
  { label: '∫', insert: '\\int_{}^{}' },
  { label: 'α', insert: '\\alpha' },
  { label: 'β', insert: '\\beta' },
  { label: 'θ', insert: '\\theta' },
  { label: 'π', insert: '\\pi' },
  { label: 'ω', insert: '\\omega' },
  { label: '×', insert: '\\times ' },
  { label: '≤', insert: '\\leq ' },
  { label: '≥', insert: '\\geq ' },
  { label: '∞', insert: '\\infty ' },
];
//=============================================================================
function render(latex: string): string {
  try {
    return katex.renderToString(latex || '\\,', { displayMode: true, throwOnError: false });
  } catch {
    return latex;
  }
}
//=============================================================================
export function EquationCell({ block, editor }: EquationCellProps) {
  const [editing, setEditing] = useState(false);
  const [draft, setDraft] = useState<string>(block.props.latex ?? '');
  const inputRef = useRef<HTMLTextAreaElement>(null);
  const latex: string = block.props.latex ?? '';

  const insert = (snippet: string) => {
    const el = inputRef.current;
    const pos = el ? el.selectionStart : draft.length;
    const next = draft.slice(0, pos) + snippet + draft.slice(el ? el.selectionEnd : draft.length);
    setDraft(next);
    requestAnimationFrame(() => {
      if (el) {
        el.focus();
        const caret = pos + snippet.length;
        el.setSelectionRange(caret, caret);
      }
    });
  };

  if (!editing) {
    return (
      <div className="nls-equation" contentEditable={false}>
        <div
          className="nls-equation-render"
          onDoubleClick={() => {
            setDraft(latex);
            setEditing(true);
          }}
          dangerouslySetInnerHTML={{ __html: render(latex) }}
        />
      </div>
    );
  }

  return (
    <div className="nls-equation nls-equation-editing" contentEditable={false}>
      <div className="nls-eq-preview" dangerouslySetInnerHTML={{ __html: render(draft) }} />
      <div className="nls-eq-palette">
        {PALETTE.map((p) => (
          <button key={p.label} title={p.insert} onClick={() => insert(p.insert)}>
            {p.label}
          </button>
        ))}
      </div>
      <textarea
        ref={inputRef}
        className="nls-equation-input"
        autoFocus
        value={draft}
        onChange={(e) => setDraft(e.target.value)}
        onKeyDown={(e) => e.stopPropagation()}
      />
      <div className="nls-eq-actions">
        <button
          className="nls-btn nls-btn-primary"
          onClick={() => {
            editor.updateBlock(block, { props: { latex: draft } });
            setEditing(false);
          }}
        >
          Done
        </button>
        <button className="nls-btn" onClick={() => setEditing(false)}>
          Cancel
        </button>
      </div>
    </div>
  );
}
//=============================================================================
