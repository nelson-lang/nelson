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
import Editor, { type OnMount } from '@monaco-editor/react';
import { setupMonaco } from '../../editor/monacoSetup';
import { useNotebook } from '../context';
import { clearedOutputProps, isCellRunning } from '../cellOutputs';
import { isFigureLive, parseFigureHandles, closeLiveFigures, releaseFigure } from '../liveFigures';
import { FigurePanel } from '../../panels/FigurePanel';
import { undockFigure } from '../../desktop/desktop';
//=============================================================================
interface CodeCellProps {
  block: any;
  editor: any;
}
//=============================================================================
export function CodeCell({ block, editor }: CodeCellProps) {
  const ctx = useNotebook();
  const codeRef = useRef<string>(block.props.code ?? '');
  const [, force] = useState(0);
  const lines = (block.props.code ?? '').split('\n').length;
  const height = Math.min(360, Math.max(56, lines * 19 + 16));

  const persist = () => editor.updateBlock(block, { props: { code: codeRef.current } });

  const handleMount: OnMount = (ed) => {
    ed.onDidBlurEditorText(() => persist());
  };

  const output: string = block.props.output ?? '';
  const errorMsg: string = block.props.errorMsg ?? '';
  let figures: string[] = [];
  try {
    figures = JSON.parse(block.props.figures || '[]');
  } catch {
    figures = [];
  }
  const figureHandles = parseFigureHandles(block.props.figureHandles);
  const hasResult = output || figures.length > 0 || errorMsg;

  const stale = !!block.props.stale;
  const runningNow = isCellRunning(ctx.runningBlockId, block.id);
  const clearOutput = () => {
    void closeLiveFigures(parseFigureHandles(block.props.figureHandles));
    editor.updateBlock(block, { props: clearedOutputProps() });
  };

  const popOutFigure = (index: number) => {
    const handles = parseFigureHandles(block.props.figureHandles);
    const h = handles[index];
    if (!(h >= 0)) return;
    handles[index] = -1;
    void releaseFigure(h).then(() => undockFigure(h, `Figure ${h + 1}`));
    editor.updateBlock(block, { props: { figureHandles: JSON.stringify(handles) } });
  };

  return (
    <div
      className="nls-codecell"
      contentEditable={false}
      data-hide={ctx.hideCode}
      data-stale={stale}
      data-running={runningNow}
      data-layout={ctx.outputLayout}
    >
      {!ctx.hideCode && (
        <div className="nls-codecell-editor">
          {stale && (
            <button
              className="nls-stale-badge"
              title="This cell is out of date — click to re-run it"
              onClick={() => {
                persist();
                void ctx.runBlock(block.id);
              }}
            >
              out of date
            </button>
          )}
          <div className="nls-gutter">
            <button
              className="nls-run"
              title="Run this cell"
              onClick={() => {
                persist();
                void ctx.runBlock(block.id);
              }}
            >
              <svg viewBox="0 0 16 16" width="14" height="14" aria-hidden="true">
                <path d="M4 3l9 5-9 5z" fill="currentColor" />
              </svg>
            </button>
            <button
              className="nls-run nls-run-sub"
              title="Run to here"
              onClick={() => {
                persist();
                void ctx.runToHere(block.id);
              }}
            >
              <svg viewBox="0 0 16 16" width="12" height="12" aria-hidden="true">
                <path d="M4 2l7 4-7 4z M3 12h10v1.4H3z" fill="currentColor" />
              </svg>
            </button>
            <button
              className="nls-run nls-run-sub"
              title="Run section"
              onClick={() => {
                persist();
                void ctx.runSection(block.id);
              }}
            >
              <svg viewBox="0 0 16 16" width="12" height="12" aria-hidden="true">
                <path d="M2 3h12v1.5H2z M4.5 7l6 2.5-6 2.5z" fill="currentColor" />
              </svg>
            </button>
            <span className="nls-lang">nelson</span>
          </div>
          <div className="nls-monaco" style={{ height }} onKeyDown={(e) => e.stopPropagation()}>
            <Editor
              defaultLanguage="nelson"
              defaultValue={block.props.code ?? ''}
              theme={ctx.dark ? 'vs-dark' : 'vs'}
              beforeMount={setupMonaco}
              onMount={handleMount}
              onChange={(v) => {
                codeRef.current = v ?? '';
                if ((v ?? '').split('\n').length !== lines) force((n) => n + 1);
              }}
              options={{
                minimap: { enabled: false },
                fontSize: 13,
                lineNumbers: 'on',
                lineNumbersMinChars: 2,
                folding: false,
                scrollBeyondLastLine: false,
                scrollbar: { vertical: 'auto', alwaysConsumeMouseWheel: false },
                renderLineHighlight: 'none',
                overviewRulerLanes: 0,
                padding: { top: 6, bottom: 6 },
                fontFamily: "Consolas, 'Cascadia Mono', monospace",
                automaticLayout: true,
              }}
            />
          </div>
        </div>
      )}

      {hasResult && (
        <div className="nls-result">
          <button
            className="nls-clear-output"
            title="Clear this cell's output"
            onClick={clearOutput}
          >
            Clear output
          </button>
          {errorMsg ? (
            <pre className="nls-output nls-error">Error: {errorMsg}</pre>
          ) : (
            output && <pre className="nls-output">{output}</pre>
          )}
          {figures.map((src, i) => {
            const handle = figureHandles[i];
            if (isFigureLive(handle)) {
              // Keyed by slot, not by handle, so the panel survives a re-run and
              // swaps its content in place. `reloadToken` changes on every run:
              // the panel refetches the display list and keeps the old frame
              // until it loads, so there is no blank flash.
              return (
                <div className="nls-figure-wrap nls-figure-live" key={`fig-live-${i}`}>
                  <FigurePanel
                    handle={handle}
                    title={`Figure ${handle + 1}`}
                    inline
                    reloadToken={
                      Number.isFinite(Number(block.props.runRevision))
                        ? Number(block.props.runRevision)
                        : src
                    }
                    onPopOut={() => popOutFigure(i)}
                  />
                </div>
              );
            }
            return (
              <div className="nls-figure-wrap" key={`img-${i}`}>
                <img className="nls-figure" src={src} alt="figure" />
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}
//=============================================================================
