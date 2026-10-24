//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, useState } from 'react';
import Editor, { type OnMount } from '@monaco-editor/react';
import type * as Monaco from 'monaco-editor';
import { onPush, rpc } from '../rpc/client';
import { webPrompt } from '../components/PromptHost';
import { onEngineReady, runCommand } from '../desktop/engineSession';
import { openHelp, openEditor, ensureCorePanel } from '../desktop/desktop';
import { EditorRibbon, type RibbonGroupDef } from './EditorRibbon';
import { getActiveScheme, subscribeScheme } from '../desktop/theme';
import { getPrefs, subscribePrefs } from '../desktop/prefs';
import type { BreakpointsResult, EditorOpenResult } from '../rpc/types';
import { DesktopIcon } from '../components/DesktopIcon';
import { setupMonaco } from '../editor/monacoSetup';
//=============================================================================
setupMonaco();
type DebugState = {
  running: boolean;
  currentFile: string | null;
  currentLine: number | null;
  stack: { file: string; line: number; function: string }[];
};
//=============================================================================
const emptyDebug: DebugState = {
  running: false,
  currentFile: null,
  currentLine: null,
  stack: [],
};
//=============================================================================
type AnalyzerEdit = {
  lineStart: number;
  lineEnd: number;
  columnStart: number;
  columnEnd: number;
  replacementText: string;
};
//=============================================================================
type AnalyzerFix = { title: string; safe: boolean; edits: AnalyzerEdit[] };
// One diagnostic from the backend code.analyze RPC (Nelson's CodeAnalyzer).
type AnalyzerDiagnostic = {
  severity: string; // "error" | "warning" | "info"
  message: string;
  ruleName: string;
  lineStart: number;
  lineEnd: number;
  columnStart: number;
  columnEnd: number;
  fixes?: AnalyzerFix[];
};
//=============================================================================
// Latest diagnostics per model URI, so the (global) code-action provider can
// offer each diagnostic's quick fixes.
const modelDiagnostics = new Map<string, AnalyzerDiagnostic[]>();
const samePath = (a: string, b: string) =>
  a.replace(/\\/g, '/').toLowerCase().endsWith(b.replace(/\\/g, '/').toLowerCase()) ||
  b.replace(/\\/g, '/').toLowerCase().endsWith(a.replace(/\\/g, '/').toLowerCase());
const monacoTheme = (s: 'light' | 'dark') => (s === 'dark' ? 'vs-dark' : 'light');
// Cache help(name) lookups so hover / signature help don't hammer the backend.
const helpCache = new Map<string, Promise<string>>();
function fetchHelp(name: string): Promise<string> {
  let p = helpCache.get(name);
  if (!p) {
    p = rpc<{ text?: string }>('help.text', { name })
      .then((r) => r.text ?? '')
      .catch(() => '');
    helpCache.set(name, p);
  }
  return p;
}
//=============================================================================
let nelsonRegistered = false;
function registerNelson(monaco: typeof Monaco) {
  if (nelsonRegistered) return;
  nelsonRegistered = true;
  const langs = monaco.languages.getLanguages().map((l) => l.id);
  if (!langs.includes('nelson')) {
    monaco.languages.register({ id: 'nelson', extensions: ['.m'] });
  }
  monaco.languages.setLanguageConfiguration('nelson', {
    comments: { lineComment: '%', blockComment: ['%{', '%}'] },
    brackets: [
      ['{', '}'],
      ['[', ']'],
      ['(', ')'],
    ],
    autoClosingPairs: [
      { open: '(', close: ')' },
      { open: '[', close: ']' },
      { open: '{', close: '}' },
      { open: "'", close: "'" },
      { open: '"', close: '"' },
    ],
  });

  monaco.languages.registerHoverProvider('nelson', {
    provideHover: async (model, position) => {
      const word = model.getWordAtPosition(position);
      if (!word) return null;
      const text = await fetchHelp(word.word);
      if (!text.trim()) return null;
      const body = text.split('\n').slice(0, 14).join('\n');
      return { contents: [{ value: '```text\n' + body + '\n```' }] };
    },
  });

  monaco.languages.registerSignatureHelpProvider('nelson', {
    signatureHelpTriggerCharacters: ['(', ','],
    provideSignatureHelp: async (model, position) => {
      const line = model.getLineContent(position.lineNumber).slice(0, position.column - 1);
      const m = /([A-Za-z_]\w*)\s*\([^()]*$/.exec(line);
      if (!m) return null;
      const name = m[1];
      const text = await fetchHelp(name);
      const rx = new RegExp('\\b' + name + '\\s*\\(');
      const sigs = text
        .split('\n')
        .map((l) => l.trim())
        .filter((l) => rx.test(l) && l.length < 120)
        .slice(0, 6)
        .map((label) => ({ label, parameters: [] }));
      if (!sigs.length) return null;
      const activeParameter = (line.slice(line.lastIndexOf('(') + 1).match(/,/g) ?? []).length;
      return {
        value: { signatures: sigs, activeSignature: 0, activeParameter },
        dispose: () => undefined,
      };
    },
  });

  monaco.languages.registerDocumentSymbolProvider('nelson', {
    provideDocumentSymbols: (model) => {
      const out: Monaco.languages.DocumentSymbol[] = [];
      const lines = model.getLinesContent();
      const fullRange = (ln: number) => ({
        startLineNumber: ln + 1,
        startColumn: 1,
        endLineNumber: ln + 1,
        endColumn: (lines[ln]?.length ?? 0) + 1,
      });
      for (let i = 0; i < lines.length; i++) {
        const fnm = /^\s*function\b(.*)$/.exec(lines[i]);
        if (fnm) {
          const decl = fnm[1];
          const nm = decl.includes('=')
            ? /=\s*([A-Za-z_]\w*)/.exec(decl)?.[1]
            : /^\s*([A-Za-z_]\w*)/.exec(decl)?.[1];
          if (nm) {
            out.push({
              name: nm,
              detail: 'function',
              kind: monaco.languages.SymbolKind.Function,
              tags: [],
              range: fullRange(i),
              selectionRange: fullRange(i),
            });
          }
          continue;
        }
        const sec = /^\s*%%\s?(.*)$/.exec(lines[i]);
        if (sec) {
          out.push({
            name: sec[1].trim() || 'section',
            detail: 'section',
            kind: monaco.languages.SymbolKind.Namespace,
            tags: [],
            range: fullRange(i),
            selectionRange: fullRange(i),
          });
        }
      }
      return out;
    },
  });

  monaco.languages.registerCodeActionProvider('nelson', {
    provideCodeActions: (model, range) => {
      const diags = modelDiagnostics.get(model.uri.toString()) ?? [];
      const actions: Monaco.languages.CodeAction[] = [];
      for (const d of diags) {
        if (!d.fixes || d.fixes.length === 0) continue;
        const line = Math.max(1, d.lineStart || 1);
        if (line < range.startLineNumber || line > range.endLineNumber) continue;
        for (const fix of d.fixes) {
          actions.push({
            title: fix.title || `Fix: ${d.ruleName}`,
            kind: 'quickfix',
            isPreferred: fix.safe,
            edit: {
              edits: fix.edits.map((e) => ({
                resource: model.uri,
                versionId: undefined,
                textEdit: {
                  range: {
                    startLineNumber: Math.max(1, e.lineStart || line),
                    startColumn: e.columnStart > 0 ? e.columnStart : 1,
                    endLineNumber: Math.max(1, e.lineEnd || e.lineStart || line),
                    endColumn:
                      e.columnEnd > 0
                        ? e.columnEnd
                        : model.getLineMaxColumn(Math.max(1, e.lineEnd || e.lineStart || line)),
                  },
                  text: e.replacementText,
                },
              })),
            },
          });
        }
      }
      return { actions, dispose: () => undefined };
    },
  });

  monaco.languages.registerCompletionItemProvider('nelson', {
    triggerCharacters: ['.', '/'],
    provideCompletionItems: async (model, position) => {
      const line = model.getValueInRange({
        startLineNumber: position.lineNumber,
        startColumn: 1,
        endLineNumber: position.lineNumber,
        endColumn: position.column,
      });
      const res = await rpc<{
        prefix?: string;
        builtin?: string[];
        macros?: string[];
        variables?: string[];
        files?: string[];
      }>('completion.request', { line, cursor: line.length }).catch(() => null);
      if (!res) return { suggestions: [] };
      const prefix = res.prefix ?? '';
      const range = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: position.column - prefix.length,
        endColumn: position.column,
      };
      const K = monaco.languages.CompletionItemKind;
      const mk = (label: string, kind: Monaco.languages.CompletionItemKind) => ({
        label,
        kind,
        insertText: label,
        range,
      });
      const suggestions = [
        ...(res.variables ?? []).map((v) => mk(v, K.Variable)),
        ...(res.builtin ?? []).map((v) => mk(v, K.Function)),
        ...(res.macros ?? []).map((v) => mk(v, K.Method)),
        ...(res.files ?? []).map((v) => mk(v, K.File)),
      ];
      return { suggestions };
    },
  });
}
//=============================================================================
export function EditorPanel({ path }: { path: string }): JSX.Element {
  const [content, setContent] = useState<string>('');
  const [language, setLanguage] = useState<string>('plaintext');
  const [ready, setReady] = useState(false);
  const [scheme, setScheme] = useState<'light' | 'dark'>(getActiveScheme());
  const debugRef = useRef<DebugState>(emptyDebug);
  const [, forcePaint] = useState(0);
  const debug = debugRef.current;
  const editorRef = useRef<Monaco.editor.IStandaloneCodeEditor | null>(null);
  const monacoRef = useRef<typeof Monaco | null>(null);
  const decorationsRef = useRef<string[]>([]);
  const debugDecoRef = useRef<string[]>([]);
  const prefsOffRef = useRef<(() => void) | null>(null);
  const analyzeTimerRef = useRef<number | undefined>(undefined);
  const [issues, setIssues] = useState<{ errors: number; warnings: number; infos: number }>({
    errors: 0,
    warnings: 0,
    infos: 0,
  });
  const [diagnostics, setDiagnostics] = useState<AnalyzerDiagnostic[]>([]);
  const [showProblems, setShowProblems] = useState(false);
  const [wrap, setWrap] = useState(false);
  const toggleWrap = () => {
    setWrap((w) => {
      const next = !w;
      editorRef.current?.updateOptions({ wordWrap: next ? 'on' : 'off' });
      return next;
    });
  };
  useEffect(
    () => () => {
      prefsOffRef.current?.();
      if (analyzeTimerRef.current) window.clearTimeout(analyzeTimerRef.current);
    },
    []
  );

  useEffect(() => {
    rpc<EditorOpenResult>('editor.open', { path })
      .then((r) => {
        setContent(r.content);
        setLanguage(r.language);
        setReady(true);
      })
      .catch(() => setReady(true));
  }, [path]);

  useEffect(() => subscribeScheme(setScheme), []);

  useEffect(() => {
    const onGoto = (e: Event) => {
      const d = (e as CustomEvent<{ path?: string; line?: number }>).detail;
      const ed = editorRef.current;
      if (!ed || !d?.line || !d.path || !samePath(path, d.path)) return;
      ed.revealLineInCenter(d.line);
      ed.setPosition({ lineNumber: d.line, column: 1 });
      ed.focus();
    };
    window.addEventListener('nelson-webview.editor-goto', onGoto as EventListener);
    return () => window.removeEventListener('nelson-webview.editor-goto', onGoto as EventListener);
  }, [path]);

  // Re-query the debugger position on every prompt change (a breakpoint pause
  // pushes a fresh K>> prompt), stored in a ref and painted by a timer pump.
  useEffect(() => {
    let alive = true;
    let last = '';
    const pull = async () => {
      try {
        debugRef.current = (await rpc<DebugState>('debugger.state')) ?? emptyDebug;
      } catch {
        debugRef.current = emptyDebug;
      }
    };
    void pull();
    const off = onEngineReady(() => void pull());
    const offDebug = onPush('debugger.state', (state) => {
      debugRef.current = (state as DebugState) ?? emptyDebug;
    });
    // Paint pump: a synchronous setState inside a timer callback reliably
    // flushes dockview's portal, whereas an async setState from the push's
    // microtask chain does not. Only re-render when the position actually
    // changed, so the idle cost is a cheap identity check.
    const iv = setInterval(() => {
      if (!alive) return;
      const sig = `${debugRef.current.running}:${debugRef.current.currentFile}:${debugRef.current.currentLine}`;
      if (sig !== last) {
        last = sig;
        forcePaint((n) => n + 1);
      }
    }, 250);
    return () => {
      alive = false;
      off();
      offDebug();
      clearInterval(iv);
    };
  }, []);

  useEffect(() => {
    const editor = editorRef.current;
    const monaco = monacoRef.current;
    if (!editor || !monaco) return;
    const here =
      debug.running && debug.currentFile && debug.currentLine && samePath(path, debug.currentFile)
        ? debug.currentLine
        : null;
    debugDecoRef.current = editor.deltaDecorations(
      debugDecoRef.current,
      here
        ? [
            {
              range: new monaco.Range(here, 1, here, 1),
              options: {
                isWholeLine: true,
                className: 'debug-current-line',
                glyphMarginClassName: 'debug-current-glyph',
              },
            },
          ]
        : []
    );
    if (here) editor.revealLineInCenter(here);
  }, [debug, path]);

  // Run the code analyzer on the live buffer and show the diagnostics as Monaco
  // markers. Only meaningful for Nelson (.m) source.
  const runAnalyze = () => {
    const editor = editorRef.current;
    const monaco = monacoRef.current;
    if (!editor || !monaco) return;
    const model = editor.getModel();
    if (!model) return;
    if (!/\.m$/i.test(path)) {
      monaco.editor.setModelMarkers(model, 'nelson-analyzer', []);
      setIssues({ errors: 0, warnings: 0, infos: 0 });
      setDiagnostics([]);
      return;
    }
    const severityOf = (s: string): Monaco.MarkerSeverity =>
      s === 'error'
        ? monaco.MarkerSeverity.Error
        : s === 'warning'
        ? monaco.MarkerSeverity.Warning
        : monaco.MarkerSeverity.Info;
    rpc<{ diagnostics: AnalyzerDiagnostic[] }>('code.analyze', {
      path,
      source: model.getValue(),
    })
      .then((r) => {
        const diags = r.diagnostics ?? [];
        const markers: Monaco.editor.IMarkerData[] = diags.map((d) => {
          const line = Math.max(1, d.lineStart || 1);
          const endLine = Math.max(line, d.lineEnd || line);
          // The analyzer gives 0 columns when it flags a whole line; span it.
          const startCol = d.columnStart > 0 ? d.columnStart : 1;
          const endCol =
            d.columnEnd > d.columnStart ? d.columnEnd : model.getLineMaxColumn(endLine);
          return {
            severity: severityOf(d.severity),
            message: d.ruleName ? `${d.message} (${d.ruleName})` : d.message,
            startLineNumber: line,
            startColumn: startCol,
            endLineNumber: endLine,
            endColumn: endCol,
            source: 'Nelson',
          };
        });
        monaco.editor.setModelMarkers(model, 'nelson-analyzer', markers);
        modelDiagnostics.set(model.uri.toString(), diags);
        setDiagnostics(diags);
        setIssues({
          errors: diags.filter((d) => d.severity === 'error').length,
          warnings: diags.filter((d) => d.severity === 'warning').length,
          infos: diags.filter((d) => d.severity !== 'error' && d.severity !== 'warning').length,
        });
      })
      .catch(() => undefined);
  };

  const scheduleAnalyze = () => {
    if (analyzeTimerRef.current) window.clearTimeout(analyzeTimerRef.current);
    analyzeTimerRef.current = window.setTimeout(runAnalyze, 500);
  };

  const renderBreakpoints = (lines: number[]) => {
    const editor = editorRef.current;
    const monaco = monacoRef.current;
    if (!editor || !monaco) return;
    decorationsRef.current = editor.deltaDecorations(
      decorationsRef.current,
      lines.map((line) => ({
        range: new monaco.Range(line, 1, line, 1),
        options: { isWholeLine: false, glyphMarginClassName: 'bp-glyph' },
      }))
    );
  };

  // Send the current selection (or, if empty, the current line) to the engine,
  // one line at a time; Nelson's getLine reads a single line per prompt.
  const evaluateSelection = () => {
    const editor = editorRef.current;
    if (!editor) return;
    const model = editor.getModel();
    const sel = editor.getSelection();
    if (!model || !sel) return;
    const text = sel.isEmpty()
      ? model.getLineContent(sel.positionLineNumber)
      : model.getValueInRange(sel);
    for (const line of text.split('\n')) {
      if (line.trim()) void runCommand(line);
    }
  };

  const runSection = () => {
    const editor = editorRef.current;
    if (!editor) return;
    const model = editor.getModel();
    const pos = editor.getPosition();
    if (!model || !pos) return;
    const total = model.getLineCount();
    const isSep = (n: number) => /^\s*%%/.test(model.getLineContent(n));
    let start = pos.lineNumber;
    while (start > 1 && !isSep(start)) start--;
    if (isSep(start)) start++;
    let end = pos.lineNumber + 1;
    while (end <= total && !isSep(end)) end++;
    for (let n = start; n < end; n++) {
      const line = model.getLineContent(n);
      if (line.trim() && !/^\s*%/.test(line)) void runCommand(line);
    }
  };

  const runFile = () => {
    // Persist first so `run` sees the latest edits, then execute the file.
    const editor = editorRef.current;
    if (!editor) return;
    const escaped = path.replace(/'/g, "''");
    rpc('editor.save', { path, content: editor.getValue() })
      .then(() => runCommand(`run('${escaped}')`))
      .catch(() => undefined);
  };

  const save = () => {
    const editor = editorRef.current;
    if (!editor) return;
    rpc('editor.save', { path, content: editor.getValue() }).catch(() => undefined);
    runAnalyze();
  };

  const saveAs = async () => {
    const editor = editorRef.current;
    if (!editor) return;
    const target = await webPrompt('Save as (path):', path);
    if (!target || target === path) return;
    rpc('editor.save', { path: target, content: editor.getValue() })
      .then(() => openEditor(target))
      .catch(() => undefined);
  };

  const act = (id: string) => editorRef.current?.getAction(id)?.run();
  const find = () => act('actions.find');
  const gotoLine = () => act('editor.action.gotoLine');
  const gotoSymbol = () => act('editor.action.quickOutline');
  const commentToggle = () => act('editor.action.commentLine');
  const format = () => act('editor.action.formatDocument');
  const insertSection = () => {
    const editor = editorRef.current;
    const pos = editor?.getPosition();
    if (!editor || !pos) return;
    editor.executeEdits('nelson-section', [
      {
        range: {
          startLineNumber: pos.lineNumber,
          startColumn: 1,
          endLineNumber: pos.lineNumber,
          endColumn: 1,
        },
        text: '%% \n',
      },
    ]);
    editor.setPosition({ lineNumber: pos.lineNumber, column: 4 });
    editor.focus();
  };
  const newScript = () => openEditor('untitled.m');

  const applyFix = (d: AnalyzerDiagnostic) => {
    const editor = editorRef.current;
    const model = editor?.getModel();
    if (!editor || !model || !d.fixes?.length) return;
    const fix = d.fixes[0];
    editor.executeEdits(
      'nelson-quickfix',
      fix.edits.map((e) => {
        const sl = Math.max(1, e.lineStart || d.lineStart || 1);
        const el = Math.max(1, e.lineEnd || e.lineStart || sl);
        return {
          range: {
            startLineNumber: sl,
            startColumn: e.columnStart > 0 ? e.columnStart : 1,
            endLineNumber: el,
            endColumn: e.columnEnd > 0 ? e.columnEnd : model.getLineMaxColumn(el),
          },
          text: e.replacementText,
        };
      })
    );
    editor.focus();
    scheduleAnalyze();
  };

  const gotoDiagnostic = (d: AnalyzerDiagnostic) => {
    const editor = editorRef.current;
    if (!editor) return;
    const line = Math.max(1, d.lineStart || 1);
    const col = d.columnStart > 0 ? d.columnStart : 1;
    editor.revealLineInCenter(line);
    editor.setPosition({ lineNumber: line, column: col });
    editor.focus();
  };

  const onMount: OnMount = (editor, monaco) => {
    editorRef.current = editor;
    monacoRef.current = monaco;
    registerNelson(monaco);

    rpc<BreakpointsResult>('debugger.getBreakpoints', { file: path })
      .then((r) => renderBreakpoints(r.lines))
      .catch(() => undefined);

    runAnalyze();

    editor.onMouseDown((e) => {
      if (e.target.type === monaco.editor.MouseTargetType.GUTTER_GLYPH_MARGIN) {
        const line = e.target.position?.lineNumber;
        if (!line) return;
        rpc<BreakpointsResult>('debugger.toggleBreakpoint', { file: path, line })
          .then((r) => renderBreakpoints(r.lines))
          .catch(() => undefined);
      }
    });

    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, save);
    editor.addAction({
      id: 'nelson.evaluate',
      label: 'Evaluate Selection',
      keybindings: [monaco.KeyCode.F9],
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 1,
      run: evaluateSelection,
    });
    editor.addAction({
      id: 'nelson.section',
      label: 'Run Section',
      keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 2,
      run: runSection,
    });
    editor.addAction({
      id: 'nelson.run',
      label: 'Run File',
      keybindings: [monaco.KeyCode.F5],
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 3,
      run: runFile,
    });
    editor.addAction({
      id: 'nelson.help',
      label: 'Help on Selection',
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 4,
      run: (ed) => {
        const m = ed.getModel();
        const sel = ed.getSelection();
        let word = '';
        if (m && sel) {
          word = sel.isEmpty()
            ? m.getWordAtPosition(sel.getPosition())?.word ?? ''
            : m.getValueInRange(sel);
        }
        if (word.trim()) openHelp(word.trim());
      },
    });
    editor.addAction({
      id: 'nelson.breakpoint',
      label: 'Toggle Breakpoint',
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 5,
      run: (ed) => {
        const pos = ed.getPosition();
        if (!pos) return;
        rpc<BreakpointsResult>('debugger.toggleBreakpoint', { file: path, line: pos.lineNumber })
          .then((r) => renderBreakpoints(r.lines))
          .catch(() => undefined);
      },
    });
    editor.addAction({
      id: 'nelson.clearBreakpoints',
      label: 'Clear All Breakpoints',
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 6,
      run: () => {
        void runCommand('dbclear all');
        setTimeout(
          () =>
            rpc<BreakpointsResult>('debugger.getBreakpoints', { file: path })
              .then((r) => renderBreakpoints(r.lines))
              .catch(() => undefined),
          200
        );
      },
    });
    editor.addAction({
      id: 'nelson.gotoSymbol',
      label: 'Go to Function/Section…',
      keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyO],
      contextMenuGroupId: 'navigation',
      contextMenuOrder: 1.5,
      run: (ed) => ed.getAction('editor.action.quickOutline')?.run(),
    });

    // Apply font/tab preferences imperatively (no React re-render → avoids the
    // dockview portal paint quirk) and keep them in sync with Preferences.
    const applyPrefs = () => {
      const p = getPrefs();
      editor.updateOptions({ fontSize: p.editorFontSize, tabSize: p.editorTabSize });
    };
    applyPrefs();
    prefsOffRef.current = subscribePrefs(applyPrefs);
  };

  if (!ready) return <div className="browser__empty">loading…</div>;

  const totalIssues = issues.errors + issues.warnings + issues.infos;
  const ribbonGroups: RibbonGroupDef[] = [
    {
      id: 'file',
      label: 'FILE',
      priority: 10, // collapses last (most important)
      actions: [
        {
          key: 'new',
          label: (
            <>
              <DesktopIcon name="file" size={16} /> New
            </>
          ),
          title: 'New script',
          onClick: newScript,
        },
        {
          key: 'save',
          label: (
            <>
              <DesktopIcon name="save" size={16} /> Save
            </>
          ),
          title: 'Save (Ctrl+S)',
          onClick: save,
        },
        { key: 'saveas', label: 'Save As', title: 'Save As…', onClick: saveAs },
      ],
    },
    {
      id: 'navigate',
      label: 'NAVIGATE',
      priority: 1,
      actions: [
        {
          key: 'find',
          label: (
            <>
              <DesktopIcon name="search" size={16} /> Find
            </>
          ),
          title: 'Find / Replace (Ctrl+F)',
          onClick: find,
        },
        { key: 'goto', label: 'Go To', title: 'Go to line (Ctrl+G)', onClick: gotoLine },
        {
          key: 'sym',
          label: 'Symbols',
          title: 'Go to function/section (Ctrl+Shift+O)',
          onClick: gotoSymbol,
        },
      ],
    },
    {
      id: 'code',
      label: 'CODE',
      priority: 2,
      actions: [
        {
          key: 'comment',
          label: '% Comment',
          title: 'Comment / Uncomment (Ctrl+/)',
          onClick: commentToggle,
        },
        { key: 'format', label: 'Format', title: 'Format document', onClick: format },
        {
          key: 'wrap',
          label: 'Wrap',
          title: 'Toggle word wrap',
          onClick: toggleWrap,
          className: wrap ? 'ribbon-btn--on' : '',
        },
      ],
    },
    {
      id: 'analyze',
      label: 'ANALYZE',
      priority: 3,
      actions: [
        {
          key: 'issues',
          className: 'editor-toolbar__issues' + (showProblems ? ' ribbon-btn--on' : ''),
          title: 'Show code issues',
          onClick: () => setShowProblems((v) => !v),
          label:
            totalIssues > 0 ? (
              <>
                {issues.errors > 0 && (
                  <span className="editor-issues__err">
                    <DesktopIcon name="error" size={16} /> {issues.errors}
                  </span>
                )}
                {issues.warnings > 0 && (
                  <span className="editor-issues__warn">
                    <DesktopIcon name="warning" size={16} /> {issues.warnings}
                  </span>
                )}
                {issues.infos > 0 && (
                  <span className="editor-issues__info">
                    <DesktopIcon name="info" size={16} /> {issues.infos}
                  </span>
                )}
                {' Issues'}
              </>
            ) : (
              <>
                <DesktopIcon name="check" size={16} /> No issues
              </>
            ),
          disabled: totalIssues === 0,
        },
        {
          key: 'prof',
          label: (
            <>
              <DesktopIcon name="profiler" size={16} /> Profiler
            </>
          ),
          title: 'Profiler',
          onClick: () => ensureCorePanel('profiler'),
        },
      ],
    },
    {
      id: 'section',
      label: 'SECTION',
      priority: 4,
      actions: [
        {
          key: 'break',
          label: '%% Break',
          title: 'Insert section break (%%)',
          onClick: insertSection,
        },
        {
          key: 'runsec',
          label: (
            <>
              <DesktopIcon name="execute" size={16} /> Section
            </>
          ),
          title: 'Run section (Ctrl+Enter)',
          onClick: runSection,
        },
        {
          key: 'eval',
          label: (
            <>
              <DesktopIcon name="execute" size={16} /> Evaluate
            </>
          ),
          title: 'Evaluate selection (F9)',
          onClick: evaluateSelection,
        },
      ],
    },
    {
      id: 'run',
      label: 'RUN',
      priority: 9, // collapses near-last
      actions: [
        {
          key: 'run',
          label: (
            <>
              <DesktopIcon name="execute" size={16} /> Run
            </>
          ),
          title: 'Run file (F5)',
          onClick: runFile,
          className: 'ribbon-btn--run',
        },
      ],
    },
  ];

  return (
    <div className="editor-panel">
      <EditorRibbon groups={ribbonGroups} />
      {debug.running && (
        <div className="editor-debugbar">
          <span className="editor-debugbar__label">
            Paused
            {debug.stack[0]
              ? ` · ${debug.stack[0].function || debug.stack[0].file}:${debug.currentLine}`
              : ''}
          </span>
          <span className="editor-toolbar__spacer" />
          <button
            className="editor-toolbar__btn"
            title="Step over (dbstep)"
            onClick={() => void runCommand('dbstep')}
          >
            Step
          </button>
          <button
            className="editor-toolbar__btn"
            title="Step in (dbstep in)"
            onClick={() => void runCommand('dbstep in')}
          >
            In
          </button>
          <button
            className="editor-toolbar__btn"
            title="Step out (dbstep out)"
            onClick={() => void runCommand('dbstep out')}
          >
            Out
          </button>
          <button
            className="editor-toolbar__btn"
            title="Continue (dbcont)"
            onClick={() => void runCommand('dbcont')}
          >
            <DesktopIcon name="execute" size={16} /> Continue
          </button>
          <button
            className="editor-toolbar__btn"
            title="Quit debugging (dbquit)"
            onClick={() => void runCommand('dbquit')}
          >
            <DesktopIcon name="stop" size={16} /> Quit
          </button>
        </div>
      )}
      <div className="editor-panel__body">
        <Editor
          theme={monacoTheme(scheme)}
          language={language}
          value={content}
          onMount={onMount}
          onChange={scheduleAnalyze}
          options={{
            glyphMargin: true,
            minimap: { enabled: false },
            mouseWheelZoom: false,
            fontSize: getPrefs().editorFontSize,
            tabSize: getPrefs().editorTabSize,
            automaticLayout: true,
            quickSuggestions: { other: true, comments: false, strings: false },
            suggestOnTriggerCharacters: true,
            wordBasedSuggestions: 'off',
            contextmenu: true,
            // Render the context menu / suggestion widgets in a body-level fixed
            // container so the dockview panel's overflow:hidden can't clip them.
            fixedOverflowWidgets: true,
          }}
        />
      </div>
      {showProblems && diagnostics.length > 0 && (
        <div className="editor-problems">
          <div className="editor-problems__head">
            <span>
              {diagnostics.length} issue{diagnostics.length > 1 ? 's' : ''}
            </span>
            <button
              className="editor-problems__close"
              title="Hide"
              onClick={() => setShowProblems(false)}
            >
              <DesktopIcon name="close" size={16} />
            </button>
          </div>
          <ul className="editor-problems__list">
            {diagnostics.map((d, i) => (
              <li
                key={i}
                className="editor-problems__item"
                onClick={() => gotoDiagnostic(d)}
                title="Go to line"
              >
                <span className={'editor-problems__sev editor-problems__sev--' + d.severity}>
                  <DesktopIcon
                    name={
                      d.severity === 'error'
                        ? 'error'
                        : d.severity === 'warning'
                        ? 'warning'
                        : 'info'
                    }
                    size={16}
                  />
                </span>
                <span className="editor-problems__msg">{d.message}</span>
                {d.ruleName ? <span className="editor-problems__rule">{d.ruleName}</span> : null}
                {d.fixes && d.fixes.length > 0 ? (
                  <button
                    className="editor-problems__fix"
                    title={d.fixes[0].title || 'Apply fix'}
                    onClick={(e) => {
                      e.stopPropagation();
                      applyFix(d);
                    }}
                  >
                    Fix
                  </button>
                ) : null}
                <span className="editor-problems__loc">Ln {d.lineStart || 1}</span>
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
//=============================================================================
