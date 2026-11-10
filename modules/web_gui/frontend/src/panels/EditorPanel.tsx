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
import { requestSavePath } from '../components/DesktopFilePickerHost';
import { baseName, dirName } from '../desktop/paths';
import { isEngineBusy, onEngineReady, runCommand, subscribeEngine } from '../desktop/engineSession';
import { openHelp, openEditor, ensureCorePanel } from '../desktop/desktop';
import { EditorRibbon, type RibbonGroupDef } from './EditorRibbon';
import { getActiveScheme, subscribeScheme } from '../desktop/theme';
import { getPrefs, subscribePrefs } from '../desktop/prefs';
import type {
  AllBreakpointsResult,
  BreakpointInfo,
  BreakpointsResult,
  DebuggerCommand,
  DebuggerEvaluateResult,
  DebuggerState,
  EditorFileStamp,
  EditorOpenResult,
  EditorSaveResult,
} from '../rpc/types';
import { DesktopIcon } from '../components/DesktopIcon';
import { BreakpointPopover } from '../components/BreakpointPopover';
import { setupMonaco } from '../editor/monacoSetup';
import {
  breakpointDecoration,
  breakpointMarkerState,
  breakpointMoves,
  dataTipExpression,
  debugLineDecorations,
  pendingBreakpoints,
  samePath,
  type EditorBreakpoint,
} from '../editor/debugDecorations';
import { subscribeTranslations, t, tf } from '../i18n';
import { externalChangeAction, rpcFailureReason } from '../editor/externalChange';
//=============================================================================
setupMonaco();
//=============================================================================
const emptyDebug: DebuggerState = {
  running: false,
  paused: false,
  currentFile: null,
  currentLine: null,
  currentColumn: 0,
  function: '',
  reason: 'none',
  errorIdentifier: '',
  errorMessage: '',
  canStep: true,
  stack: [],
  revision: 0,
  breakpointsRevision: 0,
};
// latest debugger state revision: data tips are cached per revision
let latestDebugRevision = 0;
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
const dataTipCache = new Map<string, Promise<string>>();
function fetchDataTip(expr: string): Promise<string> {
  const key = `${latestDebugRevision}:${expr}`;
  let p = dataTipCache.get(key);
  if (!p) {
    if (dataTipCache.size > 200) dataTipCache.clear();
    p = rpc<DebuggerEvaluateResult>('debugger.evaluate', { expr })
      .then((r) => (r.ok && r.text ? r.text : ''))
      .catch(() => '');
    dataTipCache.set(key, p);
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
      const contents: Monaco.IMarkdownString[] = [];
      // data tip: the value of the variable under the pointer while the engine waits at a prompt
      const expr = dataTipExpression(model.getLineContent(position.lineNumber), word);
      if (expr && !isEngineBusy()) {
        const tip = await fetchDataTip(expr);
        // a variable is not a help topic: the tip alone, without a help() round trip
        if (tip) return { contents: [{ value: '```text\n' + tip + '\n```' }] };
      }
      const text = await fetchHelp(word.word);
      if (text.trim()) {
        const body = text.split('\n').slice(0, 14).join('\n');
        contents.push({ value: '```text\n' + body + '\n```' });
      }
      return contents.length ? { contents } : null;
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
            name: sec[1].trim() || t('section'),
            detail: t('section'),
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
            title: fix.title || tf('Fix: %1', d.ruleName),
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
export function EditorPanel({
  path,
  line: initialLine,
  column: initialColumn,
}: {
  path: string;
  line?: number;
  column?: number;
}) {
  const [content, setContent] = useState<string>('');
  const [language, setLanguage] = useState<string>('plaintext');
  const [ready, setReady] = useState(false);
  const [scheme, setScheme] = useState<'light' | 'dark'>(getActiveScheme());
  const debugRef = useRef<DebuggerState>(emptyDebug);
  const [, forcePaint] = useState(0);
  const debug = debugRef.current;
  const editorRef = useRef<Monaco.editor.IStandaloneCodeEditor | null>(null);
  const monacoRef = useRef<typeof Monaco | null>(null);
  // breakpoints of this file; evaluatorLine 0 marks one added while modified (pending save)
  const bpRef = useRef<EditorBreakpoint[]>([]);
  const savedVersionRef = useRef(0);
  const dirtyRef = useRef(false);
  const debugDecoRef = useRef<string[]>([]);
  const [popover, setPopover] = useState<{ line: number; x: number; y: number } | null>(null);
  const [conditionError, setConditionError] = useState('');
  const [errorBreakpoints, setErrorBreakpoints] = useState({ error: false, caughtError: false });
  const [notice, setNotice] = useState('');
  // the file on disk as last opened, saved or kept; checks compare against it
  const stampRef = useRef<EditorFileStamp | null>(null);
  const diskStampRef = useRef<EditorFileStamp | null>(null);
  const savingRef = useRef(0);
  const checkGenRef = useRef(0);
  const [diskChanged, setDiskChanged] = useState(false);
  const noticeTimerRef = useRef<number | undefined>(undefined);
  const [engineBusy, setEngineBusy] = useState(isEngineBusy());
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
      if (noticeTimerRef.current) window.clearTimeout(noticeTimerRef.current);
    },
    []
  );
  useEffect(() => subscribeEngine(setEngineBusy), []);
  useEffect(() => subscribeTranslations(() => forcePaint((n) => n + 1)), []);

  // breakpoints: red, light gray disabled, dark gray while unsaved, orange conditional
  const showNotice = (text: string) => {
    setNotice(text);
    if (noticeTimerRef.current) window.clearTimeout(noticeTimerRef.current);
    noticeTimerRef.current = window.setTimeout(() => setNotice(''), 6000);
  };
  const showWriteError = (file: string, error: unknown) => {
    const reason = rpcFailureReason(error);
    showNotice(`${t('Cannot write file: ')}${file}${reason ? ` (${t(reason)})` : ''}`);
  };
  const currentLineOf = (decorationId: string): number | null => {
    const model = editorRef.current?.getModel();
    return model?.getDecorationRange(decorationId)?.startLineNumber ?? null;
  };
  // repaints the gutter from items; lines gives each item's line (decoration line when omitted)
  const paintBreakpoints = (items: EditorBreakpoint[], lines?: number[]) => {
    const editor = editorRef.current;
    const monaco = monacoRef.current;
    if (!editor || !monaco) return;
    const placed: { item: EditorBreakpoint; line: number }[] = [];
    items.forEach((item, index) => {
      const line = lines ? lines[index] : currentLineOf(item.decorationId);
      if (line) placed.push({ item, line });
    });
    const ids = editor.deltaDecorations(
      bpRef.current.map((b) => b.decorationId).filter(Boolean),
      placed.map(({ item, line }) =>
        breakpointDecoration(monaco, line, item, breakpointMarkerState(item, dirtyRef.current))
      )
    );
    bpRef.current = placed.map(({ item }, index) => ({
      ...item,
      decorationId: ids[index] ?? '',
    }));
  };
  // replaces the tracked list with the evaluator's; pending breakpoints and edited lines are kept
  const acceptBreakpoints = (list: BreakpointInfo[] | undefined, fallbackLines?: number[]) => {
    const infos: BreakpointInfo[] =
      list ?? (fallbackLines ?? []).map((line) => ({ id: 0, line, enabled: true, condition: '' }));
    const known = new Map<number, EditorBreakpoint>();
    for (const item of bpRef.current) if (item.evaluatorLine) known.set(item.evaluatorLine, item);
    const items: EditorBreakpoint[] = [];
    const lines: number[] = [];
    for (const info of infos) {
      const prev = known.get(info.line);
      const line =
        (prev && dirtyRef.current ? currentLineOf(prev.decorationId) : null) ?? info.line;
      items.push({
        decorationId: '',
        evaluatorLine: info.line,
        enabled: info.enabled,
        condition: info.condition ?? '',
      });
      lines.push(line);
    }
    for (const item of bpRef.current) {
      if (item.evaluatorLine) continue;
      const line = currentLineOf(item.decorationId);
      if (line) {
        items.push(item);
        lines.push(line);
      }
    }
    paintBreakpoints(items, lines);
  };
  const syncBreakpoints = () =>
    rpc<BreakpointsResult>('debugger.getBreakpoints', { file: path })
      .then((r) => acceptBreakpoints(r.breakpoints, r.lines))
      .catch(() => undefined);
  const refreshErrorBreakpoints = () =>
    rpc<AllBreakpointsResult>('debugger.listBreakpoints', {})
      .then((r) => {
        if (r?.errorBreakpoints) setErrorBreakpoints(r.errorBreakpoints);
      })
      .catch(() => undefined);
  const trackedAt = (line: number) =>
    bpRef.current.find((item) => currentLineOf(item.decorationId) === line);
  const addPendingBreakpoint = (line: number, condition: string) => {
    const editor = editorRef.current;
    const monaco = monacoRef.current;
    if (!editor || !monaco) return;
    const [id] = editor.deltaDecorations(
      [],
      [breakpointDecoration(monaco, line, { enabled: true, condition }, 'invalid')]
    );
    bpRef.current = [
      ...bpRef.current,
      { decorationId: id, evaluatorLine: 0, enabled: true, condition },
    ];
  };
  // Set/Clear Breakpoint; on a modified file a new breakpoint waits for the save
  const toggleBreakpointAt = (line: number) => {
    const existing = trackedAt(line);
    if (existing && !existing.evaluatorLine) {
      paintBreakpoints(bpRef.current.filter((item) => item !== existing));
      return;
    }
    if (dirtyRef.current) {
      if (existing) {
        rpc<BreakpointsResult>('debugger.removeBreakpoint', {
          file: path,
          line: existing.evaluatorLine,
        })
          .then((r) => acceptBreakpoints(r.breakpoints, r.lines))
          .catch(() => undefined);
      } else {
        addPendingBreakpoint(line, '');
      }
      return;
    }
    rpc<BreakpointsResult>('debugger.toggleBreakpoint', { file: path, line })
      .then((r) => {
        acceptBreakpoints(r.breakpoints, r.lines);
        if (r.error) showNotice(r.error);
      })
      .catch(() => undefined);
  };
  // Set/Modify Condition: the evaluator validates the expression
  const setConditionAt = (line: number, condition: string) => {
    const existing = trackedAt(line);
    if (existing && !existing.evaluatorLine) {
      existing.condition = condition;
      paintBreakpoints(bpRef.current);
      setPopover(null);
      return;
    }
    if (!existing && dirtyRef.current) {
      addPendingBreakpoint(line, condition);
      setPopover(null);
      return;
    }
    const target = existing ? existing.evaluatorLine : line;
    rpc<BreakpointsResult>('debugger.setBreakpoint', { file: path, line: target, condition })
      .then((r) => {
        if (r.error) {
          setConditionError(r.error);
          return;
        }
        acceptBreakpoints(r.breakpoints, r.lines);
        setConditionError('');
        setPopover(null);
      })
      .catch(() => undefined);
  };
  const setEnabledAt = (line: number, enabled: boolean) => {
    const existing = trackedAt(line);
    if (!existing) return;
    if (!existing.evaluatorLine) {
      existing.enabled = enabled;
      paintBreakpoints(bpRef.current);
      return;
    }
    rpc<BreakpointsResult>('debugger.setBreakpoint', {
      file: path,
      line: existing.evaluatorLine,
      enabled,
    })
      .then((r) => acceptBreakpoints(r.breakpoints, r.lines))
      .catch(() => undefined);
  };
  const setAllEnabledInFile = (enabled: boolean) => {
    for (const item of bpRef.current) if (!item.evaluatorLine) item.enabled = enabled;
    rpc('debugger.setAllEnabled', { file: path, enabled })
      .then(() => syncBreakpoints())
      .catch(() => undefined);
  };
  const clearAllInFile = () => {
    paintBreakpoints([]);
    rpc('debugger.removeAllBreakpoints', { file: path })
      .then(() => syncBreakpoints())
      .catch(() => undefined);
  };
  const clearAllBreakpoints = () => {
    paintBreakpoints([]);
    rpc('debugger.removeAllBreakpoints', {})
      .then(() => syncBreakpoints())
      .catch(() => undefined);
  };
  const setErrorBreakpointFlags = (next: { error: boolean; caughtError: boolean }) => {
    setErrorBreakpoints(next);
    rpc<AllBreakpointsResult>('debugger.setErrorBreakpoints', next)
      .then((r) => {
        if (r?.errorBreakpoints) setErrorBreakpoints(r.errorBreakpoints);
      })
      .catch(() => undefined);
  };
  // debugger commands: dbstep, dbcont, dbquit, pause, run to here
  const debugCommand = (cmd: DebuggerCommand, extra: Record<string, unknown> = {}) =>
    rpc<{ ok?: boolean; error?: string }>('debugger.command', { cmd, ...extra })
      .then((r) => {
        if (r?.error) showNotice(r.error);
      })
      .catch(() => undefined);
  // one step per stop: a held key or a double click must not queue a burst of steps
  const steppedAtRevisionRef = useRef(-1);
  const stepCommand = (cmd: DebuggerCommand) => {
    const d = debugRef.current;
    if (!d.paused || !d.canStep) return Promise.resolve();
    if (steppedAtRevisionRef.current === d.revision) return Promise.resolve();
    steppedAtRevisionRef.current = d.revision;
    return debugCommand(cmd);
  };
  const stopDebugging = () => {
    if (!debugRef.current.running) return Promise.resolve();
    return debugCommand('quit');
  };
  const cursorLine = () => editorRef.current?.getPosition()?.lineNumber ?? 0;

  useEffect(() => {
    rpc<EditorOpenResult>('editor.open', { path })
      .then((r) => {
        stampRef.current = r.stamp ?? null;
        setContent(r.content);
        setLanguage(r.language);
        setReady(true);
      })
      .catch(() => setReady(true));
  }, [path]);

  // replaces the buffer with the file on disk; undo can still bring the old text back
  const reloadFromDisk = async (automatic: boolean) => {
    const gen = ++checkGenRef.current;
    let r: EditorOpenResult;
    try {
      r = await rpc<EditorOpenResult>('editor.open', { path });
    } catch {
      return;
    }
    const model = editorRef.current?.getModel();
    if (!model || gen !== checkGenRef.current || savingRef.current) return;
    if (model.getValue() !== r.content) {
      model.pushEditOperations(
        [],
        [{ range: model.getFullModelRange(), text: r.content }],
        () => null
      );
    }
    stampRef.current = r.stamp ?? null;
    savedVersionRef.current = model.getAlternativeVersionId();
    dirtyRef.current = false;
    setDiskChanged(false);
    if (automatic) showNotice(tf('File %1 was reloaded from disk.', baseName(path)));
    await syncBreakpoints();
    runAnalyze();
  };
  const keepBuffer = () => {
    if (diskStampRef.current) stampRef.current = diskStampRef.current;
    setDiskChanged(false);
  };
  // compares the file on disk with the one the buffer came from (no timer: zero idle cost)
  const checkDisk = () => {
    if (!stampRef.current || savingRef.current || !editorRef.current) return;
    const gen = ++checkGenRef.current;
    rpc<EditorFileStamp>('editor.stat', { path })
      .then((current) => {
        if (gen !== checkGenRef.current || savingRef.current) return;
        const action = externalChangeAction(stampRef.current, current, dirtyRef.current);
        if (action === 'forget') {
          stampRef.current = current;
          setDiskChanged(false);
        } else if (action === 'reload') {
          void reloadFromDisk(true);
        } else if (action === 'ask') {
          diskStampRef.current = current;
          setDiskChanged(true);
        }
      })
      .catch(() => undefined);
  };
  const checkDiskRef = useRef(checkDisk);
  checkDiskRef.current = checkDisk;
  useEffect(() => {
    const check = () => checkDiskRef.current();
    const onVisible = () => {
      if (document.visibilityState === 'visible') check();
    };
    window.addEventListener('focus', check);
    document.addEventListener('visibilitychange', onVisible);
    // commands typed at the prompt may write the file too
    const offReady = onEngineReady(check);
    const offFs = onPush('fs.changed', check);
    return () => {
      window.removeEventListener('focus', check);
      document.removeEventListener('visibilitychange', onVisible);
      offReady();
      offFs();
    };
  }, [path]);

  useEffect(() => subscribeScheme(setScheme), []);

  useEffect(() => {
    const onGoto = (e: Event) => {
      const d = (e as CustomEvent<{ path?: string; line?: number; column?: number }>).detail;
      const ed = editorRef.current;
      if (!ed || !d?.line || !d.path || !samePath(path, d.path)) return;
      ed.revealLineInCenter(d.line);
      ed.setPosition({ lineNumber: d.line, column: d.column && d.column > 0 ? d.column : 1 });
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
    let lastBpRevision = -1;
    const accept = (state: DebuggerState | null | undefined) => {
      const next = state && typeof state === 'object' ? { ...emptyDebug, ...state } : emptyDebug;
      debugRef.current = next;
      latestDebugRevision = next.revision;
      // a new breakpoints revision (run to here consumed, dbstop or dbclear typed) reloads the gutter
      if (next.breakpointsRevision !== lastBpRevision) {
        lastBpRevision = next.breakpointsRevision;
        void syncBreakpoints();
      }
    };
    const pull = async () => {
      try {
        accept(await rpc<DebuggerState>('debugger.state'));
      } catch {
        accept(null);
      }
    };
    void pull();
    const off = onEngineReady(() => void pull());
    const offDebug = onPush('debugger.state', (state) => accept(state as DebuggerState));
    const offBreakpoints = onPush('debugger.breakpoints', () => {
      void syncBreakpoints();
      void refreshErrorBreakpoints();
    });
    // Paint pump: a synchronous setState inside a timer callback reliably
    // flushes dockview's portal, whereas an async setState from the push's
    // microtask chain does not. Only re-render when the position actually
    // changed, so the idle cost is a cheap identity check.
    const iv = setInterval(() => {
      if (!alive) return;
      const d = debugRef.current;
      const sig = `${d.running}:${d.paused}:${d.currentFile}:${d.currentLine}:${d.reason}:${d.revision}`;
      if (sig !== last) {
        last = sig;
        forcePaint((n) => n + 1);
      }
    }, 250);
    return () => {
      alive = false;
      off();
      offDebug();
      offBreakpoints();
      clearInterval(iv);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [path]);

  // paints the paused line: on every state change, and once Monaco has mounted
  const paintDebugLine = () => {
    const editor = editorRef.current;
    const monaco = monacoRef.current;
    const model = editor?.getModel();
    if (!editor || !monaco || !model) return;
    const d = debugRef.current;
    const line = d.paused && d.currentFile && samePath(path, d.currentFile) ? d.currentLine : null;
    const maxColumn = line && line <= model.getLineCount() ? model.getLineMaxColumn(line) : 1;
    debugDecoRef.current = editor.deltaDecorations(
      debugDecoRef.current,
      debugLineDecorations(monaco, d, path, maxColumn)
    );
    if (line) editor.revealLineInCenter(line);
  };
  useEffect(() => {
    paintDebugLine();
    // eslint-disable-next-line react-hooks/exhaustive-deps
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

  // save: breakpoints follow the edits, pending ones are created, a paused session ends
  const saveFile = async (): Promise<boolean> => {
    const editor = editorRef.current;
    const model = editor?.getModel();
    if (!editor || !model) return false;
    const moves = breakpointMoves(bpRef.current, currentLineOf);
    const pending = pendingBreakpoints(bpRef.current, currentLineOf);
    const wasPaused = debugRef.current.paused;
    const version = model.getAlternativeVersionId();
    savingRef.current += 1;
    checkGenRef.current += 1;
    try {
      const r = await rpc<EditorSaveResult>('editor.save', { path, content: editor.getValue() });
      stampRef.current = r?.stamp ?? stampRef.current;
      setDiskChanged(false);
    } catch (e) {
      // the buffer stays modified
      showWriteError(path, e);
      return false;
    } finally {
      savingRef.current -= 1;
    }
    savedVersionRef.current = version;
    dirtyRef.current = model.getAlternativeVersionId() !== version;
    if (wasPaused) {
      await debugCommand('quit');
      showNotice(t('Debugging ended: the file was saved while paused.'));
    }
    if (moves.length) {
      await rpc('debugger.moveBreakpoints', { file: path, moves }).catch(() => undefined);
    }
    for (const bp of pending) {
      await rpc<BreakpointsResult>('debugger.setBreakpoint', { file: path, ...bp })
        .then((r) => {
          if (r.error) showNotice(r.error);
        })
        .catch(() => undefined);
    }
    await syncBreakpoints();
    runAnalyze();
    return true;
  };

  const save = () => void saveFile();

  const runFile = () => {
    // Persist first so `run` sees the latest edits, then execute the file.
    const escaped = path.replace(/'/g, "''");
    void saveFile().then((ok) => (ok ? runCommand(`run('${escaped}')`) : undefined));
  };

  // Run to Here: runs to the cursor line and pauses before it
  const runToHere = () => {
    const line = cursorLine();
    if (!line) return;
    const go = () => debugCommand('runToLine', { file: path, line });
    if (dirtyRef.current) void saveFile().then((ok) => (ok ? go() : undefined));
    else void go();
  };

  const onContentChange = () => {
    scheduleAnalyze();
    const model = editorRef.current?.getModel();
    if (!model) return;
    const dirty = model.getAlternativeVersionId() !== savedVersionRef.current;
    if (dirty !== dirtyRef.current) {
      dirtyRef.current = dirty;
      // the markers turn dark gray while the file has unsaved changes
      paintBreakpoints(bpRef.current);
    }
  };

  const saveAs = async () => {
    const editor = editorRef.current;
    if (!editor) return;
    // Same path picker as the Notebook: native dialog, else the web file picker.
    const target = await requestSavePath({
      title: t('Save As'),
      suggestedName: baseName(path),
      filters: ['.m'],
      defaultPath: dirName(path) || undefined,
    });
    if (!target || target === path) return;
    rpc('editor.save', { path: target, content: editor.getValue() })
      .then(() => openEditor(target))
      .catch((e) => showWriteError(target, e));
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

    savedVersionRef.current = editor.getModel()?.getAlternativeVersionId() ?? 0;
    dirtyRef.current = false;
    void syncBreakpoints();
    void refreshErrorBreakpoints();

    runAnalyze();
    // the stop may have opened this editor: the state is already known at mount time
    paintDebugLine();
    // a panel opened at a line: the goto event fired before this editor could listen
    if (initialLine && initialLine > 0) {
      editor.revealLineInCenter(initialLine);
      editor.setPosition({
        lineNumber: initialLine,
        column: initialColumn && initialColumn > 0 ? initialColumn : 1,
      });
    }

    editor.onMouseDown((e) => {
      if (
        e.event.leftButton &&
        e.target.type === monaco.editor.MouseTargetType.GUTTER_GLYPH_MARGIN
      ) {
        const line = e.target.position?.lineNumber;
        if (line) toggleBreakpointAt(line);
      }
    });
    // right-click in the gutter opens the breakpoint menu before Monaco's own menu
    editor.getDomNode()?.addEventListener(
      'contextmenu',
      (ev) => {
        const T = monaco.editor.MouseTargetType;
        const target = editor.getTargetAtClientPoint(ev.clientX, ev.clientY);
        if (
          !target ||
          (target.type !== T.GUTTER_GLYPH_MARGIN && target.type !== T.GUTTER_LINE_NUMBERS)
        ) {
          return;
        }
        ev.preventDefault();
        ev.stopPropagation();
        const line = target.position?.lineNumber;
        if (line) {
          setConditionError('');
          setPopover({ line, x: ev.clientX, y: ev.clientY });
        }
      },
      true
    );

    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, save);
    editor.addAction({
      id: 'nelson.evaluate',
      label: t('Evaluate Selection'),
      keybindings: [monaco.KeyCode.F9],
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 1,
      run: evaluateSelection,
    });
    editor.addAction({
      id: 'nelson.section',
      label: t('Run Section'),
      keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 2,
      run: runSection,
    });
    editor.addAction({
      id: 'nelson.run',
      label: t('Run File / Continue'),
      keybindings: [monaco.KeyCode.F5],
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 3,
      // F5 continues while paused in the debugger
      run: () => {
        if (debugRef.current.paused) void debugCommand('continue');
        else runFile();
      },
    });
    editor.addAction({
      id: 'nelson.help',
      label: t('Help on Selection'),
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
      label: t('Set/Clear Breakpoint'),
      keybindings: [monaco.KeyCode.F12, monaco.KeyMod.CtrlCmd | monaco.KeyCode.F9],
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 5,
      run: () => {
        const line = cursorLine();
        if (line) toggleBreakpointAt(line);
      },
    });
    editor.addAction({
      id: 'nelson.clearBreakpoints',
      label: t('Clear All Breakpoints'),
      contextMenuGroupId: 'nelson',
      contextMenuOrder: 6,
      run: clearAllBreakpoints,
    });
    // debugger keys; F6 (Pause) and Ctrl+F10 (Run to Here) are Nelson's
    const debugKey = (id: string, label: string, keys: number[], run: () => void) =>
      editor.addAction({ id, label, keybindings: keys, run });
    debugKey('nelson.dbstep', t('Step'), [monaco.KeyCode.F10], () => void stepCommand('step'));
    debugKey(
      'nelson.dbstepin',
      t('Step In'),
      [monaco.KeyCode.F11],
      () => void stepCommand('stepIn')
    );
    debugKey(
      'nelson.dbstepout',
      t('Step Out'),
      [monaco.KeyMod.Shift | monaco.KeyCode.F11],
      () => void stepCommand('stepOut')
    );
    debugKey(
      'nelson.dbquit',
      t('Stop'),
      [monaco.KeyMod.Shift | monaco.KeyCode.F5],
      () => void stopDebugging()
    );
    debugKey('nelson.dbpause', t('Pause'), [monaco.KeyCode.F6], () => void debugCommand('pause'));
    debugKey(
      'nelson.runToHere',
      t('Run to Here'),
      [monaco.KeyMod.CtrlCmd | monaco.KeyCode.F10],
      runToHere
    );
    editor.addAction({
      id: 'nelson.gotoSymbol',
      label: t('Go to Function/Section…'),
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

  if (!ready) return <div className="browser__empty">{t('loading…')}</div>;

  const totalIssues = issues.errors + issues.warnings + issues.infos;
  const canStep = debug.paused && debug.canStep;
  const isNelsonFile = /\.m$/i.test(path);
  const popoverTarget = popover ? trackedAt(popover.line) : undefined;
  const ribbonGroups: RibbonGroupDef[] = [
    {
      id: 'file',
      label: t('FILE'),
      priority: 10, // collapses last (most important)
      actions: [
        {
          key: 'new',
          label: (
            <>
              <DesktopIcon name="file" size={16} /> {t('New')}
            </>
          ),
          title: t('New script'),
          onClick: newScript,
        },
        {
          key: 'save',
          label: (
            <>
              <DesktopIcon name="save" size={16} /> {t('&Save')}
            </>
          ),
          title: t('Save (Ctrl+S)'),
          onClick: save,
        },
        { key: 'saveas', label: t('Save As'), title: t('Save As…'), onClick: saveAs },
      ],
    },
    {
      id: 'navigate',
      label: t('NAVIGATE'),
      priority: 1,
      actions: [
        {
          key: 'find',
          label: (
            <>
              <DesktopIcon name="search" size={16} /> {t('Find')}
            </>
          ),
          title: t('Find / Replace (Ctrl+F)'),
          onClick: find,
        },
        { key: 'goto', label: t('Go To'), title: t('Go to line (Ctrl+G)'), onClick: gotoLine },
        {
          key: 'sym',
          label: t('Symbols'),
          title: t('Go to function/section (Ctrl+Shift+O)'),
          onClick: gotoSymbol,
        },
      ],
    },
    {
      id: 'code',
      label: t('CODE'),
      priority: 2,
      actions: [
        {
          key: 'comment',
          label: `% ${t('Comment')}`,
          title: t('Comment / Uncomment (Ctrl+/)'),
          onClick: commentToggle,
        },
        { key: 'format', label: t('Format'), title: t('Format document'), onClick: format },
        {
          key: 'wrap',
          label: t('Wrap'),
          title: t('Toggle word wrap'),
          onClick: toggleWrap,
          className: wrap ? 'ribbon-btn--on' : '',
        },
      ],
    },
    {
      id: 'analyze',
      label: t('ANALYZE'),
      priority: 3,
      actions: [
        {
          key: 'issues',
          className: 'editor-toolbar__issues' + (showProblems ? ' ribbon-btn--on' : ''),
          title: t('Show code issues'),
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
                )}{' '}
                {t('Issues')}
              </>
            ) : (
              <>
                <DesktopIcon name="check" size={16} /> {t('No issues')}
              </>
            ),
          disabled: totalIssues === 0,
        },
        {
          key: 'prof',
          label: (
            <>
              <DesktopIcon name="profiler" size={16} /> {t('Profiler')}
            </>
          ),
          title: t('Profiler'),
          onClick: () => ensureCorePanel('profiler'),
        },
      ],
    },
    {
      id: 'section',
      label: t('SECTION'),
      priority: 4,
      actions: [
        {
          key: 'break',
          label: `%% ${t('Break')}`,
          title: t('Insert section break (%%)'),
          onClick: insertSection,
        },
        {
          key: 'runsec',
          label: (
            <>
              <DesktopIcon name="execute" size={16} /> {t('Section')}
            </>
          ),
          title: t('Run section (Ctrl+Enter)'),
          onClick: runSection,
        },
        {
          key: 'eval',
          label: (
            <>
              <DesktopIcon name="execute" size={16} /> {t('Evaluate')}
            </>
          ),
          title: t('Evaluate selection (F9)'),
          onClick: evaluateSelection,
        },
      ],
    },
    {
      id: 'debug',
      label: t('DEBUG'),
      // folds first: while paused the debug bar holds the stepping buttons
      priority: 0,
      actions: [
        {
          key: 'step',
          label: t('Step'),
          title: `${t('Step')} (F10)`,
          onClick: () => void stepCommand('step'),
          disabled: !canStep,
        },
        {
          key: 'stepin',
          label: t('Step In'),
          title: `${t('Step In')} (F11)`,
          onClick: () => void stepCommand('stepIn'),
          disabled: !canStep,
        },
        {
          key: 'stepout',
          label: t('Step Out'),
          title: `${t('Step Out')} (Shift+F11)`,
          onClick: () => void stepCommand('stepOut'),
          disabled: !canStep,
        },
        {
          key: 'runtohere',
          label: t('Run to Here'),
          title: `${t('Run to Here')} (Ctrl+F10)`,
          onClick: runToHere,
          disabled: !isNelsonFile,
        },
        {
          key: 'pause',
          label: t('Pause'),
          title: `${t('Pause')} (F6)`,
          onClick: () => void debugCommand('pause'),
          disabled: !engineBusy || debug.paused,
        },
        {
          key: 'stop',
          label: (
            <>
              <DesktopIcon name="stop" size={16} /> {t('Stop')}
            </>
          ),
          title: `${t('Stop')} (Shift+F5)`,
          onClick: () => void stopDebugging(),
          disabled: !debug.running,
        },
        {
          key: 'bp',
          label: t('Set/Clear Breakpoint'),
          title: `${t('Set/Clear Breakpoint')} (F12)`,
          onClick: () => {
            const line = cursorLine();
            if (line) toggleBreakpointAt(line);
          },
          disabled: !isNelsonFile,
        },
        {
          key: 'clearbp',
          label: t('Clear All Breakpoints'),
          title: t('Clear All Breakpoints'),
          onClick: clearAllBreakpoints,
        },
        {
          key: 'pauseerr',
          label: t('Pause on Errors'),
          title: t('Pause on Errors'),
          onClick: () =>
            setErrorBreakpointFlags({ ...errorBreakpoints, error: !errorBreakpoints.error }),
          className: errorBreakpoints.error ? 'ribbon-btn--on' : '',
        },
        {
          key: 'pausecaught',
          label: t('Pause on Caught Errors'),
          title: t('Pause on Caught Errors'),
          onClick: () =>
            setErrorBreakpointFlags({
              ...errorBreakpoints,
              caughtError: !errorBreakpoints.caughtError,
            }),
          className: errorBreakpoints.caughtError ? 'ribbon-btn--on' : '',
        },
        {
          key: 'bppanel',
          label: t('Breakpoints'),
          title: t('Breakpoints'),
          onClick: () => ensureCorePanel('breakpoints'),
        },
        {
          key: 'stackpanel',
          label: t('Call Stack'),
          title: t('Call Stack'),
          onClick: () => ensureCorePanel('callstack'),
        },
      ],
    },
    {
      id: 'run',
      label: t('RUN'),
      priority: 9, // collapses near-last
      actions: [
        {
          key: 'run',
          label: (
            <>
              <DesktopIcon name="execute" size={16} /> {debug.paused ? t('Continue') : t('Run')}
            </>
          ),
          title: debug.paused ? `${t('Continue')} (F5)` : t('Run file (F5)'),
          onClick: () => {
            if (debug.paused) void debugCommand('continue');
            else runFile();
          },
          className: 'ribbon-btn--run',
        },
      ],
    },
  ];

  return (
    <div className="editor-panel">
      <EditorRibbon groups={ribbonGroups} />
      {notice && (
        <div className="editor-notice" role="status">
          {notice}
        </div>
      )}
      {diskChanged && (
        <div className="editor-notice editor-notice--disk" role="alert">
          <span className="editor-notice__text">
            {tf(
              'File %1 was modified by an external software.\nDo you want to reopen it?',
              baseName(path)
            ).replace(/\n/g, ' ')}
          </span>
          <button type="button" onClick={() => void reloadFromDisk(false)}>
            {t('Reload')}
          </button>
          <button type="button" onClick={keepBuffer}>
            {t('Keep')}
          </button>
        </div>
      )}
      {debug.running && (
        <div
          className={
            'editor-debugbar' + (debug.reason === 'error' ? ' editor-debugbar--error' : '')
          }
          title={debug.reason === 'error' ? debug.errorMessage : undefined}
        >
          <span className="editor-debugbar__label">
            {debug.reason === 'error'
              ? `${t('Paused on error')}: ${debug.errorMessage}`
              : debug.paused
              ? `${t('Paused')} · ${
                  debug.function || debug.stack[0]?.function || debug.currentFile || ''
                }:${debug.currentLine ?? ''}`
              : t('Running')}
          </span>
          <span className="editor-toolbar__spacer" />
          <button
            className="editor-toolbar__btn"
            title={`${t('Step')} (F10)`}
            disabled={!canStep}
            onClick={() => void stepCommand('step')}
          >
            {t('Step')}
          </button>
          <button
            className="editor-toolbar__btn"
            title={`${t('Step In')} (F11)`}
            disabled={!canStep}
            onClick={() => void stepCommand('stepIn')}
          >
            {t('Step In')}
          </button>
          <button
            className="editor-toolbar__btn"
            title={`${t('Step Out')} (Shift+F11)`}
            disabled={!canStep}
            onClick={() => void stepCommand('stepOut')}
          >
            {t('Step Out')}
          </button>
          <button
            className="editor-toolbar__btn"
            title={`${t('Continue')} (F5)`}
            disabled={!debug.paused}
            onClick={() => void debugCommand('continue')}
          >
            <DesktopIcon name="execute" size={16} /> {t('Continue')}
          </button>
          <button
            className="editor-toolbar__btn"
            title={`${t('Stop')} (Shift+F5)`}
            onClick={() => void stopDebugging()}
          >
            <DesktopIcon name="stop" size={16} /> {t('Stop')}
          </button>
        </div>
      )}
      <div className="editor-panel__body">
        <Editor
          theme={monacoTheme(scheme)}
          language={language}
          value={content}
          onMount={onMount}
          onChange={onContentChange}
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
      {popover && (
        <BreakpointPopover
          line={popover.line}
          x={popover.x}
          y={popover.y}
          hasBreakpoint={!!popoverTarget}
          enabled={popoverTarget?.enabled ?? true}
          condition={popoverTarget?.condition ?? ''}
          hasAny={bpRef.current.length > 0}
          conditionError={conditionError}
          onToggle={() => toggleBreakpointAt(popover.line)}
          onSetCondition={(condition) => setConditionAt(popover.line, condition)}
          onSetEnabled={(enabled) => setEnabledAt(popover.line, enabled)}
          onEnableAllInFile={() => setAllEnabledInFile(true)}
          onDisableAllInFile={() => setAllEnabledInFile(false)}
          onClearAllInFile={clearAllInFile}
          onClearAll={clearAllBreakpoints}
          onClose={() => {
            setPopover(null);
            setConditionError('');
          }}
        />
      )}
      {showProblems && diagnostics.length > 0 && (
        <div className="editor-problems">
          <div className="editor-problems__head">
            <span>
              {diagnostics.length > 1
                ? tf('%1 issues', diagnostics.length)
                : tf('%1 issue', diagnostics.length)}
            </span>
            <button
              className="editor-problems__close"
              title={t('Hide')}
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
                title={t('Go to line')}
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
                    title={d.fixes[0].title || t('Apply fix')}
                    onClick={(e) => {
                      e.stopPropagation();
                      applyFix(d);
                    }}
                  >
                    {t('Fix')}
                  </button>
                ) : null}
                <span className="editor-problems__loc">{tf('Ln %1', d.lineStart || 1)}</span>
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
//=============================================================================
