//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { BlockNoteView } from '@blocknote/mantine';
import {
  useCreateBlockNote,
  SuggestionMenuController,
  getDefaultReactSlashMenuItems,
} from '@blocknote/react';
import { filterSuggestionItems } from '@blocknote/core';
import '@blocknote/core/fonts/inter.css';
import '@blocknote/mantine/style.css';
import 'katex/dist/katex.min.css';
import './notebook.css';
import { schema, defaultBlockFor, CONTROL_TYPES } from './schema';
import { initialContent } from './initialContent';
import { NotebookContext, type NotebookCtx } from './context';
import { runCell, applyWorkspace, resetWorkspace } from './kernel';
import { emitLocalPushValue, rpc } from '../rpc/client';
import { controlAssignment } from './runMode';
import { reactiveTargets, computeStale } from './reactive';
import { markFiguresLive, closeLiveFigures, parseFigureHandles } from './liveFigures';
import { errorCellIds } from './cellOutputs';
import { DesktopIcon } from '../components/DesktopIcon';
//=============================================================================
const titleFromName = (name: string) => name.replace(/\.(nlx|m|ipynb)$/i, '');
import {
  blocksToIpynb,
  blocksToHtml,
  blocksToMarkdown,
  blocksToM,
  mToBlocks,
  mScriptToBlocks,
  isMarkedLiveM,
  blocksToDocx,
  ipynbToBlocks,
  inlineText,
} from './exporters';
import { serializeNlx, parseNlx, base64ToBytes } from './nlxFormat';
import { toggleNotebookMenu, type NotebookMenu } from './menuState';
import { onNotebookCommand } from './commands';
import { useMenuDismiss } from '../components/useMenuDismiss';
import { requestSavePath } from '../components/DesktopFilePickerHost';
import { isWasmRuntime } from '../desktop/platform';
import { hasFormat, saveNotebookDocument, type NotebookSaveFormat } from './saveDocument';
import { baseName } from '../desktop/paths';
import { t, tf } from '../i18n';
//=============================================================================
function useDarkScheme(): boolean {
  const read = () =>
    !document.querySelector('.nelson-shell')?.classList.contains('nelson-shell--light');
  const [dark, setDark] = useState(read);
  useEffect(() => {
    const el = document.querySelector('.nelson-shell');
    if (!el) return;
    const obs = new MutationObserver(() => setDark(read()));
    obs.observe(el, { attributes: true, attributeFilter: ['class'] });
    return () => obs.disconnect();
  }, []);
  return dark;
}
//=============================================================================
function sectionRange(blocks: any[], id: string): any[] {
  const idx = blocks.findIndex((b) => b.id === id);
  if (idx < 0) return [];
  let start = 0;
  for (let i = idx; i >= 0; i--)
    if (blocks[i].type === 'heading') {
      start = i;
      break;
    }
  let end = blocks.length;
  for (let i = idx + 1; i < blocks.length; i++)
    if (blocks[i].type === 'heading') {
      end = i;
      break;
    }
  return blocks.slice(start, end);
}
//=============================================================================
function download(filename: string, data: BlobPart, mime: string): void {
  const blob = new Blob([data], { type: mime });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = filename;
  a.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}
//=============================================================================
// Open the rendered document in a window and print it; where the webview blocks
// window.open, download the HTML instead.
function printHtml(html: string): void {
  const w = window.open('', '_blank');
  if (!w) {
    download('notebook.html', html, 'text/html');
    return;
  }
  w.document.write(html);
  w.document.close();
  w.focus();
  setTimeout(() => w.print(), 300);
}
// Notify inline figures only after a complete run has released the interpreter.
// Fitting individual cells while Run all is still iterating races the next cell.
function notifyRunCompleted() {
  emitLocalPushValue('workspace.changed', {});
  emitLocalPushValue('notebook.run.completed', {});
}
//=============================================================================
interface PanelApiLike {
  setTitle?: (title: string) => void;
  // dockview tells which panel is active; the menu bar addresses that one
  isActive?: boolean;
}
//=============================================================================
// Cells and controls offered by the slash menu and the Insert menu.
function insertableBlocks(): { type: string; title: string; subtext: string; aliases: string[] }[] {
  const mk = (type: string, title: string, subtext: string, aliases: string[]) => ({
    type,
    title,
    subtext,
    aliases,
  });
  return [
    mk('codecell', t('Code cell'), t('Executable Nelson code'), ['code', 'run']),
    mk('equation', t('Equation'), t('LaTeX display equation'), ['equation', 'math']),
    mk('livetask', t('Live task'), t('Interactive code-generating task'), ['task']),
    mk('varselect', t('Variable selector'), t('Pick a workspace variable'), ['variable']),
    mk('slider', t('Slider'), t('Numeric slider control'), ['slider']),
    mk('rangeslider', t('Range slider'), t('Two-thumb interval'), ['range']),
    mk('numberfield', t('Number field'), t('Numeric spinner'), ['number']),
    mk('dropdown', t('Drop-down'), t('Choice control'), ['dropdown', 'select']),
    mk('checkbox', t('Check box'), t('Boolean control'), ['checkbox']),
    mk('textfield', t('Text field'), t('Text/number field'), ['text', 'edit']),
    mk('colorpicker', t('Color picker'), t('Color control'), ['color']),
    mk('datepicker', t('Date picker'), t('Date control'), ['date']),
    mk('filebrowser', t('File browser'), t('Pick a file'), ['file']),
    mk('runbutton', t('Button'), t('Run the document'), ['button']),
    mk('statebutton', t('State button'), t('Toggle on/off'), ['toggle', 'state']),
  ];
}
//=============================================================================
function ToolbarCaret(): JSX.Element {
  return (
    <svg className="nls-caret" viewBox="0 0 10 6" width="8" height="5" aria-hidden="true">
      <path d="M1 1l4 4 4-4" fill="none" stroke="currentColor" strokeWidth="1.5" />
    </svg>
  );
}
//=============================================================================
export function NotebookPanel({
  panelApi,
  path,
}: { panelApi?: PanelApiLike; path?: string } = {}): JSX.Element {
  const dark = useDarkScheme();
  const [hideCode, setHideCode] = useState(false);
  const [outputLayout, setOutputLayout] = useState<'inline' | 'right'>('inline');
  const [showOutline, setShowOutline] = useState(false);
  const [running, setRunning] = useState(false);
  const [runningBlockId, setRunningBlockId] = useState<string | null>(null);
  const [docVersion, setDocVersion] = useState(0);
  const [openMenu, setOpenMenu] = useState<NotebookMenu | null>(null);
  // The host path the document came from or was last saved to; Save writes there.
  const [docPath, setDocPath] = useState<string | undefined>(path);
  const [saveNotice, setSaveNotice] = useState('');
  const editor = useCreateBlockNote({ schema });
  // The code each cell had when it last ran, the basis for staleness.
  const lastRunCode = useRef<Record<string, string>>({});
  // Staleness is only meaningful once the first full run has established a
  // baseline; before that every cell would look "never run".
  const staleReady = useRef(false);
  // Each Notebook document starts its own capture session. The shared Wasm
  // worker keeps figures from other open documents alive, while this flag
  // ensures the first cell establishes a fresh capture baseline.
  const liveFiguresReset = useRef(false);

  const runSessionCell = useCallback(async (code: string, reuseHandles: number[] = []) => {
    const result = await runCell(code, reuseHandles, {
      resetLiveFigures: !liveFiguresReset.current,
    });
    if (result.ok) liveFiguresReset.current = true;
    return result;
  }, []);

  const writeResult = useCallback(
    async (b: any) => {
      const code = b.props.code ?? '';
      setRunningBlockId(b.id);
      // Pass the cell's previous interactive figures to the engine as reuse handles:
      // it closes them just before its baseline snapshot, so the cell's figure() calls
      // re-grab the same numbers. The inline panel keeps the old frame during the run,
      // so a re-run (a slider move) updates in place with the same figure number and
      // no blank flash.
      const previousHandles = parseFigureHandles(b.props.figureHandles);
      const reuseHandles = previousHandles.filter((h) => h >= 0);
      const res = await runSessionCell(code, reuseHandles);
      lastRunCode.current[b.id] = code;
      const runRevision = Number.isFinite(Number(b.props.runRevision))
        ? Number(b.props.runRevision) + 1
        : 1;
      // An interactive figure keeps a live handle; a non-interactive one stores
      // -1 so the cell renders its PNG fallback for that slot.
      const handles = res.figures.map((f) => (f.interactive ? f.handle : -1));
      markFiguresLive(handles);
      editor.updateBlock(b, {
        props: {
          output: res.errorMessage ? '' : res.output,
          figures: JSON.stringify(res.figures.map((f) => f.image)),
          figureHandles: JSON.stringify(handles),
          runRevision,
          errorMsg: res.errorMessage,
          stale: false,
        },
      } as any);
      // The engine already closed the reuse handles it did not re-grab; forget
      // any previous handle the new run no longer owns so it stops rendering live.
      const dropped = previousHandles.filter((h) => h >= 0 && !handles.includes(h));
      if (dropped.length) void closeLiveFigures(dropped);
    },
    [editor, runSessionCell]
  );

  // Mark the cells that are out of date. Writing only when the flag flips keeps this
  // idempotent, so onChange cannot loop.
  const refreshStale = useCallback(() => {
    if (!staleReady.current) return;
    const blocks = editor.document as any[];
    const stale = computeStale(blocks, lastRunCode.current);
    for (const b of blocks)
      if (b.type === 'codecell' && !!b.props.stale !== stale.has(b.id))
        editor.updateBlock(b, { props: { stale: stale.has(b.id) } } as any);
  }, [editor]);

  const controlAssignments = useCallback(
    (blocks: any[]) =>
      blocks
        .filter((b) => CONTROL_TYPES.includes(b.type))
        .map((b) => controlAssignment(b))
        .filter(Boolean),
    []
  );

  const runAll = useCallback(async () => {
    setRunning(true);
    try {
      const reset = await resetWorkspace({ resetLiveFigures: !liveFiguresReset.current });
      if (reset.ok) liveFiguresReset.current = true;
      const blocks = editor.document as any[];
      await applyWorkspace(controlAssignments(blocks));
      for (const b of blocks) if (b.type === 'codecell') await writeResult(b);
    } finally {
      setRunning(false);
      setRunningBlockId(null);
      notifyRunCompleted();
    }
  }, [editor, writeResult, controlAssignments]);

  const runToHere = useCallback(
    async (id: string) => {
      setRunning(true);
      try {
        const reset = await resetWorkspace({ resetLiveFigures: !liveFiguresReset.current });
        if (reset.ok) liveFiguresReset.current = true;
        const blocks = editor.document as any[];
        await applyWorkspace(controlAssignments(blocks));
        for (const b of blocks) {
          if (b.type === 'codecell') await writeResult(b);
          if (b.id === id) break;
        }
      } finally {
        setRunning(false);
        setRunningBlockId(null);
        notifyRunCompleted();
      }
    },
    [editor, writeResult, controlAssignments]
  );

  const runSection = useCallback(
    async (id: string) => {
      const sec = sectionRange(editor.document as any[], id);
      await applyWorkspace(controlAssignments(sec));
      for (const b of sec) if (b.type === 'codecell') await writeResult(b);
      setRunningBlockId(null);
      notifyRunCompleted();
    },
    [editor, writeResult, controlAssignments]
  );

  const runBlock = useCallback(
    async (id: string) => {
      const block = editor.getBlock(id) as any;
      if (block?.type === 'codecell') await writeResult(block);
      setRunningBlockId(null);
      notifyRunCompleted();
    },
    [editor, writeResult]
  );

  // Apply controls, then re-run only the cells downstream of the variable this
  // control drives.
  const runReactive = useCallback(
    async (controlBlockId: string) => {
      setRunning(true);
      try {
        const blocks = editor.document as any[];
        await applyWorkspace(controlAssignments(blocks));
        const ctrl = blocks.find((b) => b.id === controlBlockId) as any;
        const changedVar = String(ctrl?.props?.varName ?? '');
        const targets = reactiveTargets(blocks, changedVar);
        for (const b of blocks)
          if (b.type === 'codecell' && targets.has(b.id)) await writeResult(b);
      } finally {
        setRunning(false);
        setRunningBlockId(null);
        notifyRunCompleted();
      }
    },
    [editor, writeResult, controlAssignments]
  );

  const runStale = useCallback(async () => {
    setRunning(true);
    try {
      const blocks = editor.document as any[];
      await applyWorkspace(controlAssignments(blocks));
      const stale = computeStale(blocks, lastRunCode.current);
      for (const b of blocks) if (b.type === 'codecell' && stale.has(b.id)) await writeResult(b);
    } finally {
      setRunning(false);
      setRunningBlockId(null);
      notifyRunCompleted();
      refreshStale();
    }
  }, [editor, writeResult, controlAssignments, refreshStale]);

  // Serialize runs. Dragging a control fires many change events; running them
  // concurrently lets one run close (reuse) a figure another run is still
  // drawing into, which crashes the engine (EXCEPTION_ACCESS_VIOLATION in
  // @graphics_object/set). Run one at a time and coalesce a burst to the latest
  // request, so the final slider value is what gets drawn.
  const runLock = useRef<Promise<void> | null>(null);
  const pendingRun = useRef<(() => Promise<void>) | null>(null);
  const serialize = useCallback((task: () => Promise<void>): Promise<void> => {
    if (runLock.current) {
      pendingRun.current = task;
      return runLock.current;
    }
    const drain = async (first: () => Promise<void>) => {
      let current: (() => Promise<void>) | null = first;
      while (current) {
        try {
          await current();
        } finally {
          current = pendingRun.current;
          pendingRun.current = null;
        }
      }
      runLock.current = null;
    };
    const p = drain(task);
    runLock.current = p;
    return p;
  }, []);

  const runAllSafe = useCallback(() => serialize(runAll), [serialize, runAll]);
  const runStaleSafe = useCallback(() => serialize(runStale), [serialize, runStale]);
  const runToHereSafe = useCallback(
    (id: string) => serialize(() => runToHere(id)),
    [serialize, runToHere]
  );
  const runSectionSafe = useCallback(
    (id: string) => serialize(() => runSection(id)),
    [serialize, runSection]
  );
  const runBlockSafe = useCallback(
    (id: string) => serialize(() => runBlock(id)),
    [serialize, runBlock]
  );
  const runReactiveSafe = useCallback(
    (id: string) => serialize(() => runReactive(id)),
    [serialize, runReactive]
  );

  const allFigureHandles = useCallback(
    () =>
      (editor.document as any[])
        .filter((b) => b.type === 'codecell')
        .flatMap((b) => parseFigureHandles(b.props.figureHandles)),
    [editor]
  );

  const clearOutputs = useCallback(() => {
    void closeLiveFigures(allFigureHandles());
    for (const b of editor.document as any[])
      if (b.type === 'codecell')
        editor.updateBlock(b, {
          props: { output: '', figures: '[]', figureHandles: '[]', errorMsg: '' },
        } as any);
  }, [editor, allFigureHandles]);

  // Release every interactive figure the notebook kept alive when the panel is
  // torn down, so handles do not leak in the engine.
  useEffect(() => {
    return () => {
      void closeLiveFigures(allFigureHandles());
    };
  }, [allFigureHandles]);

  const fileInputRef = useRef<HTMLInputElement>(null);
  // A short notice next to the Save button: where the document went, or why not.
  const noticeTimer = useRef<number | undefined>(undefined);
  const notify = useCallback((text: string) => {
    setSaveNotice(text);
    if (noticeTimer.current !== undefined) window.clearTimeout(noticeTimer.current);
    noticeTimer.current = window.setTimeout(() => setSaveNotice(''), 4000);
  }, []);
  useEffect(() => () => window.clearTimeout(noticeTimer.current), []);
  // Save writes on the host through a path picker; the WebAssembly build downloads the file.
  const saveDocument = useCallback(
    async (format: NotebookSaveFormat, saveAs: boolean) => {
      const blocks = editor.document as any[];
      const result = await saveNotebookDocument(
        {
          payload: format === 'nlx' ? serializeNlx(blocks) : blocksToM(blocks),
          format,
          path: docPath,
          saveAs,
          baseName: docPath ? titleFromName(baseName(docPath)) : 'notebook',
        },
        { rpc, pickPath: requestSavePath, download, browserOnly: isWasmRuntime }
      );
      if (result.kind === 'saved') {
        setDocPath(result.path);
        panelApi?.setTitle?.(titleFromName(baseName(result.path)));
        notify(tf('Saved %1', baseName(result.path)));
      } else if (result.kind === 'error') {
        notify(tf('Save failed: %1', result.message));
      }
    },
    [editor, docPath, panelApi, notify]
  );
  const save = useCallback(
    () => void saveDocument(hasFormat(docPath, 'm') ? 'm' : 'nlx', false),
    [saveDocument, docPath]
  );
  const saveNlx = useCallback(() => void saveDocument('nlx', true), [saveDocument]);
  const saveM = useCallback(() => void saveDocument('m', true), [saveDocument]);
  // Decode a document's bytes into blocks by its extension: .m round-trips via
  // its markers (a plain script is converted), .ipynb imports, .nlx unpacks.
  const decodeDocument = useCallback((name: string, bytes: Uint8Array): any[] | null => {
    if (/\.m$/i.test(name)) {
      const text = new TextDecoder().decode(bytes);
      return isMarkedLiveM(text) ? mToBlocks(text) : mScriptToBlocks(text);
    }
    if (/\.ipynb$/i.test(name)) {
      return ipynbToBlocks(new TextDecoder().decode(bytes));
    }
    return parseNlx(bytes);
  }, []);

  const loadDocument = useCallback(
    (blocks: any[] | null, title: string) => {
      if (!blocks || !blocks.length) return;
      void closeLiveFigures(allFigureHandles());
      editor.replaceBlocks(editor.document, blocks as any);
      setDocVersion((v) => v + 1);
      panelApi?.setTitle?.(title);
    },
    [editor, panelApi, allFigureHandles]
  );

  const openFile = useCallback(
    async (file: File) => {
      const bytes = new Uint8Array(await file.arrayBuffer());
      loadDocument(decodeDocument(file.name, bytes), titleFromName(file.name));
    },
    [loadDocument, decodeDocument]
  );

  // The menu bar of the Notebook window addresses the active document.
  useEffect(
    () =>
      onNotebookCommand(
        () => panelApi?.isActive ?? true,
        (command) => {
          if (command === 'open') fileInputRef.current?.click();
          else if (command === 'save') save();
          else if (command === 'saveAs') saveNlx();
          else if (command === 'saveM') saveM();
          else if (command === 'runAll') void runAllSafe();
        }
      ),
    [panelApi, save, saveNlx, saveM, runAllSafe]
  );

  // Expose the codecs for e2e and native round-trip checks.
  useEffect(() => {
    (window as any).__notebookNlx = {
      serializeNlx,
      parseNlx,
      blocksToM,
      mToBlocks,
      mScriptToBlocks,
      isMarkedLiveM,
      blocksToDocx,
      ipynbToBlocks,
    };
    return () => {
      delete (window as any).__notebookNlx;
    };
  }, []);

  useEffect(() => {
    if (path) {
      // This tab owns one .nlx: read it and show its saved output (output.json)
      // without auto-running. Run recomputes it.
      void rpc<{ data?: string; name?: string }>('fs.download', { path })
        .then((r) => {
          if (r.data) {
            const name = r.name || baseName(path) || 'document.nlx';
            loadDocument(decodeDocument(name, base64ToBytes(r.data)), name);
          }
          staleReady.current = true;
        })
        .catch(() => {
          staleReady.current = true;
        });
    } else {
      // A fresh, untitled Notebook: start from the sample and run it.
      editor.replaceBlocks(editor.document, initialContent() as any);
      setDocVersion((v) => v + 1); // populate the outline (replaceBlocks is programmatic)
      void runAllSafe().then(() => {
        staleReady.current = true;
      });
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const getSlashItems = useCallback(
    async (query: string) => {
      const custom = insertableBlocks().map(({ type, title, subtext, aliases }) => ({
        title,
        subtext,
        aliases,
        group: 'Nelson',
        onItemClick: () => {
          const cur = editor.getTextCursorPosition().block;
          editor.replaceBlocks([cur], [defaultBlockFor(type) as any]);
        },
      }));
      return filterSuggestionItems([...getDefaultReactSlashMenuItems(editor), ...custom], query);
    },
    [editor]
  );

  const insert = useCallback(
    (type: string) => {
      const ref = (editor.getTextCursorPosition().block ??
        editor.document[editor.document.length - 1]) as any;
      editor.insertBlocks([defaultBlockFor(type)], ref, 'after');
    },
    [editor]
  );

  const ctx = useMemo<NotebookCtx>(
    () => ({
      runAll: runAllSafe,
      runBlock: runBlockSafe,
      runToHere: runToHereSafe,
      runSection: runSectionSafe,
      runReactive: runReactiveSafe,
      running,
      runningBlockId,
      hideCode,
      outputLayout,
      dark,
    }),
    [
      runAllSafe,
      runBlockSafe,
      runToHereSafe,
      runSectionSafe,
      runReactiveSafe,
      running,
      runningBlockId,
      hideCode,
      outputLayout,
      dark,
    ]
  );

  const closeMenu = useCallback(() => setOpenMenu(null), []);
  // A toolbar menu closes like a native one: pointer down outside or Escape.
  useMenuDismiss(openMenu !== null, closeMenu, '.nls-menu[open]');
  const toggleMenu = (menu: NotebookMenu) => (event: React.MouseEvent<HTMLElement>) => {
    event.preventDefault();
    setOpenMenu((current) => toggleNotebookMenu(current, menu));
  };

  // Cycle through the cells whose last run reported an error. Recomputed each
  // render; docVersion drives the re-render.
  void docVersion;
  const errorCells = errorCellIds(editor.document as any[]);
  const errorCycle = useRef(0);
  const jumpToNextError = () => {
    if (errorCells.length === 0) return;
    const id = errorCells[errorCycle.current % errorCells.length];
    errorCycle.current += 1;
    document
      .querySelector(`[data-id="${id}"]`)
      ?.scrollIntoView({ behavior: 'smooth', block: 'center' });
  };

  return (
    <NotebookContext.Provider value={ctx}>
      <div className={`nls-panel ${dark ? '' : 'nls-light'}`}>
        <header className="nls-toolbar" role="toolbar" aria-label={t('Notebook toolbar')}>
          <div className="nls-toolbar__group">
            <button
              type="button"
              className="nls-btn nls-btn-primary"
              title={t('Run every code cell in order')}
              onClick={() => void runAllSafe()}
              disabled={running}
            >
              <DesktopIcon name="execute" />
              <span className="nls-btn__label">{t('Run all')}</span>
            </button>
            <button
              type="button"
              className="nls-btn"
              title={t('Run only the cells that are out of date and their dependents')}
              onClick={() => void runStaleSafe()}
              disabled={running}
            >
              <DesktopIcon name="refresh" />
              <span className="nls-btn__label">{t('Run stale')}</span>
            </button>
            <button
              type="button"
              className="nls-btn"
              title={t('Clear every cell output')}
              onClick={clearOutputs}
            >
              <DesktopIcon name="close" />
              <span className="nls-btn__label">{t('Clear')}</span>
            </button>
            {runningBlockId && (
              <button
                type="button"
                className="nls-btn nls-running-jump"
                title={t('Scroll to the cell being evaluated')}
                onClick={() =>
                  document
                    .querySelector(`[data-id="${runningBlockId}"]`)
                    ?.scrollIntoView({ behavior: 'smooth', block: 'center' })
                }
              >
                <span className="nls-running-dot" /> {t('Running…')}
              </button>
            )}
            {errorCells.length > 0 && !runningBlockId && (
              <button
                type="button"
                className="nls-btn nls-error-jump"
                title={t('Go to the next cell with an error')}
                onClick={jumpToNextError}
              >
                <DesktopIcon name="warning" />
                {errorCells.length > 1 ? tf('%1 errors', errorCells.length) : t('1 error')}
              </button>
            )}
          </div>
          <span className="nls-sep" aria-hidden="true" />
          <div className="nls-toolbar__group">
            <button
              type="button"
              className="nls-btn"
              title={t(
                'Open a Notebook (.nlx), a plain script (.m) or a Jupyter notebook (.ipynb)'
              )}
              onClick={() => fileInputRef.current?.click()}
            >
              <DesktopIcon name="folder" />
              <span className="nls-btn__label">{t('&Open')}</span>
            </button>
            <details className="nls-menu" open={openMenu === 'save'}>
              <summary
                className="nls-btn"
                title={t('Save the document')}
                onClick={toggleMenu('save')}
              >
                <DesktopIcon name="save" />
                <span className="nls-btn__label">{t('&Save')}</span>
                <ToolbarCaret />
              </summary>
              <div className="nls-menu-pop">
                <button
                  onClick={() => {
                    closeMenu();
                    save();
                  }}
                >
                  {t('&Save')}
                </button>
                <button
                  onClick={() => {
                    closeMenu();
                    saveNlx();
                  }}
                >
                  {t('Save As… (.nlx)')}
                </button>
                <button
                  onClick={() => {
                    closeMenu();
                    saveM();
                  }}
                >
                  {t('Save as Script… (.m)')}
                </button>
              </div>
            </details>
            {saveNotice && (
              <span className="nls-save-notice" role="status">
                {saveNotice}
              </span>
            )}
            <input
              ref={fileInputRef}
              type="file"
              accept=".nlx,.m,.ipynb,.json,application/json"
              hidden
              onChange={(e) => {
                const f = e.target.files?.[0];
                if (f) void openFile(f);
                e.target.value = '';
              }}
            />
          </div>
          <span className="nls-sep" aria-hidden="true" />
          <div className="nls-toolbar__group">
            <details className="nls-menu" open={openMenu === 'insert'}>
              <summary
                className="nls-btn"
                title={t('Insert a cell or a control (or type “/” in the document)')}
                onClick={toggleMenu('insert')}
              >
                <DesktopIcon name="add" />
                <span className="nls-btn__label">{t('Insert')}</span>
                <ToolbarCaret />
              </summary>
              <div className="nls-menu-pop">
                <button
                  onClick={() => {
                    closeMenu();
                    insert('codecell');
                  }}
                >
                  {t('Code cell')}
                </button>
                <button
                  onClick={() => {
                    closeMenu();
                    insert('equation');
                  }}
                >
                  {t('Equation')}
                </button>
                <button
                  onClick={() => {
                    closeMenu();
                    insert('livetask');
                  }}
                >
                  {t('Live task')}
                </button>
                <div className="nls-menu-label">{t('Controls')}</div>
                {insertableBlocks()
                  .filter((b) => CONTROL_TYPES.includes(b.type))
                  .map((b) => (
                    <button
                      key={b.type}
                      onClick={() => {
                        closeMenu();
                        insert(b.type);
                      }}
                    >
                      {b.title}
                    </button>
                  ))}
              </div>
            </details>
            <details className="nls-menu" open={openMenu === 'export'}>
              <summary
                className="nls-btn"
                title={t('Export the document')}
                onClick={toggleMenu('export')}
              >
                <DesktopIcon name="upload" />
                <span className="nls-btn__label">{t('Export')}</span>
                <ToolbarCaret />
              </summary>
              <div className="nls-menu-pop">
                <button
                  onClick={() => {
                    closeMenu();
                    download('notebook.html', blocksToHtml(editor.document as any[]), 'text/html');
                  }}
                >
                  {t('HTML…')}
                </button>
                <button
                  onClick={() => {
                    closeMenu();
                    download(
                      'notebook.md',
                      blocksToMarkdown(editor.document as any[]),
                      'text/markdown'
                    );
                  }}
                >
                  {t('Markdown…')}
                </button>
                <button
                  onClick={() => {
                    closeMenu();
                    download(
                      'notebook.docx',
                      blocksToDocx(editor.document as any[]) as unknown as BlobPart,
                      'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
                    );
                  }}
                >
                  {t('Word (.docx)…')}
                </button>
                <button
                  onClick={() => {
                    closeMenu();
                    printHtml(blocksToHtml(editor.document as any[]));
                  }}
                >
                  {t('PDF / Print…')}
                </button>
                <button
                  onClick={() => {
                    closeMenu();
                    download(
                      'notebook.ipynb',
                      blocksToIpynb(editor.document as any[]),
                      'application/json'
                    );
                  }}
                >
                  {t('Jupyter (.ipynb)…')}
                </button>
              </div>
            </details>
          </div>
          <span className="nls-spacer" />
          <div className="nls-toolbar__group">
            <button
              type="button"
              className={`nls-btn ${outputLayout === 'right' ? 'is-on' : ''}`}
              aria-pressed={outputLayout === 'right'}
              title={
                outputLayout === 'right'
                  ? t('Output on the right — switch to output below')
                  : t('Output below — switch to output on the right')
              }
              onClick={() => setOutputLayout((v) => (v === 'right' ? 'inline' : 'right'))}
            >
              <DesktopIcon name="layout" />
              <span className="nls-btn__label">
                {outputLayout === 'right' ? t('Output right') : t('Output below')}
              </span>
            </button>
            <button
              type="button"
              className={`nls-btn ${showOutline ? 'is-on' : ''}`}
              aria-pressed={showOutline}
              title={t('Show or hide the document outline')}
              onClick={() => setShowOutline((v) => !v)}
            >
              <DesktopIcon name="files" />
              <span className="nls-btn__label">{t('Outline')}</span>
            </button>
            <button
              type="button"
              className={`nls-btn ${hideCode ? 'is-on' : ''}`}
              aria-pressed={hideCode}
              title={
                hideCode ? t('Show the code cells') : t('Hide the code cells (shareable view)')
              }
              onClick={() => setHideCode((v) => !v)}
            >
              <DesktopIcon name="editor" />
              <span className="nls-btn__label">{hideCode ? t('Show code') : t('Hide code')}</span>
            </button>
          </div>
        </header>
        <div className="nls-body">
          {showOutline && <Outline editor={editor} version={docVersion} />}
          <div className="nls-scroll">
            <div className="nls-paper">
              <BlockNoteView
                editor={editor}
                theme={dark ? 'dark' : 'light'}
                slashMenu={false}
                onChange={() => {
                  setDocVersion((v) => v + 1);
                  queueMicrotask(refreshStale);
                }}
              >
                <SuggestionMenuController triggerCharacter="/" getItems={getSlashItems} />
              </BlockNoteView>
            </div>
          </div>
        </div>
      </div>
    </NotebookContext.Provider>
  );
}
//=============================================================================
function Outline({ editor, version }: { editor: any; version: number }): JSX.Element {
  void version; // re-render trigger
  const headings = (editor.document as any[])
    .filter((b) => b.type === 'heading')
    .map((b) => ({ id: b.id, level: b.props?.level ?? 1, text: inlineText(b) }))
    .filter((h) => h.text.trim().length > 0);
  const scrollTo = (id: string) =>
    document
      .querySelector(`[data-id="${id}"]`)
      ?.scrollIntoView({ behavior: 'smooth', block: 'start' });
  return (
    <nav className="nls-outline" aria-label={t('Outline')}>
      <div className="nls-outline-title">{t('Outline')}</div>
      {headings.length === 0 && <div className="nls-outline-empty">{t('No sections')}</div>}
      <ul>
        {headings.map((h) => (
          <li key={h.id} className={`nls-outline-l${h.level}`}>
            <button onClick={() => scrollTo(h.id)}>{h.text}</button>
          </li>
        ))}
      </ul>
    </nav>
  );
}
//=============================================================================
