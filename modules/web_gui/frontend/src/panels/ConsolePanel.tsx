//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
  useSyncExternalStore,
} from 'react';
import { isBackendConnected, onPush, rpc } from '../rpc/client';
import { subscribeScheme } from '../desktop/theme';
import { filterHistory } from '../console/history';
import { registerConsoleInput } from '../desktop/consoleBus';
import { getDockApi, openHelp, openEditor } from '../desktop/desktop';
import { isEngineBusy, submitCommand, subscribeEngine } from '../desktop/engineSession';
import { AnsiParser } from '../console/ansi';
import { linesWithoutTrailingText, removeTrailingTextFromLine } from '../console/lineTrim';
import { getHtmlBlocks, subscribeHtml } from '../desktop/htmlStore';
import { getPrefs, subscribePrefs } from '../desktop/prefs';
import { getPersistentItem, setPersistentItem } from '../desktop/persistentStorage';
import { FONT_ZOOM_EVENT, type FontZoomDetail } from '../desktop/browserZoom';
import { SELECT_CONSOLE_ALL_EVENT } from '../desktop/actions';
import { notifyStartupUrlConsoleReady } from '../desktop/startupUrlActions';
import { DesktopIcon } from '../components/DesktopIcon';
import { insertPromptText } from '../console/promptBuffer';
import {
  exportConsoleHtml,
  printConsole,
  selectElementContents,
} from '../console/transcriptActions';
import { CommandPrompt } from '../components/CommandPrompt';
import {
  CompletionPopup,
  type CompletionItem,
  type CompletionPopupState,
} from '../console/CompletionPopup';
import {
  completionAnchor,
  completionItems,
  type CompletionResponse,
} from '../console/completionModel';
import type { MenuState, PanelApi, Segment } from '../console/consolePanelTypes';
import { ConsoleContextMenu, ConsoleFindBar } from './ConsoleOverlays';
import { RichBlock, TextSegment } from './ConsoleSegments';
//=============================================================================
const MAX_CONSOLE_SCROLLBACK_ROWS = 5000;
export function ConsolePanel({ panelApi }: { panelApi?: PanelApi }): JSX.Element {
  const scrollRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLTextAreaElement>(null);
  const measureRef = useRef<HTMLSpanElement>(null);

  const [segments, setSegments] = useState<Segment[]>([]);
  const [buffer, setBuffer] = useState('');
  const [inputRequest, setInputRequest] = useState<{ id: number; prompt: string } | null>(null);
  const engineBusy = useSyncExternalStore(subscribeEngine, isEngineBusy, () => false);
  const [popup, setPopup] = useState<CompletionPopupState | null>(null);
  const [menu, setMenu] = useState<MenuState | null>(null);
  const [maximized, setMaximized] = useState(false);
  const [fontSize, setFontSize] = useState(() => {
    const n = Number(getPersistentItem('nelson-webview.console.fontSize'));
    return n >= 8 && n <= 40 ? n : 14;
  });
  const [wrap, setWrap] = useState(
    () => getPersistentItem('nelson-webview.console.wrap') !== 'off'
  );
  const [fontFamily, setFontFamily] = useState(() => getPrefs().consoleFontFamily);

  // Focus the initial Command Window only while the page still has neutral
  // focus. React's autoFocus can run late during a busy mount and steal focus
  // back from a panel the user has already selected from the rail.
  useEffect(() => {
    const frame = requestAnimationFrame(() => {
      const active = document.activeElement as HTMLElement | null;
      const neutralFocus =
        active === document.body ||
        active === document.documentElement ||
        !!active?.closest('.nelson-dock');
      if (neutralFocus && getDockApi()?.activePanel?.id === 'console') inputRef.current?.focus();
    });
    return () => cancelAnimationFrame(frame);
  }, []);
  const [find, setFind] = useState<{ term: string; index: number } | null>(null);
  const [matchCount, setMatchCount] = useState(0);
  const findRef = useRef<HTMLInputElement>(null);
  const [, forceScheme] = useState(0);

  const parser = useRef(new AnsiParser());
  const expandables = useRef(new Map<number, string>());
  const history = useRef<string[]>([]);
  const histPrefix = useRef('');
  const histMatches = useRef<number[]>([]);
  const histPos = useRef(-1);
  const lastPrompt = useRef('');
  const pendingOutput = useRef('');
  const pendingOutputFrame = useRef<number | null>(null);
  // True right after a prompt was written with nothing appended since. Used to
  // drop a duplicate prompt push; at boot the engine's first getLine pushes the
  // prompt AND the frontend's term.sync asks it to resend, which would render
  // ">> >>" without this guard.
  const atFreshPrompt = useRef(false);
  // The boot banner ends with an async ping ("connected: ..."); the engine's
  // first prompt push can race it and land mid-banner. Hold that prompt here
  // until the banner is fully written, then release it.
  const bootBannerDone = useRef(false);
  const pendingBootPrompt = useRef('');
  const bufferRef = useRef('');
  const popupRef = useRef<CompletionPopupState | null>(null);
  const focusPromptOnReady = useRef(false);
  const stuckToBottom = useRef(true);
  bufferRef.current = buffer;
  popupRef.current = popup;

  const flushText = useCallback(() => {
    parser.current.trimRows(MAX_CONSOLE_SCROLLBACK_ROWS);
    const lines = parser.current.lines;
    setSegments((prev) => {
      const next = prev.slice();
      const last = next[next.length - 1];
      if (last && last.kind === 'text') {
        next[next.length - 1] = { kind: 'text', lines };
      } else {
        next.push({ kind: 'text', lines });
      }
      return next;
    });
  }, []);

  const writeOutput = useCallback(
    (text: string) => {
      if (!text) return;
      parser.current.write(text);
      flushText();
    },
    [flushText]
  );

  const flushPendingOutput = useCallback(() => {
    if (pendingOutputFrame.current !== null) {
      cancelAnimationFrame(pendingOutputFrame.current);
      pendingOutputFrame.current = null;
    }
    const text = pendingOutput.current;
    if (!text) return;
    pendingOutput.current = '';
    writeOutput(text);
  }, [writeOutput]);

  const queueOutput = useCallback(
    (text: string) => {
      if (!text) return;
      pendingOutput.current += text;
      if (pendingOutputFrame.current !== null) return;
      pendingOutputFrame.current = requestAnimationFrame(() => {
        pendingOutputFrame.current = null;
        const queued = pendingOutput.current;
        pendingOutput.current = '';
        writeOutput(queued);
      });
    },
    [writeOutput]
  );

  const hideFreshPrompt = useCallback(() => {
    flushPendingOutput();
    if (lastPrompt.current) {
      // Tolerate output/busy races: a gallery or tool action can start between a
      // prompt push and React's busy render. If the row still ends with the prompt,
      // remove it whatever atFreshPrompt says.
      const removed = parser.current.removeTrailingText(lastPrompt.current);
      if (removed) flushText();
      setSegments((prev) => {
        for (let segIndex = prev.length - 1; segIndex >= 0; segIndex -= 1) {
          const segment = prev[segIndex];
          if (segment.kind !== 'text') continue;
          const lineIndex = segment.lines.length - 1;
          if (lineIndex < 0) return prev;
          const trimmed = removeTrailingTextFromLine(segment.lines[lineIndex], lastPrompt.current);
          if (!trimmed) return prev;
          const next = prev.slice();
          const lines = segment.lines.slice();
          lines[lineIndex] = trimmed;
          next[segIndex] = { ...segment, lines };
          return next;
        }
        return prev;
      });
    }
    atFreshPrompt.current = false;
  }, [flushPendingOutput, flushText]);

  const resetHistoryNav = () => {
    histPos.current = -1;
  };

  const focusPrompt = useCallback(() => {
    requestAnimationFrame(() => {
      if (getDockApi()?.activePanel && getDockApi()?.activePanel?.id !== 'console') return;
      inputRef.current?.focus();
    });
  }, []);

  const resetClearedScroll = useCallback(() => {
    stuckToBottom.current = true;
    requestAnimationFrame(() => {
      const host = scrollRef.current;
      if (host) host.scrollTop = 0;
    });
  }, []);

  const runCommand = useCallback(
    async (line: string) => {
      atFreshPrompt.current = false;
      writeOutput(line + '\n');
      if (line.trim().length > 0) history.current.push(line);
      try {
        await submitCommand(line);
      } catch (err) {
        writeOutput(`\x1b[31m${String(err)}\x1b[0m\n`);
      }
    },
    [writeOutput]
  );

  const interruptCommand = useCallback(() => {
    const requestedWhileBusy = engineBusy || isEngineBusy() || inputRef.current === null;
    atFreshPrompt.current = false; // ^C leaves the prompt; let the re-prompt render
    writeOutput('^C\n');
    setBuffer('');
    resetHistoryNav();
    void rpc('term.interrupt', { busy: requestedWhileBusy }).catch(() => undefined);
  }, [engineBusy, writeOutput]);

  const handlePrompt = useCallback(
    (p: unknown) => {
      flushPendingOutput();
      const value = (p as { prompt?: string }).prompt;
      if (typeof value === 'string' && value.length > 0) {
        // The prompt is part of the output stream: writing it into the transcript lets
        // the editable input continue the same line (after cprintf('Hello') it reads
        // "Hello>> ").
        lastPrompt.current = value;
        if (isEngineBusy()) {
          // A prompt can race an externally started evaluation (examples gallery,
          // toolbar actions, a delayed term.sync reply). While busy the Command Window
          // shows no prompt; the end-of-evaluation term.prompt renders it.
          atFreshPrompt.current = false;
          return;
        }
        if (!bootBannerDone.current) {
          // Boot banner still being written: park the prompt for later.
          pendingBootPrompt.current = value;
        } else if (!atFreshPrompt.current) {
          // Idempotent: skip a back-to-back duplicate prompt (the boot sync).
          writeOutput(value);
          atFreshPrompt.current = true;
          notifyStartupUrlConsoleReady();
          if (focusPromptOnReady.current) {
            focusPromptOnReady.current = false;
            focusPrompt();
          }
        }
      }
    },
    [flushPendingOutput, focusPrompt, writeOutput]
  );

  useEffect(() => {
    if (engineBusy) hideFreshPrompt();
  }, [engineBusy, hideFreshPrompt]);

  useEffect(() => {
    const offScheme = subscribeScheme(() => forceScheme((n) => n + 1));

    const offOutput = onPush('term.output', (p) => {
      const text = (p as { text?: string }).text ?? '';
      if (text) atFreshPrompt.current = false;
      queueOutput(text);
    });
    const offBusy = onPush('term.busy', () => {
      // Commands started outside the command window (examples, toolbar actions) do
      // not echo a line that consumes the prompt. Hide the fresh prompt while busy,
      // as the Qt desktop does.
      hideFreshPrompt();
    });
    const offExpandable = onPush('term.expandable', (p) => {
      const { id, text } = p as { id?: number; text?: string };
      if (typeof id === 'number' && typeof text === 'string') expandables.current.set(id, text);
    });
    const offPrompt = onPush('term.prompt', (p) => {
      handlePrompt(p);
    });
    const offInputRequest = onPush('term.inputRequest', (p) => {
      const request = p as { id?: unknown; prompt?: unknown };
      if (typeof request.id !== 'number' || !Number.isFinite(request.id)) return;
      flushPendingOutput();
      const prompt = typeof request.prompt === 'string' ? request.prompt : '';
      if (prompt) writeOutput(prompt);
      setBuffer('');
      resetHistoryNav();
      setInputRequest({ id: request.id, prompt });
      requestAnimationFrame(() => inputRef.current?.focus());
    });
    const offClear = onPush('term.clear', () => {
      flushPendingOutput();
      parser.current.clear();
      setSegments([]);
      resetClearedScroll();
      // clc keeps a prompt on the cleared screen only when the interpreter is idle.
      if (lastPrompt.current && !isEngineBusy()) {
        writeOutput(lastPrompt.current);
        atFreshPrompt.current = true;
        focusPrompt();
      } else {
        atFreshPrompt.current = false;
      }
    });
    // inserthtml / markdown / LaTeX: freeze the current text, drop in a rich
    // block, and continue text after it, inline, in arrival order.
    let htmlSeen = getHtmlBlocks().length;
    const offHtml = subscribeHtml(() => {
      const blocks = getHtmlBlocks();
      const fresh = blocks.slice(htmlSeen);
      htmlSeen = blocks.length;
      if (fresh.length === 0) return;
      flushPendingOutput();
      const frozen = parser.current.lines;
      parser.current.clear();
      setSegments((prev) => {
        const next = prev.slice();
        const last = next[next.length - 1];
        if (last && last.kind === 'text') {
          next[next.length - 1] = { kind: 'text', lines: frozen };
        }
        for (const html of fresh) next.push({ kind: 'html', html });
        return next;
      });
    });

    // Skip the "%% -- date -- %%" history markers.
    void rpc<{ entries?: string[] }>('history.list')
      .then((r) => {
        const seed = (r.entries ?? []).filter((e) => e && !e.startsWith('%%'));
        if (seed.length) history.current.unshift(...seed);
      })
      .catch(() => undefined);

    writeOutput('\x1b[1mNelson\x1b[0m - web desktop (nelson-webview)\n');
    void rpc<{ backend?: string; version?: string }>('ping')
      .then((info) => {
        const backend = isBackendConnected() ? info.backend ?? 'engine' : 'mock (no backend)';
        writeOutput(
          `\x1b[90mconnected: ${backend}${info.version ? ' v' + info.version : ''}\x1b[0m\n`
        );
      })
      .catch((err) => writeOutput(`\x1b[31mping failed: ${String(err)}\x1b[0m\n`))
      .finally(() => {
        // Banner complete: release a prompt that raced it, then ask the engine
        // to resend in case none arrived yet (idempotent via atFreshPrompt).
        bootBannerDone.current = true;
        if (pendingBootPrompt.current) {
          handlePrompt({ prompt: pendingBootPrompt.current });
          pendingBootPrompt.current = '';
        }
        void rpc<{ prompt?: string }>('term.sync')
          .then(handlePrompt)
          .catch(() => undefined);
      });

    registerConsoleInput((text) => {
      setBuffer(text);
      histPos.current = -1;
      inputRef.current?.focus();
    });

    return () => {
      offScheme();
      offOutput();
      offBusy();
      offExpandable();
      offPrompt();
      offInputRequest();
      offClear();
      offHtml();
      if (pendingOutputFrame.current !== null) {
        cancelAnimationFrame(pendingOutputFrame.current);
        pendingOutputFrame.current = null;
      }
    };
  }, [
    flushPendingOutput,
    focusPrompt,
    handlePrompt,
    hideFreshPrompt,
    queueOutput,
    resetClearedScroll,
    writeOutput,
  ]);

  useEffect(() => {
    const host = scrollRef.current;
    const cell = measureRef.current;
    if (!host || !cell) return;
    const report = () => {
      const cw = cell.getBoundingClientRect().width / 10 || 8;
      const lh = cell.getBoundingClientRect().height || 18;
      const cols = Math.max(20, Math.floor((host.clientWidth - 12) / cw));
      const rows = Math.max(5, Math.floor(host.clientHeight / lh));
      void rpc('term.resize', { cols, rows }).catch(() => undefined);
    };
    report();
    const ro = new ResizeObserver(report);
    ro.observe(host);
    return () => ro.disconnect();
  }, []);

  // Keep the view pinned to the bottom on new content unless the user scrolled
  // up to read back.
  useLayoutEffect(() => {
    const host = scrollRef.current;
    if (host && stuckToBottom.current) host.scrollTop = host.scrollHeight;
  }, [segments, buffer]);
  const onScroll = () => {
    const host = scrollRef.current;
    if (!host) return;
    stuckToBottom.current = host.scrollHeight - host.scrollTop - host.clientHeight < 24;
  };

  useEffect(() => {
    setPersistentItem('nelson-webview.console.fontSize', String(fontSize));
  }, [fontSize]);
  useEffect(() => {
    setPersistentItem('nelson-webview.console.wrap', wrap ? 'on' : 'off');
  }, [wrap]);
  useEffect(() => {
    return subscribePrefs((p) => {
      setFontSize(p.consoleFontSize);
      setFontFamily(p.consoleFontFamily);
      setWrap(p.consoleWrap);
    });
  }, []);
  // Browser zoom is locked globally; the lock re-emits the gestures as
  // FONT_ZOOM_EVENT so the console keeps its accessible font zoom
  // (Ctrl+= / Ctrl+- / Ctrl+0, and Ctrl+wheel over the transcript).
  useEffect(() => {
    const onFontZoom = (event: Event) => {
      const detail = (event as CustomEvent<FontZoomDetail>).detail;
      if (!detail) return;
      const origin = detail.origin;
      const withinConsole =
        origin instanceof Node &&
        ((scrollRef.current?.contains(origin) ?? false) ||
          (inputRef.current?.contains(origin) ?? false));
      if (!withinConsole) return;
      if (detail.delta === 0) {
        setFontSize(14);
      } else {
        setFontSize((n) => Math.max(8, Math.min(40, n + detail.delta)));
      }
    };
    window.addEventListener(FONT_ZOOM_EVENT, onFontZoom);
    return () => window.removeEventListener(FONT_ZOOM_EVENT, onFontZoom);
  }, []);
  useLayoutEffect(() => {
    const host = scrollRef.current;
    if (!host || !find) return;
    const marks = host.querySelectorAll<HTMLElement>('.console-find');
    setMatchCount(marks.length);
    marks.forEach((m, i) => m.classList.toggle('console-find--current', i === find.index));
    marks[find.index]?.scrollIntoView({ block: 'center' });
  }, [find, segments]);
  const stepFind = (delta: number) => {
    if (matchCount === 0) return;
    setFind((f) => (f ? { ...f, index: (f.index + delta + matchCount) % matchCount } : f));
  };
  const openFind = () => {
    const sel = window.getSelection()?.toString().trim();
    setFind({ term: sel && !sel.includes('\n') ? sel : find?.term ?? '', index: 0 });
    setTimeout(() => findRef.current?.select(), 0);
  };

  const selectAll = useCallback(() => {
    const host = scrollRef.current;
    if (!host) return;
    setMenu(null);
    inputRef.current?.blur();
    selectElementContents(host);
  }, []);

  useEffect(() => {
    window.addEventListener(SELECT_CONSOLE_ALL_EVENT, selectAll);
    return () => window.removeEventListener(SELECT_CONSOLE_ALL_EVENT, selectAll);
  }, [selectAll]);

  const submit = async () => {
    const line = bufferRef.current;
    if (inputRequest) {
      setBuffer('');
      resetHistoryNav();
      atFreshPrompt.current = false;
      writeOutput(line + '\n');
      const request = inputRequest;
      setInputRequest(null);
      await rpc('term.inputResponse', { id: request.id, value: line }).catch((err) => {
        writeOutput(`\x1b[31m${String(err)}\x1b[0m\n`);
      });
      return;
    }
    if (isEngineBusy()) return;
    focusPromptOnReady.current = document.activeElement === inputRef.current;
    setBuffer('');
    resetHistoryNav();
    await runCommand(line);
  };

  const acceptCompletion = (item: CompletionItem, prefix: string) => {
    const input = inputRef.current;
    const caret = input?.selectionStart ?? bufferRef.current.length;
    // Backend contract: prefix is the exact suffix of the submitted line that
    // every item replaces (possibly empty after a trailing dot or separator).
    const before = bufferRef.current.slice(0, caret);
    const start = before.endsWith(prefix) ? caret - prefix.length : caret;
    const next = bufferRef.current.slice(0, start) + item.text + bufferRef.current.slice(caret);
    setBuffer(next);
    setPopup(null);
    popupRef.current = null;
    inputRef.current?.focus();
  };

  const complete = async () => {
    if (isEngineBusy()) return;
    const input = inputRef.current;
    const caret = input?.selectionStart ?? buffer.length;
    try {
      const res = await rpc<CompletionResponse>('completion.request', {
        line: buffer.slice(0, caret),
        cursor: caret,
      });
      const items = completionItems(res);
      if (items.length === 0) return;
      if (items.length === 1) {
        acceptCompletion(items[0], res.prefix);
        return;
      }
      const state = {
        items,
        sel: 0,
        prefix: res.prefix,
        anchor: completionAnchor(input, caret, res.prefix),
      };
      popupRef.current = state;
      setPopup(state);
    } catch {
      /* completion is best-effort */
    }
  };

  const navPopup = (delta: number) => {
    const p = popupRef.current;
    if (!p) return;
    const sel = Math.max(0, Math.min(p.items.length - 1, p.sel + delta));
    const next = { ...p, sel };
    popupRef.current = next;
    setPopup(next);
  };

  const recallHistory = (dir: -1 | 1) => {
    if (dir === -1) {
      if (histPos.current === -1) {
        histPrefix.current = bufferRef.current;
        histMatches.current = filterHistory(history.current, histPrefix.current);
        if (histMatches.current.length === 0) return;
        histPos.current = histMatches.current.length - 1;
      } else if (histPos.current > 0) {
        histPos.current--;
      } else {
        return;
      }
      setBuffer(history.current[histMatches.current[histPos.current]]);
    } else {
      if (histPos.current === -1) return;
      if (histPos.current < histMatches.current.length - 1) {
        histPos.current++;
        setBuffer(history.current[histMatches.current[histPos.current]]);
      } else {
        histPos.current = -1;
        setBuffer(histPrefix.current);
      }
    }
  };

  const onKeyDown = (e: React.KeyboardEvent<HTMLTextAreaElement>) => {
    if (e.defaultPrevented) return;
    if ((e.ctrlKey || e.metaKey) && !e.altKey && (e.key === 'a' || e.key === 'A')) {
      e.preventDefault();
      selectAll();
      return;
    }
    if (e.ctrlKey && (e.key === 'f' || e.key === 'F')) {
      e.preventDefault();
      openFind();
      return;
    }
    if (popupRef.current && !inputRequest) {
      switch (e.key) {
        case 'ArrowUp':
          e.preventDefault();
          navPopup(-1);
          return;
        case 'ArrowDown':
          e.preventDefault();
          navPopup(1);
          return;
        case 'Enter':
        case 'Tab': {
          e.preventDefault();
          const p = popupRef.current;
          acceptCompletion(p.items[p.sel], p.prefix);
          return;
        }
        case 'Escape':
          e.preventDefault();
          setPopup(null);
          popupRef.current = null;
          return;
        default:
          setPopup(null);
          popupRef.current = null;
      }
    }
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      void submit();
    } else if (e.key === 'Tab' && !inputRequest) {
      e.preventDefault();
      void complete();
    } else if (e.key === 'ArrowUp' && !inputRequest && !bufferRef.current.includes('\n')) {
      e.preventDefault();
      recallHistory(-1);
    } else if (e.key === 'ArrowDown' && !inputRequest && !bufferRef.current.includes('\n')) {
      e.preventDefault();
      recallHistory(1);
    } else if ((e.key === 'c' || e.key === 'C') && e.ctrlKey && !e.altKey && !e.metaKey) {
      // Ctrl+C copies when text is selected (browser default). Otherwise it
      // interrupts: at the prompt it clears the line, during a command it raises the
      // interrupt flag the evaluator polls.
      if ((window.getSelection()?.toString() ?? '').length > 0) {
        return;
      }
      e.preventDefault();
      e.nativeEvent.stopImmediatePropagation();
      interruptCommand();
    }
  };

  useEffect(() => {
    if (!engineBusy) return;
    const onWindowKeyDown = (event: KeyboardEvent) => {
      if (event.defaultPrevented || event.repeat) return;
      if (!(event.ctrlKey && !event.altKey && !event.metaKey)) return;
      if (event.key !== 'c' && event.key !== 'C') return;
      if ((window.getSelection()?.toString() ?? '').length > 0) return;
      const activePanel = getDockApi()?.activePanel;
      const activeElement = document.activeElement;
      const commandWindowActive =
        activePanel?.id === 'console' ||
        (activeElement instanceof Node && scrollRef.current?.contains(activeElement));
      if (!commandWindowActive) return;
      event.preventDefault();
      event.stopImmediatePropagation();
      interruptCommand();
    };
    window.addEventListener('keydown', onWindowKeyDown, true);
    return () => window.removeEventListener('keydown', onWindowKeyDown, true);
  }, [engineBusy, interruptCommand]);

  // Expand internal links in place; open web links in a new tab.
  const onLink = (uri: string) => {
    const m = /^nelson-expand:(\d+)$/.exec(uri);
    if (m) {
      const text = expandables.current.get(Number(m[1]));
      if (text) {
        atFreshPrompt.current = false;
        writeOutput('\n' + text + '\n');
      }
      return;
    }
    const open = /^nelson-open:\?line=(\d+)&file=(.*)$/.exec(uri);
    if (open) {
      const line = Number(open[1]);
      const path = decodeURIComponent(open[2]);
      if (path) openEditor(path, line > 0 ? line : undefined);
      return;
    }
    if (/^https?:/i.test(uri)) window.open(uri, '_blank', 'noopener');
  };

  const onContextMenu = (e: React.MouseEvent) => {
    e.preventDefault();
    // Fixed viewport coordinates keep the menu within the panel edge.
    setMenu({
      x: e.clientX,
      y: e.clientY,
      hasSelection: (window.getSelection()?.toString() ?? '').length > 0,
    });
  };
  useEffect(() => {
    if (!menu) return;
    const close = () => setMenu(null);
    window.addEventListener('click', close);
    window.addEventListener('resize', close);
    return () => {
      window.removeEventListener('click', close);
      window.removeEventListener('resize', close);
    };
  }, [menu]);
  const selectionText = () => window.getSelection()?.toString() ?? '';
  const doCopy = () => {
    const sel = selectionText();
    if (sel) void navigator.clipboard?.writeText(sel).catch(() => undefined);
    setMenu(null);
  };
  const insertText = (text: string) => {
    const input = inputRef.current;
    const inserted = insertPromptText(
      bufferRef.current,
      text,
      input?.selectionStart ?? bufferRef.current.length,
      input?.selectionEnd ?? bufferRef.current.length
    );
    setBuffer(inserted.value);
    resetHistoryNav();
    requestAnimationFrame(() => {
      inputRef.current?.focus();
      inputRef.current?.setSelectionRange(inserted.caret, inserted.caret);
    });
  };
  const doPaste = () => {
    void navigator.clipboard
      ?.readText()
      .then(insertText)
      .catch(() => undefined);
    setMenu(null);
  };
  const doClear = () => {
    parser.current.clear();
    setSegments([]);
    resetClearedScroll();
    // Keep a prompt on the cleared screen only when idle (same as term.clear).
    if (lastPrompt.current && !isEngineBusy()) {
      writeOutput(lastPrompt.current);
      atFreshPrompt.current = true;
    } else {
      atFreshPrompt.current = false;
    }
    setMenu(null);
    inputRef.current?.focus();
  };
  const doHelp = () => {
    const sel = selectionText().trim();
    if (sel) openHelp(sel);
    setMenu(null);
  };
  const exportHtml = () => {
    setMenu(null);
    const host = scrollRef.current;
    if (!host) return;
    exportConsoleHtml(host);
  };
  const doPrint = () => {
    setMenu(null);
    // Print in a popup, never window.print() on the main window: the print
    // dialog blocks the JS event loop, which stalls the heartbeat and makes the
    // backend watchdog quit nelson-webview after a few seconds.
    const host = scrollRef.current;
    if (!host) return;
    printConsole(host);
  };

  const toggleMaximize = () => {
    if (!panelApi) return;
    if (panelApi.isMaximized()) {
      panelApi.exitMaximized();
      setMaximized(false);
    } else {
      panelApi.maximize();
      setMaximized(true);
    }
  };

  return (
    <div
      className="console-panel"
      role="region"
      aria-label="Command Window"
      onContextMenu={onContextMenu}
    >
      {panelApi && (
        <div className="console-toolbar">
          <button onClick={toggleMaximize} title={maximized ? 'Restore' : 'Full window'}>
            <DesktopIcon name={maximized ? 'restore' : 'maximize'} />
          </button>
        </div>
      )}
      {find && (
        <ConsoleFindBar
          find={find}
          findRef={findRef}
          matchCount={matchCount}
          onClose={() => {
            setFind(null);
            inputRef.current?.focus();
          }}
          onStep={stepFind}
          onTermChange={(term) => setFind({ term, index: 0 })}
        />
      )}
      <span
        ref={measureRef}
        className="console-measure"
        aria-hidden
        style={{ fontSize, fontFamily }}
      >
        0123456789
      </span>
      <div
        ref={scrollRef}
        className={'console-host xterm-rows' + (wrap ? '' : ' console-host--nowrap')}
        role="region"
        aria-label="Command Window transcript"
        style={{ fontSize, fontFamily }}
        onScroll={onScroll}
        onMouseUp={() => {
          if (!engineBusy && !selectionText()) inputRef.current?.focus();
        }}
      >
        {(() => {
          // Continue the backend-owned prompt inline on the final text row.
          const lastTextIndex = segments.map((s) => s.kind).lastIndexOf('text');
          const inputEl =
            engineBusy && !inputRequest ? undefined : (
              <CommandPrompt
                ref={inputRef}
                value={buffer}
                wrapLines={wrap}
                onContextMenu={onContextMenu}
                onChange={(e) => {
                  setBuffer(e.currentTarget.value);
                  resetHistoryNav();
                }}
                onKeyDown={onKeyDown}
                onPaste={(e) => {
                  e.preventDefault();
                  insertText(e.clipboardData.getData('text'));
                }}
              />
            );
          const nodes = segments.map((seg, i) =>
            seg.kind === 'text' ? (
              <TextSegment
                key={i}
                lines={
                  engineBusy && i === lastTextIndex
                    ? linesWithoutTrailingText(seg.lines, lastPrompt.current)
                    : seg.lines
                }
                onLink={onLink}
                highlight={find?.term || undefined}
                tail={i === lastTextIndex ? inputEl : undefined}
              />
            ) : (
              <RichBlock key={i} html={seg.html} />
            )
          );
          if (lastTextIndex === -1) {
            nodes.push(
              <div className="console-line console-line--active" key="input">
                {inputEl}
              </div>
            );
          }
          return nodes;
        })()}
      </div>

      <CompletionPopup popup={popup} onAccept={acceptCompletion} />

      {menu && (
        <ConsoleContextMenu
          menu={menu}
          onClear={doClear}
          onCopy={doCopy}
          onExportHtml={exportHtml}
          onFind={() => {
            setMenu(null);
            openFind();
          }}
          onHelp={doHelp}
          onPaste={doPaste}
          onPrint={doPrint}
          onSelectAll={selectAll}
          onToggleWrap={() => {
            setWrap((v) => !v);
            setMenu(null);
          }}
          wrap={wrap}
        />
      )}
    </div>
  );
}
//=============================================================================
