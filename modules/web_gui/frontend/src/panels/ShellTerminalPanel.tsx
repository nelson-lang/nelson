//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef } from 'react';
import { Terminal } from '@xterm/xterm';
import { FitAddon } from '@xterm/addon-fit';
import { rpc } from '../rpc/client';
import { getActiveScheme, subscribeScheme, xtermTheme } from '../desktop/theme';
//=============================================================================
interface ShellRunResult {
  output: string;
  cwd: string;
  exitCode: number;
}
//=============================================================================
// System shell terminal (the Nelson `terminal` feature): a cross-platform
// command runner backed by SystemCommand. Each line runs as its own OS command;
// `cd` is tracked so the working directory persists between commands.
export function ShellTerminalPanel(): JSX.Element {
  const hostRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const host = hostRef.current;
    if (!host) return;

    const term = new Terminal({
      convertEol: true,
      cursorBlink: true,
      fontFamily: 'Consolas, "Cascadia Mono", "DejaVu Sans Mono", monospace',
      fontSize: 13,
      screenReaderMode: true,
      // Follow the desktop theme, not a fixed dark palette.
      theme: xtermTheme(getActiveScheme()),
    });
    const offScheme = subscribeScheme((s) => {
      term.options.theme = xtermTheme(s);
    });
    const fit = new FitAddon();
    term.loadAddon(fit);
    term.open(host);
    fit.fit();

    let buffer = '';
    let cursor = 0;
    let cwd = '';
    let busy = false;
    const history: string[] = [];
    let historyIndex = -1;

    const prompt = () => `\x1b[36m${cwd}\x1b[0m$ `;
    const redraw = () => {
      term.write('\r\x1b[K' + prompt() + buffer);
      const back = buffer.length - cursor;
      if (back > 0) term.write(`\x1b[${back}D`);
    };

    const run = async () => {
      const command = buffer;
      term.write('\r\n');
      buffer = '';
      cursor = 0;
      if (command.trim().length > 0) history.push(command);
      historyIndex = -1;
      busy = true;
      try {
        const r = await rpc<ShellRunResult>('shell.run', { command, cwd });
        if (r.output) term.write(r.output.replace(/\n/g, '\r\n'));
        cwd = r.cwd;
      } catch (err) {
        term.write(`\x1b[31m${String(err)}\x1b[0m\r\n`);
      } finally {
        busy = false;
        term.write(prompt());
      }
    };

    const onData = (data: string) => {
      if (busy) return;
      switch (data) {
        case '\r':
          void run();
          return;
        case '\x7f':
          if (cursor > 0) {
            buffer = buffer.slice(0, cursor - 1) + buffer.slice(cursor);
            cursor--;
            redraw();
          }
          return;
        case '\x1b[D':
          if (cursor > 0) {
            cursor--;
            term.write('\x1b[D');
          }
          return;
        case '\x1b[C':
          if (cursor < buffer.length) {
            cursor++;
            term.write('\x1b[C');
          }
          return;
        case '\x1b[A':
          if (history.length > 0) {
            historyIndex = historyIndex < 0 ? history.length - 1 : Math.max(0, historyIndex - 1);
            buffer = history[historyIndex] ?? '';
            cursor = buffer.length;
            redraw();
          }
          return;
        case '\x1b[B':
          if (historyIndex >= 0) {
            historyIndex++;
            buffer =
              historyIndex >= history.length ? ((historyIndex = -1), '') : history[historyIndex];
            cursor = buffer.length;
            redraw();
          }
          return;
        default:
          if (data >= ' ') {
            buffer = buffer.slice(0, cursor) + data + buffer.slice(cursor);
            cursor += data.length;
            redraw();
          }
      }
    };

    const sub = term.onData(onData);
    const ro = new ResizeObserver(() => {
      try {
        fit.fit();
      } catch {
        /* ignore */
      }
    });
    ro.observe(host);

    term.writeln('\x1b[1mSystem terminal\x1b[0m');
    rpc<ShellRunResult>('shell.run', { command: '', cwd: '' })
      .then((r) => {
        cwd = r.cwd;
      })
      .catch(() => undefined)
      .finally(() => term.write(prompt()));

    return () => {
      offScheme();
      sub.dispose();
      ro.disconnect();
      term.dispose();
    };
  }, []);

  return <div ref={hostRef} className="console-panel" role="region" aria-label="System terminal" />;
}
//=============================================================================
