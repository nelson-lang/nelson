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
import { registerTerminal } from '../desktop/terminalRegistry';
import { t } from '../i18n';
//=============================================================================
interface ShellRunResult {
  output: string;
  cwd: string;
  exitCode: number;
}
//=============================================================================
// Maps a scriptable terminal's Theme ("auto" | "light" | "dark") to an xterm
// palette; "auto" (or anything else) follows the desktop scheme.
function themeForName(name: string | undefined): ReturnType<typeof xtermTheme> {
  if (name === 'dark' || name === 'light') return xtermTheme(name);
  return xtermTheme(getActiveScheme());
}
//=============================================================================
// System shell terminal (the Nelson `terminal` feature): a cross-platform
// command runner backed by SystemCommand. Each line runs as its own OS command;
// `cd` is tracked so the working directory persists between commands. With a
// `handle` the panel is driven by the engine's scriptable `terminal` object
// (run / read / clear / focus / theme) in addition to the user's own typing.
export function ShellTerminalPanel({
  handle,
  theme,
}: {
  handle?: number;
  theme?: string;
}): JSX.Element {
  const hostRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const host = hostRef.current;
    if (!host) return;

    let themeOverride = theme;
    const term = new Terminal({
      convertEol: true,
      cursorBlink: true,
      fontFamily: 'Consolas, "Cascadia Mono", "DejaVu Sans Mono", monospace',
      fontSize: 13,
      screenReaderMode: true,
      theme: themeOverride ? themeForName(themeOverride) : xtermTheme(getActiveScheme()),
    });
    const offScheme = subscribeScheme((s) => {
      term.options.theme = themeOverride ? themeForName(themeOverride) : xtermTheme(s);
    });
    const fit = new FitAddon();
    term.loadAddon(fit);
    term.open(host);
    fit.fit();

    let buffer = '';
    let cursor = 0;
    let cwd = '';
    let busy = false;
    let transcript = '';
    const history: string[] = [];
    let historyIndex = -1;
    // Commands queued while one is running (user typing + scripted runCommand).
    const pending: { command: string; echo: boolean }[] = [];

    const prompt = () => `\x1b[36m${cwd}\x1b[0m$ `;
    const redraw = () => {
      term.write('\r\x1b[K' + prompt() + buffer);
      const back = buffer.length - cursor;
      if (back > 0) term.write(`\x1b[${back}D`);
    };
    const appendTranscript = (text: string) => {
      transcript += (transcript.length > 0 ? '\n' : '') + text;
    };

    const runNext = async () => {
      if (busy) return;
      const next = pending.shift();
      if (!next) return;
      busy = true;
      const command = next.command;
      if (next.echo) {
        // Scripted command: nothing is on the line yet, so draw it like a typed one.
        term.write('\r\x1b[K' + prompt() + command + '\r\n');
      } else {
        // Typed command: already shown inline, just break the line.
        term.write('\r\n');
      }
      appendTranscript(command);
      try {
        const r = await rpc<ShellRunResult>('shell.run', { command, cwd });
        if (r.output) {
          term.write(r.output.replace(/\n/g, '\r\n'));
          appendTranscript(r.output);
        }
        cwd = r.cwd;
      } catch (err) {
        const message = String(err);
        term.write(`\x1b[31m${message}\x1b[0m\r\n`);
        appendTranscript(message);
      } finally {
        busy = false;
        term.write(prompt());
        void runNext();
      }
    };

    const enqueue = (command: string, echo: boolean) => {
      pending.push({ command, echo });
      void runNext();
    };

    const onData = (data: string) => {
      if (busy) return;
      switch (data) {
        case '\r': {
          const command = buffer;
          buffer = '';
          cursor = 0;
          if (command.trim().length > 0) history.push(command);
          historyIndex = -1;
          enqueue(command, false);
          return;
        }
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

    term.writeln('\x1b[1m' + t('System terminal') + '\x1b[0m');
    rpc<ShellRunResult>('shell.run', { command: '', cwd: '' })
      .then((r) => {
        cwd = r.cwd;
      })
      .catch(() => undefined)
      .finally(() => term.write(prompt()));

    // Expose the scriptable surface for the engine's `terminal` object.
    const offRegister =
      typeof handle === 'number'
        ? registerTerminal(handle, {
            runCommand: (command: string) => enqueue(command, true),
            clear: () => {
              term.clear();
              transcript = '';
            },
            focus: () => term.focus(),
            setProps: (props) => {
              if (typeof props.theme === 'string') {
                themeOverride = props.theme;
                term.options.theme = themeForName(themeOverride);
              }
            },
            getTranscript: () => transcript,
          })
        : undefined;

    return () => {
      offRegister?.();
      offScheme();
      sub.dispose();
      ro.disconnect();
      term.dispose();
    };
  }, [handle, theme]);

  return (
    <div ref={hostRef} className="console-panel" role="region" aria-label={t('System terminal')} />
  );
}
//=============================================================================
