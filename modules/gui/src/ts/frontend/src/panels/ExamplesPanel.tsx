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
import { mountExampleGallery, type ExampleEntry } from '@nelson/example-gallery/example-gallery.js';
import '@nelson/example-gallery/gallery-shared.css';
import { onPush, rpc } from '../rpc/client';
import { runCommand } from '../desktop/engineSession';
//=============================================================================
// Examples gallery docked as a panel: the shared component over the
// `examples.list` RPC. Running an example is a backend-owned desktop action,
// not typed Command Window input: the backend hides the current prompt, marks
// the interpreter busy and re-prompts when it is ready again.
function quote(value: string): string {
  return `'${value.replace(/'/g, "''")}'`;
}
//=============================================================================
// The command line launched for an entry: a script runs, a command entry is
// its own call, and an NFlow model opens in the block-diagram editor.
export function exampleCommand(entry: ExampleEntry): string {
  if (entry.kind === 'command') return String(entry.command ?? '');
  if (entry.kind === 'model') return `nflow(${quote(entry.file ?? '')})`;
  return `run(${quote(entry.file ?? '')})`;
}
//=============================================================================
function exampleParams(entry: ExampleEntry): Record<string, unknown> {
  if (entry.kind === 'command') return { command: entry.command ?? '' };
  return { path: entry.file ?? '' };
}
//=============================================================================
async function dispatchExampleAction(
  method: 'examples.run' | 'examples.open',
  entry: ExampleEntry
): Promise<void> {
  try {
    await rpc(method, exampleParams(entry));
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    if (!/method not implemented|unknown method/i.test(message)) throw error;
    // Older backends only expose term.input. The WebView and Wasm backends use
    // examples.run/open so the prompt is handled.
    await runCommand(exampleCommand(entry));
  }
}
//=============================================================================
export function ExamplesPanel(): JSX.Element {
  const hostRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const host = hostRef.current;
    if (!host) return undefined;
    const gallery = mountExampleGallery(host, {
      primarySource: 'nelson',
      hasEditor: true,
      compact: 'auto',
      load: ({ refresh }) => rpc(refresh ? 'examples.refresh' : 'examples.list'),
      onRun: (entry) => dispatchExampleAction('examples.run', entry),
      onOpen: (entry) => dispatchExampleAction('examples.open', entry),
    });
    const unsubscribe = onPush('examples.changed', () => {
      void gallery.reload(true);
    });
    return () => {
      unsubscribe();
      gallery.destroy();
    };
  }, []);

  return <div ref={hostRef} className="examples-panel" />;
}
//=============================================================================
