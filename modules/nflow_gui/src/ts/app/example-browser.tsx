//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Example browser: the shared examples gallery hosted in a light-dismiss
// sheet over the editor. Opening a model closes the sheet (the canvas is
// replaced); running a script keeps it open so several demos can be tried.
//=============================================================================
import { useEffect, useRef, useState } from 'react';
import { mountExampleGallery, type ExampleEntry } from '@nelson/example-gallery/example-gallery.js';
import { listExamples, examplesAvailable } from './example-service.js';
import { renderThumbnailSvg } from './example-thumbnail.js';
import { nflowRpc, canUseNFlowRpc } from './native-rpc.js';
import { openLibraryManager } from './library-manager.js';
import {
  EXAMPLE_BROWSER_SHOW_EVENT,
  EXAMPLE_BROWSER_HIDE_EVENT,
  OPEN_MODEL_PATH_EVENT,
} from './components/modal-events.js';
import { t } from '../utils/i18n.js';
//=============================================================================
function escapeNelsonString(value: string): string {
  return value.replace(/'/g, "''");
}
//=============================================================================
async function readExampleText(path: string): Promise<string | null> {
  const raw = await nflowRpc<string>('nflow.file.readText', { path });
  const res = JSON.parse(String(raw ?? '')) as { success?: boolean; content?: string };
  if (res.success !== true || typeof res.content !== 'string') return null;
  return res.content;
}
//=============================================================================
async function modelThumbnail(entry: ExampleEntry): Promise<string | null> {
  if (entry.kind !== 'model' || !entry.file || !canUseNFlowRpc('nflow.file.readText')) {
    return null;
  }
  try {
    const text = await readExampleText(entry.file);
    return text ? renderThumbnailSvg(JSON.parse(text)) : null;
  } catch (_e) {
    return null;
  }
}
//=============================================================================
function openExampleModel(path: string): void {
  document.dispatchEvent(
    new CustomEvent(OPEN_MODEL_PATH_EVENT, { cancelable: true, detail: { path } })
  );
}
//=============================================================================
async function runExample(entry: ExampleEntry): Promise<void> {
  if (!canUseNFlowRpc('nflow.callback.invoke')) {
    throw new Error(t('Nelson is not connected.'));
  }
  const code =
    entry.kind === 'command'
      ? String(entry.command ?? '')
      : `run('${escapeNelsonString(entry.file ?? '')}')`;
  await nflowRpc('nflow.callback.invoke', { code, blockId: '' });
}
//=============================================================================
function ExampleBrowser({ onClose }: { onClose: () => void }): JSX.Element {
  const hostRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    const host = hostRef.current;
    if (!host) return undefined;
    const gallery = mountExampleGallery(host, {
      primarySource: 'nflow',
      hasEditor: false,
      compact: 'auto',
      translate: (key) => t(key),
      load: ({ refresh }) => listExamples({ refresh }),
      onRun: (entry) => runExample(entry),
      onOpen: (entry) => {
        if (entry.kind === 'model' && entry.file) {
          openExampleModel(entry.file);
          onClose();
        }
      },
      thumbnail: (entry) => modelThumbnail(entry),
      onClose,
      footerAction: {
        label: t('Manage example locations'),
        onClick: () => {
          onClose();
          openLibraryManager();
        },
      },
    });
    gallery.focusSearch();
    return () => gallery.destroy();
  }, [onClose]);

  useEffect(() => {
    const onKey = (e: KeyboardEvent): void => {
      if (e.key === 'Escape') onClose();
    };
    document.addEventListener('keydown', onKey);
    return () => document.removeEventListener('keydown', onKey);
  }, [onClose]);

  return (
    <div
      className="nf-ex-overlay"
      role="presentation"
      onClick={(e) => {
        if (e.target === e.currentTarget) onClose();
      }}
    >
      <div
        ref={hostRef}
        className="nf-ex-dialog"
        role="dialog"
        aria-modal="true"
        aria-label={t('Examples')}
      />
    </div>
  );
}
//=============================================================================
export function ExampleBrowserModal(): JSX.Element | null {
  const [open, setOpen] = useState(false);

  useEffect(() => {
    const show = (event: Event): void => {
      event.preventDefault();
      setOpen(true);
    };
    const hide = (event: Event): void => {
      event.preventDefault();
      setOpen(false);
    };
    document.addEventListener(EXAMPLE_BROWSER_SHOW_EVENT, show);
    document.addEventListener(EXAMPLE_BROWSER_HIDE_EVENT, hide);
    return () => {
      document.removeEventListener(EXAMPLE_BROWSER_SHOW_EVENT, show);
      document.removeEventListener(EXAMPLE_BROWSER_HIDE_EVENT, hide);
    };
  }, []);

  if (!open) return null;
  return <ExampleBrowser onClose={() => setOpen(false)} />;
}
//=============================================================================
export function openExampleBrowser(): void {
  if (!examplesAvailable()) {
    return;
  }
  document.dispatchEvent(new CustomEvent(EXAMPLE_BROWSER_SHOW_EVENT, { cancelable: true }));
}
//=============================================================================
export function closeExampleBrowser(): void {
  document.dispatchEvent(new CustomEvent(EXAMPLE_BROWSER_HIDE_EVENT, { cancelable: true }));
}
//=============================================================================
