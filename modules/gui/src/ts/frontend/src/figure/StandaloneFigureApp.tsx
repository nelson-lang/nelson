//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The chrome of a standalone figure window: just the figure panel, the prompt
// host and the audio-unlock host, themed like the desktop. It is deliberately
// free of the desktop shell (menus, dock, console, editor) so the slim figure
// entry bundle pulls in none of those. The full desktop reuses it for the same
// standalone render, so the two stay in step.
import { useEffect, useState } from 'react';
import { FigurePanel } from '../panels/FigurePanel';
import { PromptHost } from '../components/PromptHost';
import { AudioUnlockHost } from '../components/AudioUnlockHost';
import { rpc } from '../rpc/client';
import {
  getThemePref,
  resolveScheme,
  setActiveScheme,
  THEME_CHANGE_EVENT,
  type Scheme,
} from '../desktop/theme';
//=============================================================================
export function StandaloneFigureApp({
  handle,
  engineIdle,
}: {
  handle: number;
  // The interpreter was at its prompt when the window opened.
  engineIdle?: boolean;
}): JSX.Element {
  const [scheme, setScheme] = useState<Scheme>(() => resolveScheme(getThemePref()));
  // The imperative panels read the active scheme from the store, so keep it in
  // sync with the local state the shell class uses.
  useEffect(() => {
    setActiveScheme(scheme);
  }, [scheme]);
  // Follow the OS scheme while the preference is "system", and any explicit
  // theme change pushed by the host.
  useEffect(() => {
    const recompute = () => setScheme(resolveScheme(getThemePref()));
    const mm = window.matchMedia('(prefers-color-scheme: dark)');
    mm.addEventListener('change', recompute);
    window.addEventListener(THEME_CHANGE_EVENT, recompute);
    return () => {
      mm.removeEventListener('change', recompute);
      window.removeEventListener(THEME_CHANGE_EVENT, recompute);
    };
  }, []);

  // The window opens after the figure (and any Name) exists, so the figure.title
  // push already went by: fetch the current title once instead of staying on the
  // "Figure N" fallback.
  const [title, setTitle] = useState<string | null>(null);
  useEffect(() => {
    let alive = true;
    rpc<{ figures?: { handle: number; title?: string }[] }>('figure.list')
      .then((r) => {
        if (!alive) return;
        const entry = (r.figures ?? []).find((f) => f.handle === handle);
        if (entry?.title) setTitle(entry.title);
      })
      .catch(() => undefined);
    return () => {
      alive = false;
    };
  }, [handle]);

  return (
    <div className={`nelson-shell nelson-shell--${scheme} nelson-shell--standalone`}>
      <FigurePanel
        handle={handle}
        title={title ?? `Figure ${handle + 1}`}
        standalone
        engineIdleAtOpen={engineIdle}
      />
      <PromptHost />
      <AudioUnlockHost />
    </div>
  );
}
//=============================================================================
