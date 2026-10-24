//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import { rpc } from '../rpc/client';
//=============================================================================
// About dialog, opened from the Help menu by a window event, like Preferences.
export const OPEN_ABOUT_EVENT = 'nelson-webview.open-about';
export function AboutHost(): JSX.Element | null {
  const [open, setOpen] = useState(false);
  const [version, setVersion] = useState('');

  useEffect(() => {
    const onOpen = () => {
      setOpen(true);
      rpc<{ version?: string }>('ping')
        .then((r) => setVersion(r.version ?? ''))
        .catch(() => undefined);
    };
    window.addEventListener(OPEN_ABOUT_EVENT, onOpen);
    return () => window.removeEventListener(OPEN_ABOUT_EVENT, onOpen);
  }, []);

  if (!open) return null;

  return (
    <div className="dialog-overlay" role="presentation" onMouseDown={() => setOpen(false)}>
      <div
        className="dialog-card about-card"
        role="dialog"
        aria-modal="true"
        aria-label="About Nelson"
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={(e) => {
          if (e.key === 'Escape') setOpen(false);
        }}
      >
        <div className="about-card__title">Nelson</div>
        {version ? <div className="about-card__version">Version {version}</div> : null}
        <div className="about-card__badge">Web desktop preview</div>
        <p className="about-card__desc">
          Nelson is an open-source, matrix-oriented computing environment for numerical analysis,
          data visualization and engineering.
        </p>
        <div className="about-card__links">
          <a
            href="https://nelson-lang.github.io/nelson-website/"
            target="_blank"
            rel="noopener noreferrer"
          >
            Website
          </a>
          <a href="https://github.com/nelson-lang/nelson" target="_blank" rel="noopener noreferrer">
            Source
          </a>
          <a
            href="https://nelson-lang.github.io/nelson-website/en_US/"
            target="_blank"
            rel="noopener noreferrer"
          >
            Documentation
          </a>
        </div>
        <div className="dialog-card__buttons">
          <button className="dialog-btn dialog-btn--default" onClick={() => setOpen(false)}>
            OK
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
