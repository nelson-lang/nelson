//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Mounts the interactive figure panel for a live notebook output. It reuses the
// same panel the desktop and the browser use, so a gesture goes through the
// shared RPC client to the kernel and the fresh frame comes back on the push
// bus. The panel is mounted inline, with no desktop toolbar actions that only a
// window host can answer.
//=============================================================================
import { createElement } from 'react';
import ReactDOM from 'react-dom/client';
import { FigurePanel } from '../panels/FigurePanel';
// The panel's own styles live under the desktop shell class, which this
// standalone bundle does not otherwise load; pull them in so a mounted panel is
// sized and styled like the desktop and web notebook figures. shell.css carries
// the toolbar icon masks and button sizing, panels.css the figure layout.
import '../shell.css';
import '../panels.css';
import './notebookFigure.css';
//=============================================================================
export interface LiveFigureHandle {
  destroy(): void;
}
//=============================================================================
// Match the host color scheme so the panel chrome reads correctly in a light or
// dark notebook. The figure itself carries its own engine colors.
function shellSchemeClass(): string {
  try {
    return window.matchMedia('(prefers-color-scheme: light)').matches
      ? 'nelson-shell--light'
      : '';
  } catch {
    return '';
  }
}
//=============================================================================
export function mountLiveFigure(
  container: HTMLElement,
  handle: number,
  title: string
): LiveFigureHandle {
  const host = document.createElement('div');
  // The figure panel styles are scoped to a .nelson-shell ancestor; the mount
  // carries it so the stylesheet applies and the panel fills the output.
  host.className = `nelson-shell ${shellSchemeClass()} nls-figure-wrap nls-figure-live`.trim();
  container.appendChild(host);
  const root = ReactDOM.createRoot(host);
  root.render(createElement(FigurePanel, { handle, title, inline: true }));
  return {
    destroy() {
      root.unmount();
      if (host.parentNode === container) container.removeChild(host);
    },
  };
}
//=============================================================================
