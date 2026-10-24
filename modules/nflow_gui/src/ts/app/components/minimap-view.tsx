//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
//=============================================================================
const roots = new WeakMap<HTMLElement, Root>();
function MinimapView(): JSX.Element {
  return (
    <svg preserveAspectRatio="xMidYMid meet">
      <use href="#wireLayer" />
      <use href="#blockLayer" />
      <rect id="minimapViewport" fill="none" />
    </svg>
  );
}
//=============================================================================
export function renderMinimapView(container: HTMLElement): void {
  let root = roots.get(container);
  if (!root) {
    if (container.childNodes.length) container.replaceChildren();
    root = createRoot(container);
    roots.set(container, root);
  }
  flushSync(() => {
    root?.render(<MinimapView />);
  });
}
//=============================================================================
