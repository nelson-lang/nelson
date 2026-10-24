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
const inspectorRoots = new WeakMap<HTMLElement, Root>();
export function unmountInspectorReact(inspectorBody: HTMLElement | null): void {
  if (!inspectorBody) return;
  const root = inspectorRoots.get(inspectorBody);
  if (!root) return;
  root.unmount();
  inspectorRoots.delete(inspectorBody);
}
//=============================================================================
export function renderInspectorReact(
  inspectorBody: HTMLElement | null,
  element: JSX.Element
): void {
  if (!inspectorBody) return;
  unmountInspectorReact(inspectorBody);
  inspectorBody.innerHTML = '';
  const host = document.createElement('div');
  host.dataset.reactRoot = 'inspector-panel';
  host.style.display = 'contents';
  inspectorBody.appendChild(host);
  const root = createRoot(host);
  inspectorRoots.set(inspectorBody, root);
  flushSync(() => {
    root.render(element);
  });
}
//=============================================================================
