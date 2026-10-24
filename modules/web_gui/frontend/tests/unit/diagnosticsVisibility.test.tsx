//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { createRoot, type Root } from 'react-dom/client';
import { Sidebar } from '../../src/components/Sidebar';
import { desktopActions } from '../../src/desktop/actions';
import { isWasmRuntime } from '../../src/desktop/platform';
//=============================================================================
// The Diagnostics panel reports the browser runtime and the WebAssembly bundle.
// The native engine (WebView desktop, `--web` server) only answers with a stub,
// so the panel must not be reachable there: no rail button, no View entry.
const wasmGlobal = globalThis as unknown as { __NELSON_WASM__?: boolean };
function setWasmBuild(enabled: boolean): void {
  if (enabled) wasmGlobal.__NELSON_WASM__ = true;
  else delete wasmGlobal.__NELSON_WASM__;
}
//=============================================================================
function renderSidebar(): { root: Root; container: HTMLDivElement } {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  act(() => root.render(<Sidebar />));
  return { root, container };
}
//=============================================================================
function railLabels(container: HTMLElement): string[] {
  return Array.from(container.querySelectorAll('button.sidebar__btn')).map((button) =>
    (button.getAttribute('aria-label') ?? '').replace(/ \(.*\)$/, '')
  );
}
//=============================================================================
describe('diagnostics panel visibility', () => {
  beforeEach(() => vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true));
  afterEach(() => {
    setWasmBuild(false);
    vi.unstubAllGlobals();
  });

  it('reports the native runtime unless the wasm build flag is set', () => {
    expect(isWasmRuntime()).toBe(false);
    setWasmBuild(true);
    expect(isWasmRuntime()).toBe(true);
  });

  it('hides the Diagnostics rail button and View action on the native engine', () => {
    const { root, container } = renderSidebar();
    expect(railLabels(container)).not.toContain('Diagnostics');
    expect(railLabels(container)).toContain('Profiler');
    expect(desktopActions().some((action) => action.id === 'view.diagnostics')).toBe(false);
    act(() => root.unmount());
    container.remove();
  });

  it('keeps the Diagnostics rail button and View action in the wasm build', () => {
    setWasmBuild(true);
    const { root, container } = renderSidebar();
    expect(railLabels(container)).toContain('Diagnostics');
    expect(desktopActions()).toContainEqual(
      expect.objectContaining({ id: 'view.diagnostics', title: 'Diagnostics', category: 'View' })
    );
    act(() => root.unmount());
    container.remove();
  });
});
//=============================================================================
