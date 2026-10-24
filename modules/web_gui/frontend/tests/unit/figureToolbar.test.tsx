//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act, useState } from 'react';
import { createRoot } from 'react-dom/client';
import { expect, it, vi } from 'vitest';
import { FigureToolbar } from '../../src/panels/FigureToolbar';
import type { FigureTool } from '../../src/rpc/figureTypes';
//=============================================================================
it('shows the unified navigation bar, keeps modes radio-style and routes actions', () => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  const noop = () => undefined;
  const onAction = vi.fn();
  function Workspace({ saveAsLabel }: { saveAsLabel?: string }) {
    const [details, setDetails] = useState(false);
    const [tool, setTool] = useState<FigureTool>('inspect');
    return (
      <FigureToolbar
        title="Surface"
        is3d
        tool={tool}
        setTool={setTool}
        detailsVisible={details}
        toggleDetails={() => setDetails(!details)}
        inspectorOpen={false}
        toggleInspector={noop}
        maximized={false}
        onAction={onAction}
        exportFigure={noop}
        exporting={false}
        saveAs={noop}
        saveAsLabel={saveAsLabel}
        print={noop}
        refresh={noop}
        detach={noop}
        detachLabel="Undock"
      />
    );
  }
  const button = (name: string) =>
    [...host.querySelectorAll('button')].find(
      (element) => (element.getAttribute('aria-label') ?? element.textContent?.trim()) === name
    );
  const click = (name: string) => {
    const element = button(name);
    expect(element, name).toBeDefined();
    act(() => element!.click());
  };
  const openMore = () => {
    const more = button('More figure actions');
    expect(more).toBeDefined();
    if (more!.getAttribute('aria-expanded') !== 'true') act(() => more!.click());
  };
  try {
    act(() => root.render(<Workspace />));
    const nav = host.querySelector('[aria-label="Figure navigation"]');
    expect(nav).not.toBeNull();
    expect(nav?.textContent).toContain('Inspect');
    expect(nav?.textContent).toContain('Zoom');
    expect(nav?.textContent).toContain('Pan');
    expect(nav?.textContent).toContain('Rotate');
    expect(nav?.textContent).toContain('Fit');
    expect(nav?.textContent).toContain('Restore view');

    // Default tool is Inspect; modes are mutually exclusive.
    expect(button('Inspect')?.getAttribute('aria-pressed')).toBe('true');
    click('Pan');
    expect(button('Pan')?.getAttribute('aria-pressed')).toBe('true');
    expect(button('Inspect')?.getAttribute('aria-pressed')).toBe('false');
    expect(button('Pan')?.title).toContain('move the view');
    expect(button('Zoom')?.title).toContain('zoom');
    expect(button('Fit')?.title).toContain('keep the current orientation');

    click('Fit');
    expect(onAction).toHaveBeenLastCalledWith({ action: 'fit' });
    click('Restore view');
    expect(onAction).toHaveBeenLastCalledWith({ action: 'restore' });

    openMore();
    click('Zoom in');
    expect(onAction).toHaveBeenLastCalledWith({ action: 'zoom', factor: 1.25 });
    openMore();
    click('Zoom out');
    expect(onAction).toHaveBeenLastCalledWith({ action: 'zoom', factor: 0.8 });
    openMore();
    click('Use current view as reference');
    expect(onAction).toHaveBeenLastCalledWith({ action: 'reference' });
    openMore();
    click('Plot details bar');
    openMore();
    expect(button('Plot details bar')?.getAttribute('aria-pressed')).toBe('true');

    expect(host.textContent).toContain('Properties');
    expect(host.textContent).toContain('Export');
    expect(host.querySelector('.figure-export-primary')?.getAttribute('title')).toContain(
      'download'
    );
    openMore();
    expect(button('Save as…')?.title).toContain('local file');
    act(() => root.render(<Workspace saveAsLabel="Save to workspace…" />));
    openMore();
    expect(button('Save to workspace…')?.title).toContain('virtual filesystem');
  } finally {
    act(() => root.unmount());
    host.remove();
    vi.unstubAllGlobals();
  }
});
//=============================================================================
