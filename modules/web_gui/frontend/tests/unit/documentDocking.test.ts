//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import type { DockviewApi } from 'dockview';
import { openEditor, openNotebook, setDockApi } from '../../src/desktop/desktop';
//=============================================================================
type AddedPanel = { id: string; position: unknown };
//=============================================================================
// Installs a minimal Dockview API that records addPanel positions, with the
// Files sidebar as the active group so a missing position would dock there.
function installFakeDock(initialPanelIds: string[]): AddedPanel[] {
  const added: AddedPanel[] = [];
  const panels: Array<{ id: string }> = initialPanelIds.map((id) => ({ id }));
  const noop = () => ({ dispose() {} });
  const fakePanel = (id: string) => ({
    id,
    group: { api: { setActive: vi.fn() } },
    api: { setActive: vi.fn() },
    focus: vi.fn(),
  });
  setDockApi({
    get panels() {
      return panels;
    },
    activePanel: { id: 'files' },
    getPanel: (id: string) => panels.find((p) => p.id === id),
    addPanel: (opts: { id: string; position?: unknown }) => {
      added.push({ id: opts.id, position: opts.position ?? undefined });
      panels.push({ id: opts.id });
      return fakePanel(opts.id);
    },
    onDidActivePanelChange: noop,
    onDidAddPanel: noop,
    onDidRemovePanel: noop,
    onDidMovePanel: noop,
  } as unknown as DockviewApi);
  return added;
}
//=============================================================================
describe('document docking', () => {
  it('opens an editor in the console group, not the active Files sidebar', () => {
    const added = installFakeDock(['console', 'files']);
    openEditor('/workspace/demo.m');
    expect(added).toHaveLength(1);
    expect(added[0].id).toMatch(/^editor:/);
    expect(added[0].position).toEqual({ referencePanel: 'console', direction: 'within' });
  });

  it('stacks a second editor as a tab beside the first one', () => {
    const added = installFakeDock(['console', 'files']);
    openEditor('/workspace/first.m');
    openEditor('/workspace/second.m');
    expect(added).toHaveLength(2);
    expect(added[1].position).toEqual({ referencePanel: added[0].id, direction: 'within' });
  });

  it('opens a notebook in the console group as well', () => {
    const added = installFakeDock(['console', 'files']);
    openNotebook('/workspace/doc.nlx');
    expect(added).toHaveLength(1);
    expect(added[0].id).toMatch(/^notebook:/);
    expect(added[0].position).toEqual({ referencePanel: 'console', direction: 'within' });
  });

  it('falls back to no reference position when no console exists', () => {
    const added = installFakeDock(['files']);
    openEditor('/workspace/demo.m');
    expect(added[0].position).toBeUndefined();
  });
});
