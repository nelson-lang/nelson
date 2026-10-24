//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const mockRenderFn = vi.fn();
vi.doMock('./library-loader.js', () => ({
  buildAllBlockTemplates: vi.fn().mockReturnValue({
    constant: {
      width: 60,
      height: 40,
      defaultParams: {},
      render: mockRenderFn,
    },
  }),
  getMissingBlockTypes: vi.fn().mockReturnValue([]),
  getDiscoveredManifests: vi.fn().mockReturnValue(new Map()),
  getLoadedLibraries: vi.fn().mockReturnValue([]),
  loadAllDiscovered: (vi.fn() as any).mockResolvedValue(undefined),
  loadLibrary: (vi.fn() as any).mockResolvedValue(undefined),
  unloadLibrary: vi.fn(),
  isLibraryLoaded: vi.fn().mockReturnValue(false),
  addSearchPath: vi.fn(),
  getSearchPaths: vi.fn().mockReturnValue([]),
  initLibraries: (vi.fn() as any).mockResolvedValue(undefined),
}));
//=============================================================================
let handlers;
let stateMod;
beforeAll(async () => {
  handlers = await import('./block-library-handlers.js');
  stateMod = await import('./state.js');
});
//=============================================================================
beforeEach(() => {
  delete window.__renderBlockLibraryRetryCount;
  document.body.innerHTML = '';
  stateMod.state.loadedSubsystems = new Map();
  stateMod.state.spawnIndex = 0;
  stateMod.state.routingDirty = false;
});
//=============================================================================
afterEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
describe('block-library-handlers - getters/setters', () => {
  test('spreads successive library insertions across the workspace', () => {
    expect(handlers.spawnOffsetForIndex(0)).toEqual({ x: -180, y: 0 });
    expect(handlers.spawnOffsetForIndex(1)).toEqual({ x: -60, y: 0 });
    expect(handlers.spawnOffsetForIndex(2)).toEqual({ x: 100, y: 0 });
    expect(handlers.spawnOffsetForIndex(9)).toEqual({ x: -180, y: 0 });
  });

  test('setters and getters work correctly', () => {
    const el = document.createElement('div');
    handlers.setBlockLibraryGroups(el);
    expect(handlers.getBlockLibraryGroups()).toBe(el);

    const lib = [{ id: 'g1', title: 'G1', blocks: [] }];
    handlers.setBlockLibrary(lib);
    expect(handlers.getBlockLibrary()).toBe(lib);

    handlers.setGridSize(40);
    expect(handlers.getGridSize()).toBe(40);

    expect(() => handlers.setRendererRef({ current: {} })).not.toThrow();
    expect(() => handlers.setStatusEl(document.createElement('div'))).not.toThrow();
  });
});
//=============================================================================
describe('block-library-handlers - renderBlockLibrary', () => {
  test('schedules retries when no groups set', () => {
    handlers.setBlockLibraryGroups(null);
    handlers.renderBlockLibrary();
    expect(window.__renderBlockLibraryRetryCount).toBe(1);
  });

  test('stops retrying after 5 attempts', () => {
    handlers.setBlockLibraryGroups(null);
    window.__renderBlockLibraryRetryCount = 5;
    handlers.renderBlockLibrary();
    // should not increment beyond 5 (stops at 6)
    expect(window.__renderBlockLibraryRetryCount).toBe(6);
  });

  test('creates groups and tool buttons', () => {
    const container = document.createElement('div');
    handlers.setBlockLibraryGroups(container);
    handlers.setBlockLibrary([
      { id: 'g1', title: 'Group 1', blocks: [{ type: 'constant', label: 'Const' }] },
    ]);

    handlers.renderBlockLibrary();

    const details = container.querySelectorAll('details');
    expect(details.length).toBe(1);

    const button = container.querySelector('.tool') as HTMLElement | null;
    expect(button).not.toBeNull();
    expect(button!.textContent).toContain('Const');
  });

  test('restores persisted per-group open state and orders open groups first', () => {
    const container = document.createElement('div');
    handlers.setBlockLibraryGroups(container);
    localStorage.setItem('nf-library-group-g1-open', '0');
    localStorage.setItem('nf-library-group-g2-open', '1');
    localStorage.setItem('nf-library-group-g3-open', '0');
    handlers.setBlockLibrary([
      { id: 'g1', title: 'G1', blocks: [{ type: 'constant', label: 'C1' }] },
      { id: 'g2', title: 'G2', blocks: [{ type: 'constant', label: 'C2' }] },
      { id: 'g3', title: 'G3', blocks: [{ type: 'constant', label: 'C3' }] },
    ]);

    handlers.renderBlockLibrary();

    const details = Array.from(
      container.querySelectorAll('details.tool-group')
    ) as HTMLDetailsElement[];
    expect(details.length).toBe(3);
    const titleOf = (d: HTMLDetailsElement) => d.querySelector('summary span')?.textContent;
    expect(titleOf(details[0])).toBe('G2');
    // Persisted state respected; the first library group is NOT force-opened.
    const byTitle = (title: string) => details.find((d) => titleOf(d) === title)!;
    expect(byTitle('G2').open).toBe(true);
    expect(byTitle('G1').open).toBe(false);
    expect(byTitle('G3').open).toBe(false);
    localStorage.clear();
  });

  test('keeps every group collapsed when the user collapsed them all last session', () => {
    const container = document.createElement('div');
    handlers.setBlockLibraryGroups(container);
    // Previous session: the user collapsed every category (including "Source").
    localStorage.setItem('nf-library-group-g1-open', '0');
    localStorage.setItem('nf-library-group-g2-open', '0');
    handlers.setBlockLibrary([
      { id: 'g1', title: 'G1', blocks: [{ type: 'constant', label: 'C1' }] },
      { id: 'g2', title: 'G2', blocks: [{ type: 'constant', label: 'C2' }] },
    ]);

    handlers.renderBlockLibrary();

    const details = Array.from(
      container.querySelectorAll('details.tool-group')
    ) as HTMLDetailsElement[];
    expect(details.length).toBe(2);
    expect(details.some((d) => d.open)).toBe(false);
    localStorage.clear();
  });

  test('renders multiple block types with fallback icons', () => {
    const container = document.createElement('div');
    handlers.setBlockLibraryGroups(container);
    const blockTypes = [
      'step',
      'ramp',
      'impulse',
      'sine',
      'chirp',
      'noise',
      FieldNames.kGain,
      'sum',
      'integrator',
      'tf',
      FieldNames.kDelay,
      'stateSpace',
      'scope',
      FieldNames.kLabelSource,
      FieldNames.kSubsystem,
    ];
    handlers.setBlockLibrary([
      {
        id: 'g1',
        title: 'All Types',
        blocks: blockTypes.map((t) => ({ type: t, label: t })),
      },
    ]);
    handlers.renderBlockLibrary();
    const buttons = container.querySelectorAll('.tool');
    expect(buttons.length).toBe(blockTypes.length);
  });

  test('renders blocks with window.blockTemplates when available', () => {
    const container = document.createElement('div');
    handlers.setBlockLibraryGroups(container);
    handlers.setBlockLibrary([
      {
        id: 'g1',
        title: 'G1',
        blocks: [{ type: 'constant', label: 'Const' }],
      },
    ]);
    handlers.renderBlockLibrary();
    // Without an `icon` in the manifest only the label is shown; template.render is no fallback.
    const button = container.querySelector('.tool') as HTMLElement | null;
    expect(button).not.toBeNull();
    expect(button!.textContent).toContain('Const');
    expect(mockRenderFn).not.toHaveBeenCalled();
  });

  test('handles template render error gracefully', () => {
    const container = document.createElement('div');
    handlers.setBlockLibraryGroups(container);
    // Use a block type not in the mock template - falls back to switch-case icons
    handlers.setBlockLibrary([
      {
        id: 'g1',
        title: 'G1',
        blocks: [{ type: 'unknownType', label: 'Unknown' }],
      },
    ]);
    expect(() => handlers.renderBlockLibrary()).not.toThrow();
  });

  test('includes subsystem blocks when loadedSubsystems present', () => {
    const container = document.createElement('div');
    handlers.setBlockLibraryGroups(container);
    handlers.setBlockLibrary([]);
    stateMod.state.loadedSubsystems = new Map([
      ['key1', { name: 'MySub', externalInputs: [], externalOutputs: [] }],
    ]);
    handlers.renderBlockLibrary();
    const buttons = container.querySelectorAll('.tool');
    expect(buttons.length).toBeGreaterThan(0);
    const subsysBtn = Array.from(buttons).find((b) => b.textContent.includes('MySub'));
    expect(subsysBtn).toBeDefined();
  });

  test('clicking tool creates block and updates status', () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    handlers.setBlockLibraryGroups(container);
    handlers.setBlockLibrary([
      { id: 'g1', title: 'Group 1', blocks: [{ type: 'constant', label: 'Const' }] },
    ]);

    const mockBlock = { id: 'new1' };
    const mockRenderer = {
      current: {
        createBlock: vi.fn().mockReturnValue(mockBlock),
        updateConnections: vi.fn(),
      },
    };
    handlers.setRendererRef(mockRenderer);
    const statusEl = document.createElement('div');
    handlers.setStatusEl(statusEl);

    handlers.renderBlockLibrary();
    const button = container.querySelector('.tool') as HTMLElement | null;
    button!.dispatchEvent(new MouseEvent('click', { bubbles: true }));

    expect(statusEl.textContent).toBe('Added constant');
    expect(mockRenderer.current.createBlock).toHaveBeenCalled();
  });

  test('clicking tool sets error status when createBlock throws', () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    handlers.setBlockLibraryGroups(container);
    handlers.setBlockLibrary([
      { id: 'g1', title: 'Group 1', blocks: [{ type: 'constant', label: 'Const' }] },
    ]);

    const mockRenderer = {
      current: {
        createBlock: vi.fn().mockImplementation(() => {
          throw new Error('oops');
        }),
        updateConnections: vi.fn(),
      },
    };
    handlers.setRendererRef(mockRenderer);
    const statusEl = document.createElement('div');
    handlers.setStatusEl(statusEl);

    handlers.renderBlockLibrary();
    const button = container.querySelector('.tool') as HTMLElement | null;
    button!.dispatchEvent(new MouseEvent('click', { bubbles: true }));

    expect(statusEl.textContent).toContain('Error adding');
  });

  test('drag start sets data transfer', () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    handlers.setBlockLibraryGroups(container);
    handlers.setBlockLibrary([
      { id: 'g1', title: 'Group 1', blocks: [{ type: FieldNames.kGain, label: 'Gain' }] },
    ]);
    handlers.renderBlockLibrary();

    const button = container.querySelector('.tool') as HTMLElement | null;
    const mockTransfer = { effectAllowed: '', setData: vi.fn(), setDragImage: vi.fn() };
    const dragEvt = new Event('dragstart', { bubbles: true });
    (dragEvt as any).dataTransfer = mockTransfer;
    button!.dispatchEvent(dragEvt);

    expect(mockTransfer.setData).toHaveBeenCalledWith(
      'text/plain',
      expect.stringContaining(FieldNames.kGain)
    );
  });
});
//=============================================================================
