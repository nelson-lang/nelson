//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as FieldNames from '../FieldNames.js';
//=============================================================================
let cm;
beforeAll(async () => {
  cm = await import('./context-menu-manager.js');
});
//=============================================================================
afterEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
function makeBlockMenu(
  actions = ['mirrorHorizontal', 'mirrorVertical', 'rotateLeft', 'rotateRight', 'delete']
) {
  const menu = document.createElement('div');
  menu.className = 'context-menu';
  for (const action of actions) {
    const item = document.createElement('div');
    item.className = 'context-menu-item';
    item.setAttribute('data-action', action);
    menu.appendChild(item);
  }
  document.body.appendChild(menu);
  return menu;
}
//=============================================================================
function makeCanvasMenu(actions = ['resetView', 'selectAll']) {
  const menu = document.createElement('div');
  menu.className = 'context-menu';
  for (const action of actions) {
    const item = document.createElement('div');
    item.className = 'context-menu-item';
    item.setAttribute('data-action', action);
    menu.appendChild(item);
  }
  document.body.appendChild(menu);
  return menu;
}
//=============================================================================
function makeSvg() {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  document.body.appendChild(svg);
  return svg;
}
//=============================================================================
function makeState(selectedId: any = null, blocks = new Map()) {
  return {
    selectedId: selectedId,
    selectedIds: new Set(),
    selectedConnection: null,
    selectedConnections: new Set(),
    connections: [],
    routingDirty: false,
    dirtyBlocks: new Set(),
    fastRouting: false,
    blocks,
  };
}
//=============================================================================
import { vi } from 'vitest';
//=============================================================================
function makeRenderer() {
  return {
    current: {
      updateBlockTransform: vi.fn(),
      updateConnections: vi.fn(),
      deleteBlock: vi.fn(),
      setMultiSelection: vi.fn(),
      updateSelectionBox: vi.fn(),
    },
  };
}
//=============================================================================
describe('context-menu-manager - setters', () => {
  test('all setters can be called without error', () => {
    expect(() =>
      cm.setContextMenus(document.createElement('div'), document.createElement('div'))
    ).not.toThrow();
    expect(() => cm.setStateRef({})).not.toThrow();
    expect(() => cm.setRendererRef({})).not.toThrow();
    expect(() => cm.setFitToDiagram(() => {})).not.toThrow();
    expect(() => cm.setRenderInspector(() => {})).not.toThrow();
    expect(() => cm.setFocusPropertiesPanel(() => {})).not.toThrow();
    expect(() => cm.setSvg(document.createElement('div'))).not.toThrow();
  });

  test('shows block help context menu for a library item', () => {
    const menu = document.createElement('div');
    menu.id = 'libraryBlockContextMenu';
    menu.className = 'context-menu';
    document.body.appendChild(menu);
    cm.setContextMenus(null, null);

    const event = new MouseEvent('contextmenu', { clientX: 20, clientY: 30 });
    cm.showLibraryBlockContextMenu(event, 'gain');

    expect(menu.classList.contains('show')).toBe(true);
    expect(menu.style.left).toBe('20px');
    expect(menu.style.top).toBe('30px');
  });

  test('initContextMenu returns early when svg/state/rendererRef missing', () => {
    cm.setSvg(null);
    cm.setStateRef(null);
    cm.setRendererRef(null);
    expect(() => cm.initContextMenu()).not.toThrow();
  });
});
//=============================================================================
describe('context-menu-manager - canvas right-click', () => {
  test('right-click on svg shows canvas context menu', () => {
    const blockMenu = makeBlockMenu();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    cm.setContextMenus(blockMenu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(makeState());
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();
    svg.dispatchEvent(
      new MouseEvent('contextmenu', { bubbles: true, cancelable: true, clientX: 50, clientY: 60 })
    );
    expect(canvasMenu.classList.contains('show')).toBe(true);
  });

  test('document click outside menu hides all menus', () => {
    const blockMenu = makeBlockMenu();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    cm.setContextMenus(blockMenu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(makeState());
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();
    svg.dispatchEvent(new MouseEvent('contextmenu', { bubbles: true, cancelable: true }));
    expect(canvasMenu.classList.contains('show')).toBe(true);
    // click outside
    document.body.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    expect(canvasMenu.classList.contains('show')).toBe(false);
  });

  test('Escape key hides menus', () => {
    const blockMenu = makeBlockMenu();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    cm.setContextMenus(blockMenu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(makeState());
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();
    svg.dispatchEvent(new MouseEvent('contextmenu', { bubbles: true, cancelable: true }));
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
    expect(canvasMenu.classList.contains('show')).toBe(false);
  });
});
//=============================================================================
describe('context-menu-manager - block right-click', () => {
  test('right-click on block element shows block context menu', () => {
    const blockMenu = makeBlockMenu();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    cm.setContextMenus(blockMenu, canvasMenu);
    cm.setSvg(svg);
    const st = makeState();
    cm.setStateRef(st);
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();
    const blockEl = document.createElement('g');
    blockEl.setAttribute('data-block-id', 'b1');
    svg.appendChild(blockEl);
    blockEl.dispatchEvent(
      new MouseEvent('contextmenu', { bubbles: true, cancelable: true, clientX: 20, clientY: 30 })
    );
    expect(blockMenu.classList.contains('show')).toBe(true);
    expect(st.selectedId).toBe('b1');
  });
});
//=============================================================================
describe('context-menu-manager - block actions', () => {
  function setup() {
    const blockMenu = makeBlockMenu();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    const renderer = makeRenderer();
    const block = {
      id: 'b1',
      type: FieldNames.kGain,
      rotation: 0,
      mirrorHorizontal: false,
      mirrorVertical: false,
      width: 80,
      height: 40,
      ports: [{ x: 0, y: 20, wireX: 0, wireY: 20, el: null }],
    };
    const st = makeState('b1', new Map([['b1', block]]));
    cm.setContextMenus(blockMenu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(renderer);
    cm.initContextMenu();
    return { blockMenu, canvasMenu, renderer, block, st };
  }

  test('rotateLeft decreases rotation by 90', () => {
    const { blockMenu, renderer, block } = setup();
    block.rotation = 90;
    (blockMenu.querySelector('[data-action="rotateLeft"]') as HTMLElement)!.click();
    expect(block.rotation).toBe(0);
    expect(renderer.current.updateBlockTransform).toHaveBeenCalledWith(block);
  });

  test('rotateRight increases rotation by 90', () => {
    const { blockMenu, renderer, block } = setup();
    block.rotation = 0;
    (blockMenu.querySelector('[data-action="rotateRight"]') as HTMLElement)!.click();
    expect(block.rotation).toBe(90);
    expect(renderer.current.updateBlockTransform).toHaveBeenCalledWith(block);
  });

  test('mirrorHorizontal toggles mirror flag', () => {
    const { blockMenu, block } = setup();
    block.ports = [];
    (blockMenu.querySelector('[data-action="mirrorHorizontal"]') as HTMLElement)!.click();
    expect(block.mirrorHorizontal).toBe(true);
  });

  test('mirrorVertical toggles mirror flag', () => {
    const { blockMenu, block } = setup();
    block.ports = [];
    (blockMenu.querySelector('[data-action="mirrorVertical"]') as HTMLElement)!.click();
    expect(block.mirrorVertical).toBe(true);
  });

  test('delete calls deleteBlock', () => {
    const { blockMenu, renderer, st } = setup();
    (blockMenu.querySelector('[data-action="delete"]') as HTMLElement)!.click();
    expect(renderer.current.deleteBlock).toHaveBeenCalledWith('b1');
  });

  test('properties calls renderInspector and focuses properties panel', () => {
    const blockMenu = makeBlockMenu([
      'mirrorHorizontal',
      'mirrorVertical',
      'rotateLeft',
      'rotateRight',
      'delete',
      'properties',
    ]);
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    const renderer = makeRenderer();
    const block = {
      id: 'b1',
      type: FieldNames.kGain,
      rotation: 0,
      mirrorHorizontal: false,
      mirrorVertical: false,
      width: 80,
      height: 40,
      ports: [{ x: 0, y: 20, wireX: 0, wireY: 20, el: null }],
    };
    const st = makeState('b1', new Map([['b1', block]]));
    cm.setContextMenus(blockMenu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(renderer);
    const renderInspector = vi.fn();
    const focusProps = vi.fn();
    cm.setRenderInspector(renderInspector);
    cm.setFocusPropertiesPanel(focusProps);
    cm.initContextMenu();

    (blockMenu.querySelector('[data-action="properties"]') as HTMLElement)!.click();

    expect(renderInspector).toHaveBeenCalledWith(block);
    expect(focusProps).toHaveBeenCalled();
  });

  test('actions are no-ops when no selectedId', () => {
    const { blockMenu, renderer, st } = setup();
    st.selectedId = null;
    (blockMenu.querySelector('[data-action="rotateLeft"]') as HTMLElement)!.click();
    expect(renderer.current.updateBlockTransform).not.toHaveBeenCalled();
  });
});
//=============================================================================
describe('context-menu-manager - canvas actions', () => {
  function setup() {
    const blockMenu = makeBlockMenu();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    const renderer = makeRenderer();
    const st = makeState(
      null,
      new Map([
        ['b1', { id: 'b1' }],
        ['b2', { id: 'b2' }],
      ])
    );
    const fitFn = vi.fn();
    cm.setContextMenus(blockMenu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(renderer);
    cm.setFitToDiagram(fitFn);
    cm.initContextMenu();
    return { canvasMenu, renderer, st, fitFn };
  }

  test('resetView calls fitToDiagram', () => {
    const { canvasMenu, fitFn } = setup();
    (canvasMenu.querySelector('[data-action="resetView"]') as HTMLElement)!.click();
    expect(fitFn).toHaveBeenCalled();
  });

  test('selectAll calls setMultiSelection with all blocks', () => {
    const { canvasMenu, renderer, st } = setup();
    (canvasMenu.querySelector('[data-action="selectAll"]') as HTMLElement)!.click();
    expect(renderer.current.setMultiSelection).toHaveBeenCalled();
  });

  test('recalculateWires calls forceFullRoute when available', () => {
    const blockMenu = makeBlockMenu();
    const canvasMenu = makeCanvasMenu(['resetView', 'recalculateWires', 'selectAll']);
    const svg = makeSvg();
    const renderer = makeRenderer();
    renderer.current.forceFullRoute = vi.fn(() => Promise.resolve());
    const st = makeState(
      null,
      new Map([
        ['b1', { id: 'b1' }],
        ['b2', { id: 'b2' }],
      ])
    );
    const fitFn = vi.fn();
    cm.setContextMenus(blockMenu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(renderer);
    cm.setFitToDiagram(fitFn);
    cm.initContextMenu();

    (canvasMenu.querySelector('[data-action="recalculateWires"]') as HTMLElement)!.click();
    expect(renderer.current.forceFullRoute).toHaveBeenCalledWith(2000);
  });

  test('recalculateWires falls back to updateConnections when forceFullRoute missing', () => {
    const { canvasMenu, renderer } = setup();
    // ensure renderer does not have forceFullRoute
    delete (renderer.current as any).forceFullRoute;
    (canvasMenu.querySelector('[data-action="recalculateWires"]') as HTMLElement) ||
      (() => {
        const item = document.createElement('div');
        item.className = 'context-menu-item';
        item.setAttribute('data-action', 'recalculateWires');
        canvasMenu.appendChild(item);
        return item;
      })();

    (canvasMenu.querySelector('[data-action="recalculateWires"]') as HTMLElement)!.click();
    expect(renderer.current.updateConnections).toHaveBeenCalledWith(true);
  });
});
// The "Create Subsystem from Selection" item (and its flanking dividers) must
// be hidden when fewer than 2 blocks are in state.selectedIds, and visible
// when 2 or more are selected.
/** Build a block context menu that mirrors the real DOM structure produced by
 *  mountContextMenus(): divider / createSubsystem item / divider. */
function makeBlockMenuWithSubsystem() {
  const menu = document.createElement('div');
  menu.id = 'blockContextMenu';
  menu.className = 'context-menu';
  for (const action of ['copy', 'cut']) {
    const item = document.createElement('div');
    item.className = 'context-menu-item';
    item.setAttribute('data-action', action);
    menu.appendChild(item);
  }
  const divBefore = document.createElement('div');
  divBefore.className = 'context-menu-divider';
  menu.appendChild(divBefore);
  const subsystemItem = document.createElement('div');
  subsystemItem.className = 'context-menu-item';
  subsystemItem.setAttribute('data-action', 'createSubsystem');
  menu.appendChild(subsystemItem);
  const divAfter = document.createElement('div');
  divAfter.className = 'context-menu-divider';
  menu.appendChild(divAfter);
  const deleteItem = document.createElement('div');
  deleteItem.className = 'context-menu-item';
  deleteItem.setAttribute('data-action', 'delete');
  menu.appendChild(deleteItem);
  document.body.appendChild(menu);
  return { menu, subsystemItem, divBefore, divAfter };
}
//=============================================================================
/** Right-click a block element to trigger handleBlockContextMenu. */
function rightClickBlock(svg: SVGSVGElement, blockId: string) {
  const blockEl = document.createElement('g');
  blockEl.setAttribute('data-block-id', blockId);
  svg.appendChild(blockEl);
  blockEl.dispatchEvent(
    new MouseEvent('contextmenu', { bubbles: true, cancelable: true, clientX: 10, clientY: 10 })
  );
}
//=============================================================================
describe('context-menu-manager - createSubsystem item visibility', () => {
  test('item is hidden when only 1 block is selected', () => {
    const { menu, subsystemItem, divBefore, divAfter } = makeBlockMenuWithSubsystem();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    const st = makeState('b1', new Map([['b1', { id: 'b1' }]]));
    st.selectedIds = new Set(['b1']);
    cm.setContextMenus(menu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();

    rightClickBlock(svg, 'b1');

    expect(subsystemItem.classList.contains('context-menu-item--hidden')).toBe(true);
    expect(divBefore.classList.contains('context-menu-divider--hidden')).toBe(true);
    expect(divAfter.classList.contains('context-menu-divider--hidden')).toBe(true);
  });

  test('item is hidden when no blocks are selected', () => {
    const { menu, subsystemItem } = makeBlockMenuWithSubsystem();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    const st = makeState(null, new Map([['b1', { id: 'b1' }]]));
    st.selectedIds = new Set();
    cm.setContextMenus(menu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();

    rightClickBlock(svg, 'b1');

    expect(subsystemItem.classList.contains('context-menu-item--hidden')).toBe(true);
  });

  test('item is visible when 2 blocks are selected', () => {
    const { menu, subsystemItem, divBefore, divAfter } = makeBlockMenuWithSubsystem();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    const st = makeState(
      'b1',
      new Map([
        ['b1', { id: 'b1' }],
        ['b2', { id: 'b2' }],
      ])
    );
    st.selectedIds = new Set(['b1', 'b2']);
    cm.setContextMenus(menu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();

    rightClickBlock(svg, 'b1');

    expect(subsystemItem.classList.contains('context-menu-item--hidden')).toBe(false);
    expect(divBefore.classList.contains('context-menu-divider--hidden')).toBe(false);
    expect(divAfter.classList.contains('context-menu-divider--hidden')).toBe(false);
  });

  test('item is visible when 3 or more blocks are selected', () => {
    const { menu, subsystemItem } = makeBlockMenuWithSubsystem();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    const st = makeState(
      'b1',
      new Map([
        ['b1', { id: 'b1' }],
        ['b2', { id: 'b2' }],
        ['b3', { id: 'b3' }],
      ])
    );
    st.selectedIds = new Set(['b1', 'b2', 'b3']);
    cm.setContextMenus(menu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();

    rightClickBlock(svg, 'b1');

    expect(subsystemItem.classList.contains('context-menu-item--hidden')).toBe(false);
  });

  test('visibility updates dynamically between successive right-clicks', () => {
    const { menu, subsystemItem } = makeBlockMenuWithSubsystem();
    const canvasMenu = makeCanvasMenu();
    const svg = makeSvg();
    const st = makeState(
      'b1',
      new Map([
        ['b1', { id: 'b1' }],
        ['b2', { id: 'b2' }],
      ])
    );
    st.selectedIds = new Set(['b1']); // start with 1 selected
    cm.setContextMenus(menu, canvasMenu);
    cm.setSvg(svg);
    cm.setStateRef(st);
    cm.setRendererRef(makeRenderer());
    cm.initContextMenu();

    rightClickBlock(svg, 'b1');
    expect(subsystemItem.classList.contains('context-menu-item--hidden')).toBe(true);

    st.selectedIds = new Set(['b1', 'b2']);
    const blockEl2 = document.createElement('g');
    blockEl2.setAttribute('data-block-id', 'b2');
    svg.appendChild(blockEl2);
    blockEl2.dispatchEvent(
      new MouseEvent('contextmenu', { bubbles: true, cancelable: true, clientX: 10, clientY: 10 })
    );
    expect(subsystemItem.classList.contains('context-menu-item--hidden')).toBe(false);
  });
});
//=============================================================================
