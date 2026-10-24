//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Canvas context menu: "Auto-route Wire" appears only when right-clicking a
// pinned wire, and releases its pins on activation.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import {
  setContextMenus,
  setStateRef,
  setRendererRef,
  setSvg,
  initContextMenu,
} from './context-menu-manager.js';
//=============================================================================
function makeMenus() {
  const canvasMenu = document.createElement('div');
  canvasMenu.id = 'canvasContextMenu';
  canvasMenu.className = 'context-menu';
  canvasMenu.innerHTML = `
    <div class="context-menu-item" data-action="resetView">Reset View</div>
    <div class="context-menu-item" data-action="autoRouteWire">Auto-route Wire</div>
  `;
  document.body.appendChild(canvasMenu);
  const blockMenu = document.createElement('div');
  blockMenu.id = 'blockContextMenu';
  document.body.appendChild(blockMenu);
  return { canvasMenu, blockMenu };
}
//=============================================================================
function rightClick(svg: SVGSVGElement, x = 50, y = 50) {
  const ev = new MouseEvent('contextmenu', { bubbles: true, clientX: x, clientY: y });
  svg.dispatchEvent(ev);
}
//=============================================================================
describe('canvas context menu: Auto-route Wire', () => {
  let svg: SVGSVGElement;
  let canvasMenu: HTMLElement;
  let released: any[];
  let pinnedConn: any;
  let nearest: any;

  beforeEach(() => {
    document.body.innerHTML = '';
    svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg') as SVGSVGElement;
    document.body.appendChild(svg);
    const menus = makeMenus();
    canvasMenu = menus.canvasMenu;

    released = [];
    pinnedConn = {
      from: 'a',
      to: 'b',
      points: [
        { x: 0, y: 0 },
        { x: 50, y: 0, pin: true },
        { x: 50, y: 40, pin: true },
        { x: 100, y: 40 },
      ],
    };
    nearest = null;

    setContextMenus(menus.blockMenu, menus.canvasMenu);
    setStateRef({ blocks: new Map(), connections: [pinnedConn], selectedIds: new Set() } as any);
    setRendererRef({
      current: {
        clientToSvg: (x: number, y: number) => ({ x, y }),
        findNearestConnection: () => nearest,
        releaseWirePins: (conn: any) => {
          released.push(conn);
          return true;
        },
        updateConnections: () => {},
      },
    } as any);
    setSvg(svg);
    initContextMenu();
  });

  it('hides the entry when no pinned wire is under the pointer', () => {
    nearest = null;
    rightClick(svg);
    const item = canvasMenu.querySelector('[data-action="autoRouteWire"]') as HTMLElement;
    expect(item.classList.contains('context-menu-item--hidden')).toBe(true);
  });

  it('shows the entry over a pinned wire and releases pins on click', () => {
    nearest = pinnedConn;
    rightClick(svg);
    const item = canvasMenu.querySelector('[data-action="autoRouteWire"]') as HTMLElement;
    expect(item.classList.contains('context-menu-item--hidden')).toBe(false);

    item.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    expect(released).toEqual([pinnedConn]);
  });

  it('hides the entry over a pin-free wire', () => {
    nearest = {
      from: 'a',
      to: 'b',
      points: [
        { x: 0, y: 0 },
        { x: 100, y: 0 },
      ],
    };
    rightClick(svg);
    const item = canvasMenu.querySelector('[data-action="autoRouteWire"]') as HTMLElement;
    expect(item.classList.contains('context-menu-item--hidden')).toBe(true);
  });
});
//=============================================================================
