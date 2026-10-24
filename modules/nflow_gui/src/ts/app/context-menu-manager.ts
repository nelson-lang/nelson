//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { AppState } from './state.js';
import { applyAlignment } from '../utils/block-alignment.js';
import { autoArrangeDiagram } from './auto-arrange.js';
import { schematicArrangeDiagram, diagramHasPhysicalPins } from './schematic-arrange.js';
import { openBlockHelp } from './block-help.js';
//=============================================================================
let blockContextMenu: HTMLElement | null = null;
let canvasContextMenu: HTMLElement | null = null;
let libraryBlockContextMenu: HTMLElement | null = null;
let libraryBlockType: string | null = null;
let svg: SVGSVGElement | null = null;
let stateRef: AppState | null = null;
let rendererRef: {
  current?: {
    updateBlockTransform?: (block: any) => void;
    updateConnections?: (fast?: boolean) => void;
    deleteBlock?: (id: number) => void;
    setMultiSelection?: (ids: Set<number>, conns: Set<number>) => void;
    updateSelectionBox?: () => void;
    forceFullRoute?: (timeMs?: number) => Promise<any> | void;
  } | null;
} | null = null;
//=============================================================================
let fitToDiagram: (() => void) | null = null;
let _renderInspector: ((b: any) => void) | null = null;
let _focusPropertiesPanel: (() => void) | null = null;
let _onGenerateBlockCode: ((block: any) => void) | null = null;
// Reference potentially-unused module locals to satisfy `noUnusedLocals` during
// strict CI builds (these are set by callers via the exported setters).
void _renderInspector;
void _focusPropertiesPanel;
export function setContextMenus(blockMenu: HTMLElement | null, canvasMenu: HTMLElement | null) {
  blockContextMenu = blockMenu;
  canvasContextMenu = canvasMenu;
  libraryBlockContextMenu = document.getElementById('libraryBlockContextMenu');
}
//=============================================================================
export function setStateRef(ref: AppState | null) {
  stateRef = ref;
}
//=============================================================================
export function setRendererRef(ref: typeof rendererRef) {
  rendererRef = ref;
}
//=============================================================================
export function setFitToDiagram(fn: (() => void) | null) {
  fitToDiagram = fn;
}
//=============================================================================
export function setRenderInspector(fn: ((b: any) => void) | null) {
  _renderInspector = fn;
}
//=============================================================================
export function setFocusPropertiesPanel(fn: (() => void) | null) {
  _focusPropertiesPanel = fn;
}
//=============================================================================
export function setOnGenerateBlockCode(fn: ((block: any) => void) | null) {
  _onGenerateBlockCode = fn;
}
//=============================================================================
export function setSvg(el: SVGSVGElement | null) {
  svg = el;
}
//=============================================================================
function hideAllContextMenus() {
  if (blockContextMenu) blockContextMenu.classList.remove('show');
  if (canvasContextMenu) canvasContextMenu.classList.remove('show');
  if (libraryBlockContextMenu) libraryBlockContextMenu.classList.remove('show');
}
//=============================================================================
export function showLibraryBlockContextMenu(event: MouseEvent, blockType: string): void {
  event.preventDefault();
  event.stopPropagation();
  libraryBlockType = blockType.trim() || null;
  if (libraryBlockContextMenu && libraryBlockType) {
    showContextMenu(libraryBlockContextMenu, event.clientX, event.clientY);
  }
}
//=============================================================================
function showContextMenu(menu: HTMLElement, x: number, y: number) {
  hideAllContextMenus();

  const menuWidth = menu.offsetWidth || 200;
  const menuHeight = menu.offsetHeight || 150;
  const viewportWidth = window.innerWidth;
  const viewportHeight = window.innerHeight;
  let posX = x;
  let posY = y;

  if (posX + menuWidth > viewportWidth) {
    const flippedX = x - menuWidth - 10;
    posX = flippedX > 10 ? flippedX : Math.max(10, viewportWidth - menuWidth - 10);
  }
  if (posY + menuHeight > viewportHeight) {
    const flippedY = y - menuHeight - 10;
    posY = flippedY > 10 ? flippedY : Math.max(10, viewportHeight - menuHeight - 10);
  }

  menu.style.left = `${posX}px`;
  menu.style.top = `${posY}px`;
  menu.classList.add('show');
}
//=============================================================================
function handleBlockContextMenu(event: MouseEvent, blockId: string | null) {
  event.preventDefault();
  event.stopPropagation();
  if (!stateRef) return;
  stateRef.selectedId = blockId as any;

  if (blockContextMenu) {
    const item = blockContextMenu.querySelector<HTMLElement>('[data-action="createSubsystem"]');
    if (item) {
      const selectedCount = stateRef && stateRef.selectedIds ? stateRef.selectedIds.size : 0;
      const showSubsystem = selectedCount >= 2;

      if (showSubsystem) {
        item.classList.remove('context-menu-item--hidden');
      } else {
        item.classList.add('context-menu-item--hidden');
      }

      const prevEl = item.previousElementSibling as HTMLElement | null;
      const nextEl = item.nextElementSibling as HTMLElement | null;
      if (prevEl?.classList.contains('context-menu-divider')) {
        if (showSubsystem) prevEl.classList.remove('context-menu-divider--hidden');
        else prevEl.classList.add('context-menu-divider--hidden');
      }
      if (nextEl?.classList.contains('context-menu-divider')) {
        if (showSubsystem) nextEl.classList.remove('context-menu-divider--hidden');
        else nextEl.classList.add('context-menu-divider--hidden');
      }
    }
  }

  if (blockContextMenu) {
    const genItem = blockContextMenu.querySelector<HTMLElement>(
      '[data-action="generateSubsystemCode"]'
    );
    const genDivider = blockContextMenu.querySelector<HTMLElement>(
      '.context-menu-divider--subsystem-gen'
    );
    if (genItem) {
      let block: any = null;
      if (blockId !== null && stateRef?.blocks) {
        block = stateRef.blocks.get(blockId as any) ?? null;
        if (!block) {
          for (const [k, v] of stateRef.blocks) {
            // eslint-disable-next-line eqeqeq
            if (String(k) === String(blockId)) {
              block = v;
              break;
            }
          }
        }
      }
      const isSubsystem = block?.type === 'subsystem';
      if (isSubsystem) {
        genItem.classList.remove('context-menu-item--hidden');
        genDivider?.classList.remove('context-menu-divider--hidden');
      } else {
        genItem.classList.add('context-menu-item--hidden');
        genDivider?.classList.add('context-menu-divider--hidden');
      }
    }
  }

  if (blockContextMenu) {
    const selCount = stateRef && stateRef.selectedIds ? stateRef.selectedIds.size : 0;
    const showAlign = selCount >= 2;
    const showDistribute = selCount >= 3;
    const alignDivider = blockContextMenu.querySelector<HTMLElement>(
      '.context-menu-divider--align'
    );
    if (alignDivider) {
      if (showAlign) alignDivider.classList.remove('context-menu-divider--hidden');
      else alignDivider.classList.add('context-menu-divider--hidden');
    }
    blockContextMenu.querySelectorAll<HTMLElement>('.context-menu-item--align').forEach((el) => {
      if (showAlign) el.classList.remove('context-menu-item--hidden');
      else el.classList.add('context-menu-item--hidden');
    });
    blockContextMenu
      .querySelectorAll<HTMLElement>('.context-menu-item--distribute')
      .forEach((el) => {
        if (showDistribute) el.classList.remove('context-menu-item--hidden');
        else el.classList.add('context-menu-item--hidden');
      });
    blockContextMenu
      .querySelectorAll<HTMLElement>('.context-menu-divider--distribute')
      .forEach((el) => {
        if (showDistribute) el.classList.remove('context-menu-divider--hidden');
        else el.classList.add('context-menu-divider--hidden');
      });
  }

  if (blockContextMenu) showContextMenu(blockContextMenu, event.clientX, event.clientY);
}
//=============================================================================
/** Pinned wire under the last canvas right-click (target of autoRouteWire). */
let contextPinnedWire: unknown = null;
function handleCanvasContextMenu(event: MouseEvent) {
  event.preventDefault();
  event.stopPropagation();

  contextPinnedWire = null;
  try {
    const renderer = rendererRef?.current as any;
    if (renderer?.clientToSvg && renderer?.findNearestConnection) {
      const point = renderer.clientToSvg(event.clientX, event.clientY);
      const conn = renderer.findNearestConnection(point);
      const pts = (conn as any)?.points;
      if (Array.isArray(pts) && pts.some((p: any) => p?.pin)) contextPinnedWire = conn;
    }
  } catch (_err) {
    contextPinnedWire = null;
  }
  if (canvasContextMenu) {
    const item = canvasContextMenu.querySelector<HTMLElement>('[data-action="autoRouteWire"]');
    if (item) {
      if (contextPinnedWire) item.classList.remove('context-menu-item--hidden');
      else item.classList.add('context-menu-item--hidden');
    }
  }

  if (canvasContextMenu) showContextMenu(canvasContextMenu, event.clientX, event.clientY);
}
//=============================================================================
function handleBlockAction(action: string | null) {
  hideAllContextMenus();
  // Alignment/distribution actions work on the multi-selection and don't
  // need a single primary selectedId; handle them before the guard below.
  if (
    action === 'alignLeft' ||
    action === 'alignCenterH' ||
    action === 'alignRight' ||
    action === 'alignTop' ||
    action === 'alignMiddleV' ||
    action === 'alignBottom' ||
    action === 'distributeH' ||
    action === 'distributeV'
  ) {
    handleAlignAction(action);
    return;
  }
  if (action === 'createSubsystem') {
    try {
      import('./subsystem-handlers.js')
        .then((module) => module.createSubsystemFromSelection())
        .catch((error) => console.error('[create subsystem]', error));
    } catch (error) {
      console.error('[create subsystem]', error);
    }
    return;
  }
  if (!stateRef || !stateRef.selectedId) return;
  const selectedId = stateRef.selectedId as any;
  let block = stateRef.blocks.get(selectedId) as any;
  if (!block) {
    // tolerant lookup: handle possible number/string key mismatches
    for (const [k, v] of stateRef.blocks) {
      // eslint-disable-next-line eqeqeq
      if (k == selectedId || String(k) === String(selectedId)) {
        block = v as any;
        break;
      }
    }
  }
  if (!block) return;

  switch (action) {
    case 'mirrorHorizontal': {
      if (!(block as any).mirrorHorizontal) (block as any).mirrorHorizontal = false;
      (block as any).mirrorHorizontal = !(block as any).mirrorHorizontal;
      const centerX = (block as any).width / 2;
      ((block as any).ports || []).forEach((port: any) => {
        const mirroredX = centerX + (centerX - port.x);
        port.x = mirroredX;
        port.wireX = mirroredX;
        if (port.el) {
          const hit = port.el.querySelector('.port-hit');
          if (hit) hit.setAttribute('cx', String(mirroredX));
          const shape = port.el.querySelector('.port');
          if (shape) {
            const tag = shape.tagName.toLowerCase();
            if (tag === 'rect') {
              shape.setAttribute('x', String(mirroredX - 6));
            } else if (tag === 'circle') {
              shape.setAttribute('cx', String(mirroredX));
            } else if (tag === 'polygon') {
              const py = port.y ?? 0;
              const points = `${mirroredX - 6},${py - 6} ${mirroredX - 6},${py + 6} ${
                mirroredX + 6
              },${py}`;
              shape.setAttribute('points', points);
            }
          }
        }
      });
      rendererRef?.current?.updateBlockTransform?.(block);
      stateRef.routingDirty = true;
      stateRef.dirtyBlocks?.add((block as any).id);
      stateRef.fastRouting = false;
      rendererRef?.current?.updateConnections?.(true);
      break;
    }

    case 'mirrorVertical': {
      if (!(block as any).mirrorVertical) (block as any).mirrorVertical = false;
      (block as any).mirrorVertical = !(block as any).mirrorVertical;
      const centerY = (block as any).height / 2;
      ((block as any).ports || []).forEach((port: any) => {
        const mirroredY = centerY + (centerY - port.y);
        port.y = mirroredY;
        port.wireY = mirroredY;
        if (port.el) {
          const hit = port.el.querySelector('.port-hit');
          if (hit) hit.setAttribute('cy', String(mirroredY));
          const shape = port.el.querySelector('.port');
          if (shape) {
            const tag = shape.tagName.toLowerCase();
            if (tag === 'rect') {
              shape.setAttribute('y', String(mirroredY - 6));
            } else if (tag === 'circle') {
              shape.setAttribute('cy', String(mirroredY));
            } else if (tag === 'polygon') {
              const px = port.x ?? 0;
              const points = `${px - 6},${mirroredY - 6} ${px - 6},${mirroredY + 6} ${
                px + 6
              },${mirroredY}`;
              shape.setAttribute('points', points);
            }
          }
        }
      });
      rendererRef?.current?.updateBlockTransform?.(block);
      stateRef.routingDirty = true;
      stateRef.dirtyBlocks?.add((block as any).id);
      stateRef.fastRouting = false;
      rendererRef?.current?.updateConnections?.(true);
      break;
    }

    case 'rotateLeft':
      (block as any).rotation = (((block as any).rotation || 0) - 90 + 360) % 360;
      rendererRef?.current?.updateBlockTransform?.(block);
      stateRef.routingDirty = true;
      stateRef.dirtyBlocks?.add((block as any).id);
      stateRef.fastRouting = false;
      rendererRef?.current?.updateConnections?.(true);
      break;

    case 'rotateRight':
      (block as any).rotation = (((block as any).rotation || 0) + 90) % 360;
      rendererRef?.current?.updateBlockTransform?.(block);
      stateRef.routingDirty = true;
      stateRef.dirtyBlocks?.add((block as any).id);
      stateRef.fastRouting = false;
      rendererRef?.current?.updateConnections?.(true);
      break;

    case 'delete':
      if (stateRef.selectedId) {
        rendererRef?.current?.deleteBlock?.(stateRef.selectedId as any);
        stateRef.selectedId = null as any;
        if (stateRef.selectedIds) stateRef.selectedIds.delete(stateRef.selectedId as any);
      }
      break;
    case 'copy':
      try {
        import('./history.js')
          .then((m: any) => m.copySelection && m.copySelection())
          .catch(() => {});
      } catch (_e) {}
      break;
    case 'cut':
      try {
        import('./history.js').then((m: any) => m.cutSelection && m.cutSelection()).catch(() => {});
      } catch (_e) {}
      break;
    case 'generateSubsystemCode':
      if (block && typeof _onGenerateBlockCode === 'function') {
        _onGenerateBlockCode(block);
      }
      break;
    case 'blockHelp':
      void openBlockHelp(block);
      break;
    case 'properties': {
      try {
        _renderInspector?.(block);
      } catch (_e) {}
      try {
        _focusPropertiesPanel?.();
      } catch (_e) {}
      break;
    }
  }
}
/**
 * Re-lay the whole diagram as a left-to-right layered flow and fit it into
 * view. Shared by the canvas context menu and the menu-bar action so both entry
 * points behave identically.
 */
export function runAutoArrange(): void {
  try {
    // Acausal (physical-pin) diagrams are undirected circuits: lay them as a
    // schematic (source left, series rail, ground bottom). Causal signal
    // diagrams keep the left-to-right flow layout.
    const arranged = diagramHasPhysicalPins(stateRef)
      ? schematicArrangeDiagram(stateRef, rendererRef?.current as any)
      : autoArrangeDiagram(stateRef, rendererRef?.current as any);
    if (arranged && fitToDiagram) fitToDiagram();
  } catch (_e) {
    /* best-effort UI command */
  }
}
//=============================================================================
export function handleAlignAction(action: string) {
  if (!stateRef || !stateRef.selectedIds || stateRef.selectedIds.size < 2) return;

  const selectedBlocks: any[] = [];
  (stateRef.selectedIds as Set<any>).forEach((id: any) => {
    const b = stateRef!.blocks.get(id) as any;
    if (b) selectedBlocks.push(b);
  });
  if (selectedBlocks.length < 2) return;

  applyAlignment(selectedBlocks, action as any);

  selectedBlocks.forEach((b: any) => {
    rendererRef?.current?.updateBlockTransform?.(b);
    stateRef?.dirtyBlocks?.add(b.id as number);
  });
  if (stateRef) {
    stateRef.routingDirty = true;
    (stateRef as any).fastRouting = false;
  }
  rendererRef?.current?.updateConnections?.(true);
}
//=============================================================================
function handleCanvasAction(action: string | null) {
  hideAllContextMenus();
  if (!stateRef) return;
  switch (action) {
    case 'resetView':
      if (fitToDiagram) fitToDiagram();
      break;
    case 'autoRouteWire': {
      const renderer = rendererRef?.current as any;
      if (contextPinnedWire && renderer?.releaseWirePins) {
        renderer.releaseWirePins(contextPinnedWire);
      }
      contextPinnedWire = null;
      break;
    }
    case 'autoArrange':
      runAutoArrange();
      break;
    case 'recalculateWires': {
      // Force a full routing pass even when auto-route is disabled.
      if (stateRef) {
        stateRef.routingDirty = true;
        stateRef.fastRouting = false;
      }
      try {
        // Prefer forceFullRoute (full A* pass, may be async); fall back to
        // updateConnections(true).
        if (rendererRef?.current?.forceFullRoute) {
          void rendererRef.current.forceFullRoute(2000);
        } else {
          rendererRef?.current?.updateConnections?.(true);
        }
      } catch (_e) {}
      break;
    }
    case 'paste':
      try {
        import('./history.js')
          .then((m: any) => m.pasteSelection && m.pasteSelection())
          .catch(() => {});
      } catch (_e) {}
      break;
    case 'selectAll': {
      if (stateRef && stateRef.blocks) {
        const allBlockIds = new Set(Array.from(stateRef.blocks.keys()));
        const allConns = new Set(stateRef.connections || []);
        if (rendererRef?.current && typeof rendererRef.current.setMultiSelection === 'function') {
          rendererRef.current.setMultiSelection(allBlockIds as any, allConns as any);
        } else {
          const sRef = stateRef as AppState;
          sRef.selectedIds?.clear();
          allBlockIds.forEach((id) => sRef.selectedIds?.add(id as any));
          sRef.selectedConnections?.clear();
          allConns.forEach((c) => sRef.selectedConnections?.add(c as any));
          sRef.selectedId =
            allBlockIds.size === 1 && allConns.size === 0
              ? (Array.from(allBlockIds)[0] as any)
              : null;
          sRef.selectedConnection =
            allConns.size === 1 && allBlockIds.size === 0 ? (Array.from(allConns)[0] as any) : null;
          rendererRef?.current?.updateSelectionBox?.();
        }
      }
      break;
    }
  }
}
//=============================================================================
export function initContextMenu() {
  if (!svg || !stateRef || !rendererRef) return;

  svg.addEventListener('contextmenu', (event) => {
    const target = event.target as Element;
    const blockElement = target.closest('[data-block-id]') as Element | null;
    if (blockElement) {
      const blockId = blockElement.getAttribute('data-block-id');
      handleBlockContextMenu(event as MouseEvent, blockId);
      return;
    }
    handleCanvasContextMenu(event as MouseEvent);
  });

  document.addEventListener('click', (event) => {
    const target = event.target as Element;
    if (!target.closest('.context-menu')) hideAllContextMenus();
  });

  document.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') hideAllContextMenus();
  });

  // Programmatic entry point for Auto-arrange (menu bar, scripts, headless regeneration).
  document.addEventListener('nflow:auto-arrange', () => runAutoArrange());

  if (blockContextMenu) {
    // Use event delegation so items added after init are handled too.
    blockContextMenu.addEventListener('click', (ev) => {
      const target = ev.target as Element;
      const item = target.closest('.context-menu-item') as HTMLElement | null;
      if (!item) return;
      // Don't close/activate when clicking the submenu trigger itself.
      if (item.classList.contains('context-menu-item--has-submenu')) return;
      const action = item.getAttribute('data-action');
      handleBlockAction(action);
    });
  }

  if (canvasContextMenu) {
    canvasContextMenu.addEventListener('click', (ev) => {
      const target = ev.target as Element;
      const item = target.closest('.context-menu-item') as HTMLElement | null;
      if (!item) return;
      const action = item.getAttribute('data-action');
      handleCanvasAction(action);
    });
  }

  if (libraryBlockContextMenu) {
    libraryBlockContextMenu.addEventListener('click', (ev) => {
      const target = ev.target as Element;
      const item = target.closest('.context-menu-item') as HTMLElement | null;
      if (item?.dataset.action !== 'libraryBlockHelp' || !libraryBlockType) return;
      const type = libraryBlockType;
      hideAllContextMenus();
      void openBlockHelp({ type });
    });
  }
}
//=============================================================================
