//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { state } from './state.js';
import { buildAllBlockTemplates, getDiscoveredManifests } from './library-loader.js';
import { getViewBox as getWorkspaceViewBox } from './workspace-handlers.js';
import * as FieldNames from '../FieldNames.js';
import { t } from '../utils/i18n.js';
import { pushStatus } from './status-service.js';
import { renderTeXMath } from '../render/svg-elements.js';
import { showLibraryBlockContextMenu } from './context-menu-manager.js';
import {
  renderBlockLibraryView,
  filterBlockLibraryView,
  groupPreferenceKey,
  type BlockLibraryViewGroup,
  type BlockLibraryViewItem,
} from './components/block-library-view.js';
import { getUiPref } from './ui-prefs.js';
import {
  getIcon,
  getIconSync,
  prewarmIcons,
  toExportsUrl,
  loadBuiltinSprite,
  type CachedIcon,
} from '../libraries/icon-cache.js';
//=============================================================================
// Indirection: keeps `new URL(rel, base)` resolved at runtime instead of
// being rewritten by Vite's build-time asset analysis.
const MODULE_URL: string = import.meta.url;
const NFLOW_ROOT_URL: string =
  typeof document !== 'undefined' ? document.baseURI : new URL('../', MODULE_URL).href;
/** Dual-write: keeps legacy statusEl write for tests AND notifies the service. */
function _setStatus(
  el: HTMLElement | null,
  msg: string,
  level: 'info' | 'warn' | 'error' | 'success' = 'info',
  durationMs = 3000
): void {
  if (el) el.textContent = msg;
  pushStatus({ level, message: msg, durationMs });
}
//=============================================================================
declare global {
  interface Window {
    __renderBlockLibraryRetryCount?: number;
    blockTemplates?: Record<string, Template>;
    katex?: { render?: (tex: string, el: Element, opts?: unknown) => void };
    __katexLoading?: Promise<void>;
  }
}
//=============================================================================
export type BlockItem = {
  type: string;
  label: string;
  subsystemKey?: string;
  [k: string]: unknown;
};
//=============================================================================
export type BlockGroup = { id?: string; title?: string; blocks: BlockItem[] };
export type BlockLike = { group: Element; width: number; height: number; params?: unknown };
export type Template = {
  render?: (b: BlockLike) => void;
  width: number;
  height: number;
  defaultParams?: unknown;
};
//=============================================================================
export type RendererRef = {
  current: {
    createBlock?: (
      type: string,
      x: number,
      y: number,
      options?: Record<string, unknown>
    ) => unknown;
    selectBlock?: (id: string) => void;
    updateConnections?: (fast?: boolean) => void;
  } | null;
} | null;
//=============================================================================
let blockLibraryGroups: HTMLElement | null = null;
let blockLibrary: BlockGroup[] | null = null;
let GRID_SIZE = 20;
let rendererRef: RendererRef = null;
let statusEl: HTMLElement | null = null;
let _clickListenerAttached = false;
let _dragListenerAttached = false;
let _contextListenerAttached = false;
export function setBlockLibraryGroups(el: HTMLElement | null) {
  blockLibraryGroups = el;
  _clickListenerAttached = false; // reset so the listener is re-attached to the new element
  _dragListenerAttached = false;
  _contextListenerAttached = false;
}
//=============================================================================
export function setBlockLibrary(lib: BlockGroup[] | null) {
  blockLibrary = lib;
}
//=============================================================================
export function setGridSize(size: number) {
  GRID_SIZE = size;
}
//=============================================================================
export function setRendererRef(ref: RendererRef) {
  rendererRef = ref;
}
//=============================================================================
export function setStatusEl(el: HTMLElement | null) {
  statusEl = el;
}
//=============================================================================
export function getBlockLibraryGroups() {
  return blockLibraryGroups;
}
//=============================================================================
export function getBlockLibrary() {
  return blockLibrary;
}
//=============================================================================
export function getGridSize() {
  return GRID_SIZE;
}
//=============================================================================
function resolveLibraryViewItem(item: BlockItem): BlockLibraryViewItem {
  const manifests = getDiscoveredManifests();
  let foundDir: string | null = null;
  let foundBlock: Record<string, unknown> | undefined = undefined;

  for (const [_id, manifest] of manifests) {
    try {
      const m = manifest as Record<string, unknown> | undefined;
      if (!m) continue;
      const maybeBlocks = m[FieldNames.kBlocks] as unknown;
      const blocks = Array.isArray(maybeBlocks) ? (maybeBlocks as unknown[]) : undefined;
      if (!Array.isArray(blocks)) continue;

      const found = blocks.find((b) => {
        if (!b || typeof b !== 'object') return false;
        const bb = b as Record<string, unknown>;
        return (
          typeof bb[FieldNames.kType] === 'string' && (bb[FieldNames.kType] as string) === item.type
        );
      });
      if (found) {
        const dirVal = m['_dir'] as string | undefined;
        foundDir = dirVal ?? null;
        foundBlock = found as Record<string, unknown>;
        break;
      }
    } catch {
      // ignore malformed manifests
    }
  }

  const iconPath =
    foundBlock && typeof foundBlock['icon'] === 'string'
      ? (foundBlock['icon'] as string)
      : undefined;

  return {
    type: item.type,
    label: item.label,
    subsystemKey: item.subsystemKey,
    iconDir: foundDir ?? undefined,
    iconPath,
  };
}
//=============================================================================
function paletteIconAsImage(iconSvg: SVGElement, iconUrl: string): void {
  const targetHeight = 80;
  const targetWidth = 80;
  iconSvg.setAttribute('width', String(targetWidth));
  iconSvg.setAttribute('height', String(targetHeight));
  iconSvg.setAttribute('viewBox', `0 0 ${targetWidth} ${targetHeight}`);
  const img = document.createElementNS('http://www.w3.org/2000/svg', 'image');
  img.setAttribute('href', iconUrl);
  try {
    img.setAttributeNS('http://www.w3.org/1999/xlink', 'xlink:href', iconUrl);
  } catch {}
  img.setAttribute('width', String(targetWidth));
  img.setAttribute('height', String(targetHeight));
  img.setAttribute('preserveAspectRatio', 'xMidYMid meet');
  iconSvg.replaceChildren(img as unknown as Node);
}
//=============================================================================
function paletteIconFromCache(iconSvg: SVGElement, cached: CachedIcon | null | undefined): boolean {
  if (!cached || !cached.svgEl) return false;
  try {
    const svgEl = cached.svgEl.cloneNode(true) as SVGSVGElement;
    const targetSize = 80;
    if (!svgEl.getAttribute('viewBox')) svgEl.setAttribute('viewBox', '0 0 72 72');
    svgEl.setAttribute('width', String(targetSize));
    svgEl.setAttribute('height', String(targetSize));
    svgEl.setAttribute('preserveAspectRatio', 'xMidYMid meet');
    iconSvg.replaceChildren(
      ...Array.from(svgEl.childNodes).map((n) => document.importNode(n, true))
    );
    for (const attr of Array.from(svgEl.attributes)) {
      if (attr.name !== 'width' && attr.name !== 'height') {
        iconSvg.setAttribute(attr.name, attr.value);
      }
    }
  } catch {
    return false;
  }
  return true;
}
//=============================================================================
function hydrateLibraryIcons(container: HTMLElement): void {
  const buttons = Array.from(container.querySelectorAll<HTMLElement>('.tool[data-icon-path]'));
  // Start the builtin sprite load first (idempotent): the palette can hydrate before
  // initLibraries() kicks it off, and getIcon() then waits for one sprite request
  // instead of a per-file fetch.
  try {
    void loadBuiltinSprite(new URL('libraries/_builtin-icons.sprite.svg', NFLOW_ROOT_URL).href);
  } catch {
    // ignore malformed URL; getIcon falls back to per-file fetches
  }
  // Fire all icon fetches in parallel; the shared cache dedupes them with the
  // per-button getIcon() calls and survives palette rebuilds.
  const urls: string[] = [];
  for (const button of buttons) {
    const foundDir = button.dataset.iconDir || '';
    const iconPath = button.dataset.iconPath || '';
    if (foundDir && iconPath && iconPath.toLowerCase().endsWith('.svg')) {
      urls.push(toExportsUrl(new URL(`libraries/${foundDir}/${iconPath}`, NFLOW_ROOT_URL).href));
    }
  }
  void prewarmIcons(urls);

  for (const button of buttons) {
    const foundDir = button.dataset.iconDir || '';
    const iconPath = button.dataset.iconPath || '';
    const iconSvg = button.querySelector<SVGElement>('svg[data-library-icon]');
    if (!foundDir || !iconPath || !iconSvg) continue;

    const iconUrl = new URL(`libraries/${foundDir}/${iconPath}`, NFLOW_ROOT_URL).href;
    const isSvg = iconPath.toLowerCase().endsWith('.svg');

    if (!isSvg) {
      paletteIconAsImage(iconSvg, iconUrl);
      continue;
    }

    // Normalize to the exports/ path so palette and canvas share one cache
    // entry; fall back to an <image href> on the original URL if the fetch fails.
    const cacheUrl = toExportsUrl(iconUrl);
    const sync = getIconSync(cacheUrl);
    if (sync) {
      if (!paletteIconFromCache(iconSvg, sync)) paletteIconAsImage(iconSvg, iconUrl);
      continue;
    }
    void getIcon(cacheUrl).then((cached) => {
      if (!paletteIconFromCache(iconSvg, cached)) paletteIconAsImage(iconSvg, iconUrl);
    });
  }
}
//=============================================================================
function attachBlockLibraryDragListener(container: HTMLElement): void {
  if (_dragListenerAttached) return;
  _dragListenerAttached = true;
  container.addEventListener('dragstart', (event: DragEvent) => {
    const target = event.target as Element | null;
    const button = target?.closest('.tool') as HTMLElement | null;
    if (!button || !event.dataTransfer) return;

    event.dataTransfer.effectAllowed = 'copy';
    event.dataTransfer.setData(
      'text/plain',
      JSON.stringify({
        type: button.dataset.type || '',
        subsystemKey: button.dataset.subsystemKey || '',
      })
    );

    const dragGhost = button.cloneNode(true) as HTMLElement;
    dragGhost.style.position = 'absolute';
    dragGhost.style.top = '-9999px';
    dragGhost.style.left = '-9999px';
    dragGhost.style.opacity = '0.8';
    document.body.appendChild(dragGhost);
    event.dataTransfer.setDragImage(dragGhost, 16, 16);

    setTimeout(() => {
      if (dragGhost.parentNode) document.body.removeChild(dragGhost);
    }, 0);
  });
}
//=============================================================================
function attachBlockLibraryContextListener(container: HTMLElement): void {
  if (_contextListenerAttached) return;
  _contextListenerAttached = true;
  container.addEventListener('contextmenu', (event: MouseEvent) => {
    const target = event.target as Element | null;
    const button = target?.closest('.tool') as HTMLElement | null;
    const type = button?.dataset.type || '';
    if (type) showLibraryBlockContextMenu(event, type);
  });
}
//=============================================================================
export async function ensureKatexLoaded(): Promise<void> {
  if (window.katex && typeof window.katex.render === 'function') return;
  if (!window.__katexLoading) {
    let cssLoaded = Promise.resolve();
    try {
      const css = document.createElement('link');
      css.rel = 'stylesheet';
      css.href = new URL('assets/katex/katex.min.css', NFLOW_ROOT_URL).href;
      cssLoaded = new Promise<void>((res) => {
        css.onload = () => res();
        css.onerror = () => res();
      });
      document.head.appendChild(css);
    } catch {
      cssLoaded = Promise.resolve();
    }

    const scriptLoaded = new Promise<void>((res) => {
      try {
        const s = document.createElement('script');
        s.src = new URL('assets/katex/katex.min.js', NFLOW_ROOT_URL).href;
        s.onload = () => {
          res();
        };
        s.onerror = () => {
          res();
        };
        document.head.appendChild(s);
      } catch {
        res();
      }
    });

    window.__katexLoading = (async () => {
      await Promise.all([cssLoaded, scriptLoaded]);
    })();
  }
  try {
    await window.__katexLoading;
  } catch {}
}
//=============================================================================
export function renderBlockLibrary(): void {
  if (!blockLibraryGroups) {
    window.__renderBlockLibraryRetryCount = (window.__renderBlockLibraryRetryCount || 0) + 1;
    if ((window.__renderBlockLibraryRetryCount || 0) < 6) {
      setTimeout(renderBlockLibrary, 150);
    }
    return;
  }
  window.__renderBlockLibraryRetryCount = 0;
  if (!blockLibraryGroups) return;
  const groups = [...(blockLibrary || [])];

  const helpers = {
    GRID_SIZE,
    svgRect: (x: number, y: number, w: number, h: number, cls: string) => {
      const el = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
      el.setAttribute('x', String(x));
      el.setAttribute('y', String(y));
      el.setAttribute('width', String(Math.max(0, w)));
      el.setAttribute('height', String(Math.max(0, h)));
      el.setAttribute('class', cls);
      return el;
    },
    svgText: (x: number, y: number, text: string) => {
      return helpers.createSvgElement('text', { x, y, class: 'block-text upright' }, text);
    },
    createSvgElement: (tag: string, attrs: Record<string, unknown> = {}, text = '') => {
      const el = document.createElementNS('http://www.w3.org/2000/svg', tag);
      Object.entries(attrs).forEach(([key, value]) => {
        el.setAttribute(key, String(value));
      });
      if (text) el.textContent = text;
      return el;
    },
    renderTeXMath: (group: Element, tex: string, width: number, height: number) => {
      renderTeXMath(group, tex, width, height);
      if (!window.katex || typeof window.katex.render !== 'function') {
        void ensureKatexLoaded();
      }
    },
    renderSourcePlot: (group: Element, width: number, height: number, plotPath?: string) => {
      group.appendChild(helpers.svgRect(0, 0, width, height, 'nf-block'));
      const padding = Math.min(4, width * 0.05, height * 0.05);
      const axisX = padding;
      const axisY = height - padding;
      const axisTop = padding;
      const axisRight = width - padding;
      if (axisY > axisTop && axisRight > axisX) {
        group.appendChild(
          helpers.createSvgElement('line', {
            x1: axisX,
            y1: axisY,
            x2: axisRight,
            y2: axisY,
            class: 'axis',
          })
        );
        group.appendChild(
          helpers.createSvgElement('line', {
            x1: axisX,
            y1: axisY,
            x2: axisX,
            y2: axisTop,
            class: 'axis',
          })
        );
      }
      if (plotPath) {
        group.appendChild(
          helpers.createSvgElement('path', {
            d: plotPath,
            class: 'plot',
          })
        );
      }
    },
    renderCenteredAxesPlot: (group: Element, width: number, height: number, plotPath?: string) => {
      helpers.renderSourcePlot(group, width, height, plotPath);
    },
    loadSvgToIcon: async (
      foundDir: string,
      item: BlockItem,
      template: Template | undefined,
      iconSvg: SVGElement
    ) => {
      try {
        const baseNames = [
          item.type,
          (item.type || '').replace(/([a-z0-9])([A-Z])/g, '$1-$2').toLowerCase(),
        ] as string[];

        const candidates: string[] = [];
        for (const base of baseNames) {
          if (!base) continue;
          candidates.push(`libraries/${foundDir}/${base}.svg`);
          candidates.push(`libraries/${foundDir}/exports/${base}.svg`);
        }

        for (const rel of candidates) {
          try {
            const svgUrl = new URL(rel, NFLOW_ROOT_URL).href;
            const resp = await fetch(svgUrl);
            if (!resp.ok) continue;

            const svgText = await resp.text();
            const parser = new DOMParser();
            const doc = parser.parseFromString(svgText, 'image/svg+xml');
            const svgEl = doc.querySelector('svg');
            if (!svgEl) continue;

            const targetSize = 80;
            const viewBox = svgEl.getAttribute('viewBox');
            if (!viewBox) svgEl.setAttribute('viewBox', '0 0 72 72');

            svgEl.setAttribute('width', String(targetSize));
            svgEl.setAttribute('height', String(targetSize));
            svgEl.setAttribute('preserveAspectRatio', 'xMidYMid meet');

            iconSvg.replaceChildren(...Array.from(svgEl.childNodes));

            for (const attr of svgEl.attributes) {
              if (attr.name !== 'width' && attr.name !== 'height') {
                iconSvg.setAttribute(attr.name, attr.value);
              }
            }

            void template;
            return true;
          } catch {
            continue;
          }
        }

        return false;
      } catch (e) {
        console.error(e);
        return false;
      }
    },
    loadPngToIcon: async (foundDir: string, item: BlockItem, iconSvg: SVGElement) => {
      try {
        const baseNames = [
          item.type,
          (item.type || '').replace(/([a-z0-9])([A-Z])/g, '$1-$2').toLowerCase(),
        ] as string[];

        const candidates: string[] = [];
        for (const base of baseNames) {
          if (!base) continue;
          candidates.push(`libraries/${foundDir}/${base}.png`);
          candidates.push(`libraries/${foundDir}/exports/${base}.png`);
        }

        for (const rel of candidates) {
          try {
            const pngUrl = new URL(rel, NFLOW_ROOT_URL).href;
            const resp = await fetch(pngUrl);
            if (!resp.ok) continue;
            const targetHeight = 80;
            const targetWidth = 80;
            iconSvg.setAttribute('width', String(targetWidth));
            iconSvg.setAttribute('height', String(targetHeight));
            iconSvg.setAttribute('viewBox', `0 0 ${targetWidth} ${targetHeight}`);
            const img = document.createElementNS('http://www.w3.org/2000/svg', 'image');
            img.setAttribute('href', pngUrl);
            try {
              img.setAttributeNS('http://www.w3.org/1999/xlink', 'xlink:href', pngUrl);
            } catch {}
            img.setAttribute('width', String(targetWidth));
            img.setAttribute('height', String(targetHeight));
            img.setAttribute('preserveAspectRatio', 'xMidYMid meet');
            iconSvg.innerHTML = '';
            iconSvg.appendChild(img as unknown as Node);
            return true;
          } catch {
            continue;
          }
        }

        return false;
      } catch {
        return false;
      }
    },
    formatLabelTeX: (tex: string) => tex,
    buildTransferTeX: (num: string[], den: string[]) =>
      `\\frac{${num.join('s+')}}{${den.join('s+')}}`,
    renderLabelNode: (block: BlockLike, label: string, { showNode = true } = {}) => {
      const group = block.group as Element;
      if (!(group instanceof Element)) return;
      const tex = helpers.formatLabelTeX(label);
      const mathWidth = Math.max(block.width, tex.length * 8 + 12);
      const offsetX = (block.width - mathWidth) / 2;
      const mathGroup = helpers.createSvgElement('g', {
        class: 'label-math',
        transform: `translate(${offsetX},-24)`,
      });
      group.appendChild(mathGroup);
      helpers.renderTeXMath(mathGroup, tex, mathWidth, block.height);
      if (showNode) {
        group.appendChild(
          helpers.createSvgElement('circle', { cx: 20, cy: 20, r: 5, class: 'label-node' })
        );
      }
    },
  } as const;

  window.blockTemplates = buildAllBlockTemplates(
    helpers as unknown as Record<string, unknown>
  ) as Record<string, Template>;

  if (state.loadedSubsystems.size) {
    const subsystemBlocks = Array.from(state.loadedSubsystems.entries()).map(([key, spec]) => {
      const s = spec as { name?: string } | undefined;
      return {
        type: FieldNames.kSubsystem,
        label: s?.name || String(key),
        subsystemKey: key,
      } as BlockItem;
    });
    groups.push({
      id: 'subsystems',
      title: 'Subsystems',
      blocks: subsystemBlocks,
    });
  }

  const viewGroups: BlockLibraryViewGroup[] = groups.map((group) => ({
    id: group.id,
    title: group.title,
    blocks: group.blocks.map(resolveLibraryViewItem),
  }));

  // Each group's expanded state comes from its persisted preference (first group open
  // on a fresh profile); groups left open sort first, restoring the previous layout.
  // The default is keyed to the ORIGINAL index, and the sort is stable, so groups keep
  // their relative order within the open and closed partitions.
  const decorated = viewGroups.map((group, index) => ({
    group,
    index,
    open: getUiPref(groupPreferenceKey(group, index), index === 0 ? '1' : '0') === '1',
  }));
  decorated.sort((a, b) => (a.open === b.open ? a.index - b.index : a.open ? -1 : 1));
  const orderedGroups = decorated.map(({ group, open }) => {
    group.open = open;
    return group;
  });

  renderBlockLibraryView(blockLibraryGroups, orderedGroups);
  const search = document.getElementById('blockLibrarySearch') as HTMLInputElement | null;
  if (search?.value) filterBlockLibraryView(blockLibraryGroups, search.value);
  hydrateLibraryIcons(blockLibraryGroups);
  attachBlockLibraryDragListener(blockLibraryGroups);
  attachBlockLibraryContextListener(blockLibraryGroups);

  // The open/collapsed state is driven only by the persisted preference. No group is
  // forced open here: a palette the user fully collapsed must come back collapsed.

  if (blockLibraryGroups && !_clickListenerAttached) {
    _clickListenerAttached = true;
    blockLibraryGroups.addEventListener('click', (event: MouseEvent) => {
      const target = event.target as Element | null;
      const button = target?.closest('.tool') as HTMLElement | null;
      if (!button) return;
      const type = button.dataset.type as string;
      const subsystemKey = button.dataset.subsystemKey || '';
      const viewBox = getWorkspaceViewBox();
      const centerX = viewBox ? viewBox.x + viewBox.w / 2 : 400;
      const centerY = viewBox ? viewBox.y + viewBox.h / 2 : 300;
      const offset = spawnOffsetForIndex(state.spawnIndex++);
      try {
        const options: Record<string, unknown> = {};
        if (type === FieldNames.kSubsystem && subsystemKey) {
          const spec = state.loadedSubsystems.get(subsystemKey) as
            | {
                name?: string;
                externalInputs?: unknown;
                externalOutputs?: unknown;
                masked?: boolean;
                maskParameters?: Record<string, unknown>;
              }
            | undefined;
          if (!spec) throw new Error('Subsystem spec not found');
          const params: Record<string, unknown> = {
            name: spec.name,
            externalInputs: deepClone(spec.externalInputs),
            externalOutputs: deepClone(spec.externalOutputs),
            subsystem: deepClone(spec),
          };
          // Restore a reusable / masked block's mask flag and parameter set so a
          // placed instance resolves its inner params (and shows the lock).
          if (spec.masked) params.masked = true;
          if (spec.maskParameters && typeof spec.maskParameters === 'object') {
            params.maskParameters = deepClone(spec.maskParameters);
          }
          options.params = params;
        }
        const block = rendererRef?.current?.createBlock?.(
          type,
          centerX + offset.x,
          centerY + offset.y,
          options
        ) as { id?: string } | null;
        if (block) {
          if (block.id) rendererRef?.current?.selectBlock?.(block.id);
          state.routingDirty = true;
          rendererRef?.current?.updateConnections?.(true);
          signalDiagramChanged();
          _setStatus(statusEl, t('Added {type}', { type }), 'success', 2000);
        }
      } catch (error) {
        _setStatus(statusEl, t('Error adding {type}', { type }), 'error');
        console.error('Error adding block:', error);
      }
    });
  }
}
//=============================================================================
const deepClone = <T>(value: T): T => JSON.parse(JSON.stringify(value));
export const spawnOffsetForIndex = (index: number) => {
  const offsets = [
    { x: -180, y: 0 },
    { x: -60, y: 0 },
    { x: 100, y: 0 },
    { x: -180, y: 180 },
    { x: -60, y: 180 },
    { x: 100, y: 180 },
    { x: -180, y: -180 },
    { x: -60, y: -180 },
    { x: 100, y: -180 },
  ];
  return offsets[index % offsets.length];
};
//=============================================================================
export const signalDiagramChanged = () => {
  window.dispatchEvent(new CustomEvent('diagramChanged'));
};
//=============================================================================
export default {
  setBlockLibraryGroups,
  setBlockLibrary,
  setGridSize,
  setRendererRef,
  setStatusEl,
  getBlockLibraryGroups,
  getBlockLibrary,
  getGridSize,
  renderBlockLibrary,
};
//=============================================================================
