//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Quick-insert: double-click on empty canvas opens a small fuzzy search box;
// typing a block type and pressing Enter inserts it at that position
// The editor reuses this path for canvas insertion.
//=============================================================================
import { useEffect, useMemo, useRef, useState } from 'react';
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
import { blockTemplates, refreshBlockTemplates } from '../render/block/templates.js';
import builtinRegistry from './blocks-registry.js';
import { state, signalDiagramChanged } from './state.js';
import { writeStatus } from './status-service.js';
import { t } from '../utils/i18n.js';
//=============================================================================
interface RendererLike {
  createBlock?: (type: string, x: number, y: number, options?: unknown) => unknown;
  updateConnections?: (fast?: boolean) => void;
  clientToSvg?: (clientX: number, clientY: number) => { x: number; y: number };
}
//=============================================================================
let rendererRef: { current: RendererLike | null } | null = null;
let popup: HTMLElement | null = null;
let popupRoot: Root | null = null;
let worldPos = { x: 0, y: 0 };
export function setRendererRef(ref: { current: RendererLike | null } | null): void {
  rendererRef = ref;
}
//=============================================================================
/** Insertable block types: template keys + declarative registry definitions. */
export function getQuickInsertTypes(): string[] {
  try {
    refreshBlockTemplates();
  } catch {
    /* ignore */
  }
  const keys = new Set<string>(Object.keys((blockTemplates as Record<string, unknown>) || {}));
  try {
    for (const type of Object.keys(builtinRegistry.getDefinitions() || {})) {
      keys.add(type);
    }
  } catch {
    /* ignore */
  }
  return Array.from(keys)
    .filter((k) => !k.includes('.'))
    .sort();
}
/**
 * Score a candidate against the query: exact > prefix > substring >
 * subsequence > no match (0).
 */
export function scoreMatch(query: string, candidate: string): number {
  const q = query.toLowerCase();
  const c = candidate.toLowerCase();
  if (!q) return 1;
  if (c === q) return 1000;
  if (c.startsWith(q)) return 500 - c.length;
  const idx = c.indexOf(q);
  if (idx >= 0) return 250 - idx - c.length;
  let qi = 0;
  for (let ci = 0; ci < c.length && qi < q.length; ci++) {
    if (c[ci] === q[qi]) qi++;
  }
  if (qi === q.length) return 50 - c.length;
  return 0;
}
//=============================================================================
export function filterTypes(query: string, types?: string[]): string[] {
  const list = types ?? getQuickInsertTypes();
  return list
    .map((type) => ({ type, score: scoreMatch(query, type) }))
    .filter((e) => e.score > 0)
    .sort((a, b) => b.score - a.score || a.type.localeCompare(b.type))
    .map((e) => e.type);
}
//=============================================================================
function closePopup(): void {
  popupRoot?.unmount();
  popupRoot = null;
  popup?.remove();
  popup = null;
}
//=============================================================================
function insertType(type: string): void {
  const renderer = rendererRef?.current;
  try {
    const block = renderer?.createBlock?.(type, worldPos.x, worldPos.y) as {
      id?: string;
    } | null;
    if (block) {
      state.routingDirty = true;
      renderer?.updateConnections?.(true);
      signalDiagramChanged();
      // Link the console entry to the new block: clicking it selects/centers.
      writeStatus(null, t('Added {type}', { type }), 'success', 2000, block.id ?? null);
    } else {
      writeStatus(null, t('Error adding {type}', { type }), 'error');
    }
  } catch {
    writeStatus(null, t('Error adding {type}', { type }), 'error');
  }
  closePopup();
}
//=============================================================================
function QuickInsertPopup(): JSX.Element {
  const [query, setQuery] = useState('');
  const [selectedIndex, setSelectedIndex] = useState(0);
  const inputRef = useRef<HTMLInputElement | null>(null);
  const selectedRef = useRef(selectedIndex);
  const queryRef = useRef(query);

  const matches = useMemo(() => filterTypes(query), [query]);
  const boundedIndex = Math.min(selectedIndex, Math.max(0, matches.length - 1));

  useEffect(() => {
    selectedRef.current = boundedIndex;
  }, [boundedIndex]);

  useEffect(() => {
    queryRef.current = query;
  }, [query]);

  useEffect(() => {
    const input = inputRef.current;
    if (!input) return;

    const onInput = () => {
      queryRef.current = input.value;
      setQuery(input.value);
      setSelectedIndex(0);
      selectedRef.current = 0;
    };
    const onKeyDown = (ev: globalThis.KeyboardEvent) => {
      const currentMatches = filterTypes(queryRef.current);
      if (ev.key === 'Escape') {
        ev.preventDefault();
        closePopup();
      } else if (ev.key === 'ArrowDown') {
        ev.preventDefault();
        setSelectedIndex((idx) => Math.min(idx + 1, currentMatches.length - 1));
      } else if (ev.key === 'ArrowUp') {
        ev.preventDefault();
        setSelectedIndex((idx) => Math.max(idx - 1, 0));
      } else if (ev.key === 'Enter') {
        ev.preventDefault();
        const type = currentMatches[selectedRef.current];
        if (type) insertType(type);
      }
    };
    const onBlur = () => {
      // Delay so a pointerdown on a list item wins over the blur.
      setTimeout(closePopup, 150);
    };

    input.addEventListener('input', onInput);
    input.addEventListener('keydown', onKeyDown);
    input.addEventListener('blur', onBlur);
    input.focus();
    return () => {
      input.removeEventListener('input', onInput);
      input.removeEventListener('keydown', onKeyDown);
      input.removeEventListener('blur', onBlur);
    };
  }, []);

  useEffect(() => {
    const selected = document.querySelector('#quickInsertList li.selected') as HTMLElement | null;
    selected?.scrollIntoView?.({ block: 'nearest' });
  }, [boundedIndex, matches]);

  return (
    <>
      <input
        ref={inputRef}
        id="quickInsertInput"
        type="text"
        autoComplete="off"
        spellCheck="false"
        placeholder={t('Block type...')}
        aria-label={t('Block type...')}
      />
      <ul id="quickInsertList">
        {matches.map((type, index) => (
          <li
            key={type}
            className={index === boundedIndex ? 'selected' : ''}
            onPointerDown={(ev) => {
              ev.preventDefault();
              insertType(type);
            }}
          >
            {type}
          </li>
        ))}
      </ul>
    </>
  );
}
//=============================================================================
export function openQuickInsert(clientX: number, clientY: number, wx: number, wy: number): void {
  closePopup();
  worldPos = { x: wx, y: wy };

  popup = document.createElement('div');
  popup.id = 'quickInsertPopup';
  popup.dataset.reactRoot = 'quick-insert';
  const margin = 8;
  const width = 220;
  const left = Math.min(clientX, Math.max(margin, window.innerWidth - width - margin));
  popup.style.left = `${Math.max(margin, left)}px`;
  popup.style.top = `${Math.min(clientY, Math.max(margin, window.innerHeight - 240))}px`;
  document.body.appendChild(popup);
  popupRoot = createRoot(popup);
  flushSync(() => {
    popupRoot?.render(<QuickInsertPopup />);
  });
}
//=============================================================================
export function openQuickInsertAtCanvasCenter(): void {
  const svg = document.getElementById('svgCanvas') as SVGSVGElement | null;
  if (!svg || state.locked) return;
  const rect = svg.getBoundingClientRect();
  const clientX = rect.left + rect.width / 2;
  const clientY = rect.top + rect.height / 2;
  const renderer = rendererRef?.current;
  const point = renderer?.clientToSvg?.(clientX, clientY) ?? { x: clientX, y: clientY };
  openQuickInsert(clientX, clientY, point.x, point.y);
}
//=============================================================================
/** Attach the double-click trigger on the SVG canvas. Idempotent. */
export function initQuickInsert(svg: SVGSVGElement | null): void {
  if (!svg || (svg as unknown as { _quickInsertBound?: boolean })._quickInsertBound) return;
  (svg as unknown as { _quickInsertBound?: boolean })._quickInsertBound = true;
  svg.addEventListener('dblclick', (ev) => {
    if (state.locked) return;
    const target = ev.target as Element | null;
    // Only react on empty canvas: ignore blocks, wires and overlays.
    if (target?.closest('#blockLayer') || target?.closest('#wireLayer')) return;
    const renderer = rendererRef?.current;
    const pt = renderer?.clientToSvg?.(ev.clientX, ev.clientY) ?? { x: ev.clientX, y: ev.clientY };
    openQuickInsert(ev.clientX, ev.clientY, pt.x, pt.y);
  });
}
//=============================================================================
export default initQuickInsert;
//=============================================================================
