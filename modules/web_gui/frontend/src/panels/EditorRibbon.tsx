//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useEffect, useLayoutEffect, useRef, useState, type ReactNode } from 'react';
import { DesktopIcon } from '../components/DesktopIcon';
//=============================================================================
// A grouped editor toolbar that adapts to the panel width (similar to a
// toolstrip), in three tiers:
//   1. expanded  : buttons + caption
//   2. collapsed : "LABEL ▾" dropdown (when the panel is too narrow to expand)
//   3. overflow  : folded into a single trailing "»" menu (when there is not
//                  even room for every dropdown), so no group is ever cut off.
// Lower-priority groups fold first, so FILE and RUN stay reachable longest.
export interface RibbonAction {
  key: string;
  label: ReactNode;
  title?: string;
  onClick: () => void;
  className?: string;
  disabled?: boolean;
}
//=============================================================================
export interface RibbonGroupDef {
  id: string;
  label: string;
  actions: RibbonAction[];
  priority: number; // lower = folds sooner
}
//=============================================================================
const OVERFLOW_W = 40; // width of the trailing "»" button
const DEFAULT_EXPANDED_W = 140;
// Fallback width of a "LABEL ▾" dropdown before it has been measured, estimated
// from the label length (caret + horizontal padding + border).
const estimateCollapsedW = (label: string) => label.length * 8 + 40;
const setsEqual = (a: Set<string>, b: Set<string>) =>
  a.size === b.size && [...a].every((v) => b.has(v));
const titleOf = (a: RibbonAction): string | undefined =>
  a.title ?? (typeof a.label === 'string' ? a.label : undefined);
type Layout = { collapsed: Set<string>; overflow: Set<string> };
export function EditorRibbon({ groups }: { groups: RibbonGroupDef[] }): JSX.Element {
  const containerRef = useRef<HTMLDivElement>(null);
  const groupNodes = useRef<Map<string, HTMLDivElement>>(new Map());
  const widths = useRef<Record<string, number>>({});
  const collapsedWidths = useRef<Record<string, number>>({});
  const [layout, setLayout] = useState<Layout>({ collapsed: new Set(), overflow: new Set() });
  const [openMenu, setOpenMenu] = useState<string | null>(null); // group id or '__overflow__'
  const relayoutRef = useRef<() => void>(() => {});

  // Measure each group's real width in its current tier, then re-run the layout
  // so the next pass uses measured widths instead of estimates (converges via
  // the setsEqual guard in relayout; no re-render once the tiers are stable).
  useLayoutEffect(() => {
    groupNodes.current.forEach((node, id) => {
      if (!node) return;
      const w = node.getBoundingClientRect().width;
      if (node.dataset.state === 'expanded') widths.current[id] = w;
      else if (node.dataset.state === 'collapsed') collapsedWidths.current[id] = w;
    });
    // A narrow panel can cross a tier boundary while the groups are being
    // measured. Defer the next pass so React completes the current commit
    // before a possible layout update; this also lets ResizeObserver settle.
    const frame = window.requestAnimationFrame(() => relayoutRef.current());
    return () => window.cancelAnimationFrame(frame);
  });

  // Decide each group's tier so the ribbon fits the available width.
  const relayout = useCallback(() => {
    const el = containerRef.current;
    if (!el) return;
    const avail = el.clientWidth - 8;
    const order = groups.slice().sort((a, b) => a.priority - b.priority);
    const collapsed = new Set<string>();
    const overflow = new Set<string>();
    const total = () => {
      let w = 0;
      for (const g of groups) {
        if (overflow.has(g.id)) continue;
        w += collapsed.has(g.id)
          ? collapsedWidths.current[g.id] ?? estimateCollapsedW(g.label)
          : widths.current[g.id] ?? DEFAULT_EXPANDED_W;
      }
      if (overflow.size) w += OVERFLOW_W;
      return w;
    };
    // Tier 1→2: collapse lowest-priority groups until they fit as dropdowns.
    for (const g of order) {
      if (total() <= avail) break;
      collapsed.add(g.id);
    }
    // If the dropdowns still don't fit, fold lowest-priority ones
    // into the trailing overflow menu until the bar fits.
    for (const g of order) {
      if (total() <= avail) break;
      overflow.add(g.id);
    }
    setLayout((prev) =>
      setsEqual(prev.collapsed, collapsed) && setsEqual(prev.overflow, overflow)
        ? prev
        : { collapsed, overflow }
    );
  }, [groups]);
  relayoutRef.current = relayout;

  useEffect(() => {
    const el = containerRef.current;
    if (!el) return;
    const ro = new ResizeObserver(() => relayout());
    ro.observe(el);
    relayout();
    return () => ro.disconnect();
  }, [relayout]);

  useEffect(() => {
    if (!openMenu) return;
    const close = () => setOpenMenu(null);
    window.addEventListener('mousedown', close);
    return () => window.removeEventListener('mousedown', close);
  }, [openMenu]);

  const button = (a: RibbonAction) => (
    <button
      key={a.key}
      className={'ribbon-btn' + (a.className ? ' ' + a.className : '')}
      title={titleOf(a)}
      disabled={a.disabled}
      onClick={a.onClick}
    >
      {a.label}
    </button>
  );

  const popupItem = (a: RibbonAction) => (
    <button
      key={a.key}
      className={
        'editor-ribbon__popup-item' +
        (a.className?.includes('--on') ? ' editor-ribbon__popup-item--on' : '')
      }
      title={titleOf(a)}
      disabled={a.disabled}
      onClick={() => {
        setOpenMenu(null);
        a.onClick();
      }}
    >
      {a.label}
    </button>
  );

  const overflowGroups = groups.filter((g) => layout.overflow.has(g.id));

  return (
    <div className="editor-ribbon" ref={containerRef}>
      {groups.map((g, gi) => {
        if (layout.overflow.has(g.id)) return null;
        const isCollapsed = layout.collapsed.has(g.id);
        const state = isCollapsed ? 'collapsed' : 'expanded';
        // Right-half dropdowns open right-aligned so the popup can't spill past
        // the panel's right edge (and get clipped by the editor container).
        const alignRight = gi >= groups.length / 2;
        return (
          <div
            key={g.id}
            className={
              'editor-ribbon__group' + (isCollapsed ? ' editor-ribbon__group--collapsed' : '')
            }
            data-state={state}
            ref={(n) => {
              if (n) groupNodes.current.set(g.id, n);
              else groupNodes.current.delete(g.id);
            }}
          >
            {isCollapsed ? (
              <>
                <button
                  className="editor-ribbon__collapsed-btn"
                  title={g.label}
                  onMouseDown={(e) => {
                    e.stopPropagation();
                    setOpenMenu((m) => (m === g.id ? null : g.id));
                  }}
                >
                  {g.label} <DesktopIcon name="down" size={16} />
                </button>
                {openMenu === g.id ? (
                  <div
                    className={
                      'editor-ribbon__popup' + (alignRight ? ' editor-ribbon__popup--right' : '')
                    }
                    onMouseDown={(e) => e.stopPropagation()}
                  >
                    {g.actions.map(popupItem)}
                  </div>
                ) : null}
              </>
            ) : (
              <div className="editor-ribbon__btns">{g.actions.map(button)}</div>
            )}
            <div className="editor-ribbon__label">{g.label}</div>
          </div>
        );
      })}

      {overflowGroups.length ? (
        <div className="editor-ribbon__group editor-ribbon__group--collapsed editor-ribbon__overflow">
          <button
            className="editor-ribbon__collapsed-btn"
            title="More toolbar groups"
            aria-label="More toolbar groups"
            onMouseDown={(e) => {
              e.stopPropagation();
              setOpenMenu((m) => (m === '__overflow__' ? null : '__overflow__'));
            }}
          >
            <DesktopIcon name="more" size={18} />
          </button>
          {openMenu === '__overflow__' ? (
            <div
              className="editor-ribbon__popup editor-ribbon__popup--right"
              onMouseDown={(e) => e.stopPropagation()}
            >
              {overflowGroups.map((g) => (
                <div key={g.id} className="editor-ribbon__popup-group">
                  <div className="editor-ribbon__popup-heading">{g.label}</div>
                  {g.actions.map(popupItem)}
                </div>
              ))}
            </div>
          ) : null}
          <div className="editor-ribbon__label">MORE</div>
        </div>
      ) : null}
    </div>
  );
}
//=============================================================================
