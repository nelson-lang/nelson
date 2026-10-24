//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
import { t } from '../../utils/i18n.js';
import * as FieldNames from '../../FieldNames.js';
import { getUiPref, setUiPref } from '../ui-prefs.js';
//=============================================================================
export type BlockLibraryViewItem = {
  type: string;
  label: string;
  subsystemKey?: string;
  iconPath?: string;
  iconDir?: string;
};
//=============================================================================
export type BlockLibraryViewGroup = {
  id?: string;
  title?: string;
  blocks: BlockLibraryViewItem[];
  /**
   * Initial open/closed state of the group, decided by renderBlockLibrary from
   * the persisted per-group preference. When omitted, the view reads that
   * preference itself (default closed).
   */
  open?: boolean;
};
//=============================================================================
const roots = new WeakMap<HTMLElement, Root>();
export function normalizeLibrarySearch(value: string): string {
  return String(value)
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .trim();
}
//=============================================================================
export function groupPreferenceKey(group: BlockLibraryViewGroup, index: number): string {
  return `nf-library-group-${group.id ?? group.title ?? index}-open`;
}
//=============================================================================
export function filterBlockLibraryView(container: HTMLElement, query: string): number {
  const normalized = normalizeLibrarySearch(query);
  container.dataset.searchActive = normalized ? '1' : '0';
  let visibleCount = 0;
  container.querySelectorAll<HTMLElement>('.tool').forEach((button) => {
    const matches =
      !normalized || normalizeLibrarySearch(button.dataset.search || '').includes(normalized);
    button.hidden = !matches;
    if (matches) visibleCount += 1;
  });
  container.querySelectorAll<HTMLDetailsElement>('details.tool-group').forEach((group) => {
    const visible = !!group.querySelector('.tool:not([hidden])');
    group.hidden = !visible;
    if (normalized) {
      if (group.dataset.openBeforeSearch === undefined) {
        group.dataset.openBeforeSearch = group.open ? '1' : '0';
      }
      if (visible) group.open = true;
    } else if (group.dataset.openBeforeSearch !== undefined) {
      group.open = group.dataset.openBeforeSearch === '1';
      delete group.dataset.openBeforeSearch;
    }
  });
  const empty = document.getElementById('blockLibraryEmpty');
  if (empty) empty.hidden = visibleCount > 0;
  return visibleCount;
}
//=============================================================================
function BlockLibraryView({ groups }: { groups: BlockLibraryViewGroup[] }): JSX.Element {
  return (
    <>
      {groups.map((group, groupIndex) => (
        <details
          className="tool-group"
          key={group.id ?? group.title ?? groupIndex}
          open={
            group.open ??
            getUiPref(groupPreferenceKey(group, groupIndex), groupIndex === 0 ? '1' : '0') === '1'
          }
          onToggle={(event) => {
            if (event.currentTarget.parentElement?.dataset.searchActive === '1') return;
            setUiPref(groupPreferenceKey(group, groupIndex), event.currentTarget.open ? '1' : '0');
          }}
        >
          <summary>
            <span>{t(group.title || '')}</span>
            <span className="tool-group-count">{group.blocks.length}</span>
          </summary>
          {group.blocks.map((item) => (
            <button
              className={FieldNames.kTool}
              data-type={item.type}
              data-subsystem-key={item.subsystemKey || undefined}
              data-icon-dir={item.iconDir || undefined}
              data-icon-path={item.iconPath || undefined}
              data-search={`${item.type} ${item.label} ${group.title || ''}`}
              draggable
              key={`${item.type}:${item.subsystemKey ?? item.label}`}
            >
              {item.iconPath ? (
                <span className="tool-icon">
                  <svg
                    data-library-icon=""
                    fill="none"
                    stroke="currentColor"
                    strokeWidth="1.5"
                    strokeLinecap="round"
                    strokeLinejoin="round"
                  />
                </span>
              ) : null}
              <span className="tool-label">
                <strong>{t(item.label)}</strong>
                <small>{t(group.title || '')}</small>
              </span>
            </button>
          ))}
        </details>
      ))}
    </>
  );
}
//=============================================================================
export function renderBlockLibraryView(
  container: HTMLElement,
  groups: BlockLibraryViewGroup[]
): void {
  let root = roots.get(container);
  if (!root) {
    if (container.childNodes.length) container.replaceChildren();
    root = createRoot(container);
    roots.set(container, root);
  }
  flushSync(() => {
    root.render(<BlockLibraryView groups={groups} />);
  });
}
//=============================================================================
