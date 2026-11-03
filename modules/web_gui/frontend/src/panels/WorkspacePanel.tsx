//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useMemo, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import { webPrompt } from '../components/PromptHost';
import type { WorkspaceListResult, WorkspaceVarSummary } from '../rpc/types';
import { openVariable } from '../desktop/desktop';
import { ContextMenu, useContextMenu, type MenuItem } from '../components/ContextMenu';
import { runCommand as run } from '../desktop/engineSession';
import { getPersistentItem, setPersistentItem } from '../desktop/persistentStorage';
import { t } from '../i18n';
//=============================================================================
// Workspace browser: lists variables in scope. Double-click opens the variable
// editor. Right-click offers open, plot gallery, rename, save as and delete.
// Refreshes on the `workspace.changed` push.
type ColKey = 'value' | 'size' | 'bytes' | 'class';
// Built at render time so the labels follow the loaded catalog.
function allCols(): { key: ColKey; label: string }[] {
  return [
    { key: 'value', label: t('Value') },
    { key: 'size', label: t('Size') },
    { key: 'bytes', label: t('Bytes') },
    { key: 'class', label: t('Class') },
  ];
}
//=============================================================================
export function WorkspacePanel(): JSX.Element {
  const [vars, setVars] = useState<WorkspaceVarSummary[]>([]);
  const [filter, setFilter] = useState('');
  const [cols, setCols] = useState<Set<ColKey>>(() => {
    try {
      const saved = JSON.parse(getPersistentItem('nelson-webview.ws.cols') ?? 'null');
      if (Array.isArray(saved)) return new Set(saved as ColKey[]);
    } catch {
      /* default below */
    }
    return new Set<ColKey>(['value', 'size', 'class']);
  });
  const { menu, openMenu, closeMenu } = useContextMenu();

  useEffect(() => {
    const refresh = () =>
      rpc<WorkspaceListResult>('workspace.list')
        .then((r) => setVars(r.variables))
        .catch(() => undefined);
    refresh();
    return onPush('workspace.changed', refresh);
  }, []);

  useEffect(() => {
    setPersistentItem('nelson-webview.ws.cols', JSON.stringify([...cols]));
  }, [cols]);

  const visible = useMemo(() => {
    const f = filter.trim().toLowerCase();
    return f ? vars.filter((v) => v.name.toLowerCase().includes(f)) : vars;
  }, [vars, filter]);

  const toggleCol = (key: ColKey) =>
    setCols((prev) => {
      const next = new Set(prev);
      if (next.has(key)) {
        next.delete(key);
      } else {
        next.add(key);
      }
      return next;
    });

  const itemsFor = (v: WorkspaceVarSummary): MenuItem[] => {
    const numeric = /^(double|single|u?int\d+|logical)$/.test(v.className);
    const sparse = /sparse/i.test(v.className) || /sparse/i.test(v.value);
    const isVector = numeric && (v.size[0] === 1 || v.size[1] === 1) && v.size[0] * v.size[1] > 1;
    const isMatrix = numeric && v.size[0] > 1 && v.size[1] > 1;
    const items: MenuItem[] = [{ label: t('&Open'), onClick: () => openVariable(v.name) }];
    if (isVector || isMatrix || sparse) {
      items.push({ separator: true });
      if (isVector || isMatrix) {
        items.push({
          label: t('Plots…'),
          onClick: () =>
            window.dispatchEvent(
              new CustomEvent('nelson-webview.open-plots', {
                detail: { name: v.name, rows: v.size[0], cols: v.size[1] },
              })
            ),
        });
        items.push({ label: t('Plot'), onClick: () => run(`plot(${v.name})`) });
        items.push({ label: t('Bar'), onClick: () => run(`bar(${v.name})`) });
        items.push({ label: t('Histogram'), onClick: () => run(`hist(${v.name})`) });
      }
      if (isVector) items.push({ label: t('Pie'), onClick: () => run(`pie(${v.name})`) });
      if (isMatrix) {
        items.push({ label: t('Surf'), onClick: () => run(`surf(${v.name})`) });
        items.push({ label: t('Mesh'), onClick: () => run(`mesh(${v.name})`) });
        items.push({ label: t('Image'), onClick: () => run(`image(${v.name})`) });
      }
      if (sparse) items.push({ label: t('Spy'), onClick: () => run(`spy(${v.name})`) });
    }
    items.push({ separator: true });
    items.push({
      label: t('Rename…'),
      onClick: async () => {
        const nn = await webPrompt(t('New name:'), v.name);
        if (nn && nn !== v.name) run(`${nn} = ${v.name}; clear('${v.name}')`);
      },
    });
    items.push({
      label: t('Save As…'),
      onClick: async () => {
        const f = await webPrompt(t('Save variable to file (.nh5):'), `${v.name}.nh5`);
        if (f) run(`save('${f}','${v.name}')`);
      },
    });
    items.push({
      label: t('Copy name'),
      onClick: () => void navigator.clipboard?.writeText(v.name).catch(() => undefined),
    });
    items.push({ label: t('Delete'), danger: true, onClick: () => run(`clear('${v.name}')`) });
    return items;
  };

  const headerMenu = (e: React.MouseEvent) =>
    openMenu(
      e,
      allCols().map((c) => ({
        label: c.label,
        checked: cols.has(c.key),
        onClick: () => toggleCol(c.key),
      }))
    );

  const cell = (v: WorkspaceVarSummary, key: ColKey) => {
    switch (key) {
      case 'value':
        return (
          <td key={key} className="browser__mono">
            {v.value}
          </td>
        );
      case 'size':
        return (
          <td key={key}>
            {v.size[0]}×{v.size[1]}
          </td>
        );
      case 'bytes':
        return <td key={key}>{v.bytes}</td>;
      case 'class':
        return <td key={key}>{v.className}</td>;
    }
  };

  const shownCols = allCols().filter((c) => cols.has(c.key));

  return (
    <div className="browser">
      <input
        className="ws-filter"
        placeholder={t('Filter variables by name')}
        value={filter}
        onChange={(e) => setFilter(e.target.value)}
        spellCheck={false}
      />
      <table className="browser__table">
        <thead>
          <tr onContextMenu={headerMenu} title={t('Right-click to show/hide columns')}>
            <th>{t('Name')}</th>
            {shownCols.map((c) => (
              <th key={c.key}>{c.label}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {visible.map((v) => (
            <tr
              key={v.name}
              onDoubleClick={() => openVariable(v.name)}
              onContextMenu={(e) => openMenu(e, itemsFor(v))}
              title={t('Double-click to edit')}
            >
              <td className="browser__name">{v.name}</td>
              {shownCols.map((c) => cell(v, c.key))}
            </tr>
          ))}
          {visible.length === 0 ? (
            <tr>
              <td colSpan={shownCols.length + 1} className="browser__empty">
                {t('no variables')}
              </td>
            </tr>
          ) : null}
        </tbody>
      </table>
      <ContextMenu menu={menu} onClose={closeMenu} />
    </div>
  );
}
//=============================================================================
