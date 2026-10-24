//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { RefObject } from 'react';
import { t } from '../i18n';
import { state, update, packages, hasUpdate } from '../store';
import { installFromSource, load, openDialog } from '../actions';
import { IconArchive, IconDatabase, IconPackage, IconPlus, IconRefresh, IconSearch } from './Icons';
//=============================================================================
export function Toolbar({ searchRef }: { searchRef: RefObject<HTMLInputElement> }) {
  const all = packages();
  const updates = all.filter(hasUpdate).length;
  const installed = all.filter(p => p.installed).length;
  const filters: ['all' | 'installed' | 'updates', string, number][] = [
    ['all', t('filterAll'), 0],
    ['installed', t('filterInstalled'), installed],
    ['updates', t('filterUpdates'), updates],
  ];
  return (
    <header className="toolbar">
      <IconPackage size={18} className="toolbar__icon" />
      <span className="toolbar__title">{t('title')}</span>
      <div className="seg" role="tablist" aria-label="Filter">
        {filters.map(([id, label, count]) => (
          <button key={id} type="button" className={'seg__btn' + (state.filter === id ? ' is-active' : '')} data-filter={id} role="tab"
            onClick={() => { state.filter = id; update(); }}>
            <span>{label}</span>
            {id !== 'all' && <span className="seg__count" id={'count-' + id} hidden={count === 0}>{count}</span>}
          </button>
        ))}
      </div>
      <label className="search">
        <IconSearch />
        <input id="q" ref={searchRef} type="search" placeholder={t('searchPlaceholder')} autoComplete="off" spellCheck={false}
          value={state.query} onChange={e => { state.query = e.target.value; update(); }} />
      </label>
      <button type="button" className="btn btn--ghost" id="add-source" title={t('addSourceTitle')} onClick={() => installFromSource()}>
        <IconPlus /><span>{t('addSource')}</span>
      </button>
      <button type="button" className="btn btn--ghost" id="open-env" title={t('envTitle')} onClick={() => openDialog('env')}>
        <IconArchive /><span>{t('env')}</span>
      </button>
      <button type="button" className="btn btn--ghost" id="open-cache" title={t('cacheTitle')} onClick={() => openDialog('cache')}>
        <IconDatabase /><span>{t('cache')}</span>
      </button>
      <button type="button" className="btn btn--ghost" id="refresh" title={t('refreshTitle')} onClick={() => load(true)}>
        <IconRefresh /><span>{t('refresh')}</span>
      </button>
    </header>
  );
}
//=============================================================================
