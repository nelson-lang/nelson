//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { KeyboardEvent } from 'react';
import type { Json } from '../rpc';
import { t, isSingular } from '../i18n';
import { state, update, packages, hasUpdate, registryProblem, distinct } from '../store';
import { act, select, showInAll, updateAll } from '../actions';
import { listOf } from '../lib/info';
import { highlightParts } from '../lib/search';
import { IconCheck, IconNoMatch, IconPackage, IconUpdate } from './Icons';
//=============================================================================
export function Highlight({ text, query }: { text: string; query: string }) {
  return <>{highlightParts(text, query).map((part, k) => (part.mark ? <mark key={k}>{part.text}</mark> : part.text))}</>;
}
//=============================================================================
function Filters({ all }: { all: Json[] }) {
  const categories = distinct(all.flatMap(p => listOf(p.categories)));
  const licenses = distinct(all.map(p => String(p.license || '')));
  if (state.loaded && state.category && !categories.includes(state.category)) state.category = '';
  if (state.loaded && state.license && !licenses.includes(state.license)) state.license = '';
  const hidden = !state.loaded || (!categories.length && licenses.length < 2);
  const choice = (id: 'category' | 'license', allLabel: string, values: string[], label: string) => (
    <label className="filter">
      <span className="sr-only">{label}</span>
      <select id={id} aria-label={label} value={state[id]} onChange={e => { state[id] = e.target.value; update(); }}>
        <option value="">{allLabel}</option>
        {values.map(v => <option key={v} value={v}>{v}</option>)}
      </select>
    </label>
  );
  return (
    <div className="list__filters" id="filters" hidden={hidden}>
      {!hidden && categories.length > 0 && choice('category', t('categoryAll'), categories, t('filterCategory'))}
      {!hidden && licenses.length > 1 && choice('license', t('licenseAll'), licenses, t('filterLicense'))}
    </div>
  );
}
//=============================================================================
function RowStatus({ p }: { p: Json }) {
  if (state.busy && state.busy.name === p.name) return <span className="row__status row__status--update"><span className="spinner" />{state.busy.label}</span>;
  if (hasUpdate(p)) return <span className="row__status row__status--update"><IconUpdate />{t('rowUpdate', { version: p.latest })}</span>;
  if (p.installed) return <span className="row__status row__status--ok"><IconCheck />{t('rowInstalled')}</span>;
  return <span className="row__status">{t('rowNotInstalled')}</span>;
}
//=============================================================================
function EmptyList({ query }: { query: string }) {
  if (query) {
    const what = state.filter === 'all' ? t('whatPackage') : state.filter === 'installed' ? t('whatInstalled') : t('whatUpdate');
    return (
      <div className="list__empty">
        <IconNoMatch />
        <b>{t('emptyNoMatch', { what, query })}</b>
        <p>{state.filter !== 'all' ? t('emptyTryAll') : t('emptyTry')}</p>
        {state.filter !== 'all' && <button type="button" className="btn" data-a="filter-all" onClick={() => showInAll('', query)}>{t('searchAll')}</button>}
      </div>
    );
  }
  const [title, text] = state.filter === 'updates' ? ['emptyUpToDate', 'emptyUpToDateText']
    : state.filter === 'installed' ? ['emptyNoInstalled', 'emptyNoInstalledText'] : ['emptyNoPackages', 'emptyNoPackagesText'];
  return <div className="list__empty"><b>{t(title)}</b><p>{t(text)}</p></div>;
}
//=============================================================================
export function PackageList({ shown }: { shown: Json[] }) {
  const all = packages();
  const q = state.query.trim();
  const updates = all.filter(hasUpdate).length;
  const showUpdateAll = state.filter === 'updates' && updates > 0 && !state.busy;
  const problem = registryProblem();
  const one = isSingular(shown.length);
  const scope = state.filter === 'installed' ? t(one ? 'instOne' : 'instMany') : state.filter === 'updates' ? t('scopeUpdatable') : t(one ? 'pkgOne' : 'pkgMany');
  const summary = !state.loaded ? t('loadingPackages')
    : q ? t('summaryMatch', { shown: shown.length, all: all.length, scope, query: q })
      : t('summaryCount', { count: shown.length, scope }) + (problem ? t(problem === 'missing' ? 'noRegistrySuffix' : 'unavailableSuffix') : '');
  const onKeyDown = (e: KeyboardEvent) => {
    const i = shown.findIndex(p => p.name === state.selected);
    if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
      const next = shown[Math.min(shown.length - 1, Math.max(0, i + (e.key === 'ArrowDown' ? 1 : -1)))];
      if (next) { select(next.name); e.preventDefault(); }
    } else if (e.key === 'Enter' && !state.busy && i >= 0) {
      const p = shown[i];
      if (!p.installed) { act('install', p.name, p.version); e.preventDefault(); }
      else if (hasUpdate(p)) { act('update', p.name, p.latest); e.preventDefault(); }
    }
  };
  return (
    <section className={'list' + (state.busy ? ' is-busy' : '')} aria-label="Packages">
      <div className="list__header">
        <span id="list-summary">{summary}</span>
        <label className="sort" id="col-name" title={t('sortLabel')} hidden={showUpdateAll}>
          <span className="sort__label">{t('sortLabel')}</span>
          <select id="sort" value={state.sort} onChange={e => { state.sort = e.target.value as typeof state.sort; update(); }}>
            <option value="name">{t('sortName')}</option>
            <option value="updates">{t('sortUpdates')}</option>
            <option value="installed">{t('sortInstalled')}</option>
          </select>
        </label>
        <button type="button" className="hdr-action" id="update-all" hidden={!showUpdateAll} onClick={() => updateAll()}>
          {t('updateAll') + ' (' + updates + ')'}
        </button>
      </div>
      <Filters all={all} />
      <div className="list__rows" id="rows" role="listbox" tabIndex={0} onKeyDown={onKeyDown}>
        {!state.loaded ? Array.from({ length: 6 }, (_, k) => <div key={k} className="skeleton-row" />)
          : !shown.length ? <EmptyList query={q} />
            : <>
              {shown.map(p => (
                <div key={p.name} className={'row' + (p.name === state.selected ? ' is-selected' : '')} role="option"
                  aria-selected={p.name === state.selected} data-name={p.name} onClick={() => select(p.name)}>
                  <span className="row__icon"><IconPackage /></span>
                  <div className="row__text">
                    <div className="row__name"><b><Highlight text={p.name} query={q} /></b><span className="mono">{p.installed ? p.installed.version : p.version}</span></div>
                    <span className="row__desc"><Highlight text={p.summary || ''} query={q} /></span>
                  </div>
                  <RowStatus p={p} />
                </div>
              ))}
              {problem && <div className="list__note">{t(problem === 'missing' ? 'registryNote' : 'unavailableNote')}</div>}
            </>}
      </div>
    </section>
  );
}
//=============================================================================
