//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import type { Json } from '../rpc';
import { t } from '../i18n';
import { state, hasUpdate, isBinaryArchive, keywords, registryProblem, srcKind, visible } from '../store';
import { fetchVersions, load, setRegistry, showInAll } from '../actions';
import { licenseStatus } from '../lib/info';
import { IconPackage, IconRefresh } from './Icons';
import { DetailMeta, InstallActions, InstalledActions, LinksRow, ProgressCard } from './DetailParts';
import { DocSections, TreeSection, VerifyBox } from './DetailSections';
//=============================================================================
function RegistryPanel({ problem }: { problem: 'missing' | 'unavailable' }) {
  const [url, setUrl] = useState('');
  return (
    <section className="detail detail--center" id="detail" aria-live="polite">
      <IconPackage size={40} className="icon-lg" style={{ strokeWidth: 1.4 }} />
      <h2>{t(problem === 'missing' ? 'noRegistryTitle' : 'unavailableTitle')}</h2>
      <p>{t(problem === 'missing' ? 'noRegistryText' : 'unavailableText')}</p>
      <div className="reg-input">
        <input id="reg-url" type="text" placeholder={t('registryPlaceholder')} spellCheck={false} value={url}
          onChange={e => setUrl(e.target.value)} onKeyDown={e => { if (e.key === 'Enter') setRegistry(url.trim()); }} />
        <button type="button" className="btn btn--primary" id="use-registry" onClick={() => setRegistry(url.trim())}>{t('useRegistry')}</button>
      </div>
      <div className="actions"><button type="button" className="btn" data-a="refresh" onClick={() => load(true)}><IconRefresh />{t('refresh')}</button></div>
      {state.registryError && <p className="mono">{state.registryError}</p>}
    </section>
  );
}
//=============================================================================
function badgesOf(p: Json, busy: boolean, kind: string): JSX.Element[] {
  const update = hasUpdate(p);
  const kindLabel = kind === 'archive' ? t('kindArchive') : kind === 'git' ? t('kindGit') : t('kindLocal');
  const badges: JSX.Element[] = [];
  if (busy) badges.push(<span key="busy" className="badge badge--accent">{state.busy!.label}</span>);
  else if (p.installed) badges.push(<span key="inst" className="badge badge--ok">{t('badgeInstalled') + (p.installed.active ? t('badgeLoaded') : '')}</span>);
  if (!busy && p.installed) {
    badges.push(update ? <span key="upd" className="badge badge--accent">{t('badgeUpdateAvailable')}</span> : <span key="upd" className="badge">{t('badgeUpToDate')}</span>);
  }
  if (p.installed && p.installed.yanked) {
    badges.push(<span key="yank" className="badge badge--danger" title={p.installed.yank_reason ? t('yankedReason', { reason: p.installed.yank_reason }) : t('badgeYanked')}>{t('badgeYanked')}</span>);
  }
  if (kind && !p.installed) badges.push(<span key="kind" className="badge">{t('badgeSource', { kind: kindLabel })}</span>);
  // Registry entries are as trustworthy as the registry: signed with a
  // trusted Ed25519 key and verified by nmm.
  const info = state.registryInfo;
  if (p.latest && info && info.signature_ok) {
    badges.push(<span key="sig" className="badge badge--ok" title={t('signedTitle', { key: String(info.signature_key || '').slice(0, 16) })}>{t('badgeSigned')}</span>);
  }
  return badges;
}
//=============================================================================
export function Detail({ pkg: p }: { pkg: Json | null }) {
  const kind = p ? (isBinaryArchive(p) ? 'archive' : srcKind(p.source)) : '';
  const needsVersions = !!p && !p.installed && kind !== 'archive' && state.versions[p.name] === undefined;
  useEffect(() => { if (needsVersions && p) fetchVersions(p.name); }, [needsVersions, p?.name]);
  if (!state.loaded) {
    return (
      <section className="detail" id="detail" aria-live="polite">
        <div className="skeleton skeleton--title" /><div className="skeleton skeleton--text" /><div className="skeleton skeleton--block" />
      </section>
    );
  }
  if (!p) {
    const problem = registryProblem();
    if (problem) return <RegistryPanel problem={problem} />;
    // An empty list has nothing to select: the list explains why.
    return (
      <section className="detail detail--center" id="detail" aria-live="polite">
        {visible().length > 0 && <><IconPackage size={36} className="icon-lg" style={{ strokeWidth: 1.4 }} /><p>{t('selectPackage')}</p></>}
      </section>
    );
  }
  const busy = !!state.busy && state.busy.name === p.name;
  return (
    <section className={'detail' + (busy ? ' detail--busy' : '')} id="detail" aria-live="polite">
      <div className="head">
        <div className="head__title">
          <span className="head__name">{p.name}</span>
          <span className="head__ver">{p.installed ? p.installed.version : p.version}</span>
          {badgesOf(p, busy, kind)}
        </div>
        {(p.description || p.summary) && <span className="head__summary">{p.description || p.summary}</span>}
      </div>
      {state.loadError && <div className="error">{state.loadError}</div>}
      {licenseStatus(p.license) === 'warn' && <div className="warn-note">{t('licenseWarn', { license: p.license })}</div>}
      {busy ? <ProgressCard p={p} /> : p.installed ? <InstalledActions p={p} kind={kind} /> : <InstallActions p={p} kind={kind} />}
      <DetailMeta p={p} />
      {keywords(p).length > 0 && (
        <div className="tags">
          {keywords(p).map(k => <button key={k} type="button" className="tag" data-tag={k} onClick={() => showInAll('', k)}>{k}</button>)}
        </div>
      )}
      <LinksRow p={p} />
      <VerifyBox p={p} />
      <DocSections p={p} />
      <TreeSection p={p} />
    </section>
  );
}
//=============================================================================
